#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef enum {
    OP_INC, OP_DEC, OP_LEFT, OP_RIGHT,
    OP_LOOP_START, OP_LOOP_END,
    OP_CALL
} OpType;


typedef struct Op {
    OpType type;
    int arg; // repeat count or function id
    struct Op *next;
    struct Op *match; // for loops
} Op;

typedef struct {
    char name[32];
    char params[64];
    char *body; // stored function body as string
} Function;

// Execute a code string on the provided tape and pointer. Maintains a function
// table so that definitions (%...) and calls (?...) work across nested execs.
static void resolve_import_path(const char *base_dir, const char *filename,
                               char *out, size_t out_size) {
    if (!base_dir || base_dir[0] == '\0') {
        snprintf(out, out_size, "%s", filename);
        return;
    }
    if (filename[0] == '/') {
        snprintf(out, out_size, "%s", filename);
        return;
    }

    size_t base_len = strlen(base_dir);
    size_t name_len = strlen(filename);
    if (base_len + 1 + name_len + 1 > out_size) {
        snprintf(out, out_size, "%s/%s", base_dir, filename);
        return;
    }

    memcpy(out, base_dir, base_len);
    out[base_len] = '/';
    memcpy(out + base_len + 1, filename, name_len + 1);
}

static void path_dir(const char *path, char *out, size_t out_size) {
    const char *slash = strrchr(path, '/');
    if (!slash) {
        out[0] = '\0';
        return;
    }
    size_t len = (size_t)(slash - path);
    if (len >= out_size) len = out_size - 1;
    memcpy(out, path, len);
    out[len] = '\0';
}

static void exec(const char *code, char **output_ptr, char *tape, char **ptr,
                 Function *functions, int *func_count, const char *base_dir) {
    const char *pc = code;
    int loop;

    while (*pc) {
        switch (*pc) {
            case '>':
                (*ptr)++;
                break;
            case '<':
                (*ptr)--;
                break;
            case '+':
                (**ptr)++;
                break;
            case '-':
                (**ptr)--;
                break;
            case '.':
                // output current cell as character immediately
                putchar((int)**ptr);
                fflush(stdout);
                break;
            case ',':
                // input character into current cell
                **ptr = getchar();
                break;
            case '[':
                if (**ptr == 0) {
                    loop = 1;
                    while (loop > 0 && *pc) {
                        pc++;
                        if (*pc == '[') loop++;
                        if (*pc == ']') loop--;
                    }
                }
                break;
            case ']':
                if (**ptr != 0) {
                    loop = 1;
                    while (loop > 0 && pc > code) {
                        pc--;
                        if (*pc == '[') loop--;
                        if (*pc == ']') loop++;
                    }
                }
                break;
            case '#': // comment until end of line
                while (*pc && *pc != '\n') pc++;
                break;
            case '@': // reset current cell
                **ptr = 0;
                break;
            case '$': // output current cell as number immediately
                printf("%d", (unsigned char)(**ptr));
                fflush(stdout);
                break;

            case '%': { // function definition: %name{params}(body)
                pc++; // skip '%'
                char namebuf[32] = {0};
                int ni = 0;
                while (*pc && *pc != '{' && *pc != '(' && ni < (int)sizeof(namebuf)-1) {
                    namebuf[ni++] = *pc++;
                }
                namebuf[ni] = '\0';

                char paramsbuf[64] = {0};
                if (*pc == '{') {
                    pc++; // skip '{'
                    int pi = 0;
                    while (*pc && *pc != '}' && pi < (int)sizeof(paramsbuf)-1) {
                        paramsbuf[pi++] = *pc++;
                    }
                    paramsbuf[pi] = '\0';
                    if (*pc == '}') pc++;
                }

                // parse body inside parentheses, allowing nested parentheses
                char *bodybuf = NULL;
                if (*pc == '(') {
                    pc++; // skip '('
                    const char *start = pc;
                    int depth = 1;
                    while (*pc && depth > 0) {
                        if (*pc == '(') depth++;
                        else if (*pc == ')') depth--;
                        if (depth > 0) pc++;
                    }
                    size_t len = pc - start;
                    bodybuf = (char*)malloc(len + 1);
                    if (bodybuf) {
                        memcpy(bodybuf, start, len);
                        bodybuf[len] = '\0';
                    }
                    if (*pc == ')') pc++; // skip closing ')'
                }

                // store function
                if (*func_count < 64) {
                    Function *f = &functions[(*func_count)++];
                    strncpy(f->name, namebuf, sizeof(f->name)-1);
                    strncpy(f->params, paramsbuf, sizeof(f->params)-1);
                    f->body = bodybuf;
                } else {
                    free(bodybuf);
                }
                // we've already advanced pc to the next char after definition
                continue;
            }

            case '?': { // function call: ?name{args}
                pc++; // skip '?'
                char namebuf[32] = {0};
                int ni = 0;
                while (*pc && *pc != '{' && ni < (int)sizeof(namebuf)-1) {
                    namebuf[ni++] = *pc++;
                }
                namebuf[ni] = '\0';

                char argsbuf[128] = {0};
                if (*pc == '{') {
                    pc++;
                    int ai = 0;
                    while (*pc && *pc != '}' && ai < (int)sizeof(argsbuf)-1) {
                        argsbuf[ai++] = *pc++;
                    }
                    argsbuf[ai] = '\0';
                    if (*pc == '}') pc++;
                }

                // find function
                int found = -1;
                for (int i = 0; i < *func_count; ++i) {
                    if (strcmp(functions[i].name, namebuf) == 0) { found = i; break; }
                }
                if (found >= 0 && functions[found].body) {
                    const char *fbody = functions[found].body;

                    // parse parameter names from stored function definition
                    char param_names[16][64];
                    int param_count = 0;
                    const char *p = functions[found].params;
                    while (p && *p) {
                        // skip whitespace
                        while (*p && isspace((unsigned char)*p)) p++;
                        if (!*p) break;
                        // read token until ','
                        int pn = 0;
                        while (*p && *p != ',') {
                            if (!isspace((unsigned char)*p) && pn < 63) param_names[param_count][pn++] = *p;
                            p++;
                        }
                        param_names[param_count][pn] = '\0';
                        param_count++;
                        if (*p == ',') p++;
                        if (param_count >= 16) break;
                    }

                    // parse args from call
                    char arg_vals[16][256];
                    int arg_count = 0;
                    const char *a = argsbuf;
                    while (a && *a) {
                        // skip whitespace
                        while (*a && isspace((unsigned char)*a)) a++;
                        if (!*a) break;
                        int ai = 0;
                        // read until comma
                        while (*a && *a != ',') {
                            if (ai < 255) arg_vals[arg_count][ai++] = *a;
                            a++;
                        }
                        arg_vals[arg_count][ai] = '\0';
                        arg_count++;
                        if (*a == ',') a++;
                        if (arg_count >= 16) break;
                    }

                    // perform textual substitution: replace param names in fbody with corresponding arg_vals
                    // compute new length
                    size_t newlen = 0;
                    for (const char *q = fbody; *q; ) {
                        int matched = 0;
                        for (int pi = 0; pi < param_count; ++pi) {
                            size_t plen = strlen(param_names[pi]);
                            if (plen > 0 && strncmp(q, param_names[pi], plen) == 0) {
                                // add arg length if available
                                if (pi < arg_count) newlen += strlen(arg_vals[pi]);
                                matched = 1;
                                q += plen;
                                break;
                            }
                        }
                        if (!matched) { newlen++; q++; }
                    }

                    char *expanded = (char*)malloc(newlen + 1);
                    if (!expanded) {
                        continue;
                    }
                    char *outp = expanded;
                    for (const char *q = fbody; *q; ) {
                        int matched = 0;
                        for (int pi = 0; pi < param_count; ++pi) {
                            size_t plen = strlen(param_names[pi]);
                            if (plen > 0 && strncmp(q, param_names[pi], plen) == 0) {
                                if (pi < arg_count) {
                                    size_t al = strlen(arg_vals[pi]);
                                    memcpy(outp, arg_vals[pi], al);
                                    outp += al;
                                }
                                matched = 1;
                                q += plen;
                                break;
                            }
                        }
                        if (!matched) { *outp++ = *q++; }
                    }
                    *outp = '\0';

                    // execute expanded body
                    exec(expanded, output_ptr, tape, ptr, functions, func_count, base_dir);
                    free(expanded);
                }
                continue;
            }

            case '=':
                // set current cell to the next character's ASCII value
                pc++;
                if (*pc) {
                    **ptr = (unsigned char)(*pc);
                }
                break;

            case '!':
                // import external file and execute its contents
                pc++;
                {
                    char filename[256] = {0};
                    int fi = 0;
                    // read filename until any whitespace
                    while (*pc && !isspace((unsigned char)*pc) && fi < (int)sizeof(filename)-1) {
                        filename[fi++] = *pc++;
                    }
                    filename[fi] = '\0';

                    char resolved[512] = {0};
                    resolve_import_path(base_dir, filename, resolved, sizeof(resolved));

                    FILE *f = NULL;
                    const char *candidates[5] = { resolved, filename, NULL, NULL, NULL };
                    char with_bfppp[512] = {0};
                    char with_bfpp[512] = {0};
                    size_t name_len = strlen(filename);
                    if (strstr(filename, ".") == NULL) {
                        if (name_len + 7 < sizeof(with_bfppp)) {
                            memcpy(with_bfppp, resolved, strlen(resolved));
                            memcpy(with_bfppp + strlen(resolved), ".bfppp", 7);
                            candidates[2] = with_bfppp;
                        }
                        if (name_len + 6 < sizeof(with_bfpp)) {
                            memcpy(with_bfpp, resolved, strlen(resolved));
                            memcpy(with_bfpp + strlen(resolved), ".bfpp", 6);
                            candidates[3] = with_bfpp;
                        }
                    }

                    const char *opened = NULL;
                    for (int i = 0; i < 5; ++i) {
                        if (!candidates[i]) continue;
                        f = fopen(candidates[i], "r");
                        if (f) {
                            opened = candidates[i];
                            break;
                        }
                    }

                    if (f) {
                        char import_base_dir[512] = {0};
                        path_dir(opened, import_base_dir, sizeof(import_base_dir));

                        fseek(f, 0, SEEK_END);
                        long fsize = ftell(f);
                        fseek(f, 0, SEEK_SET);
                        char *filecode = NULL;
                        if (fsize > 0) {
                            filecode = (char*)malloc(fsize + 1);
                            if (filecode) {
                                size_t r = fread(filecode, 1, fsize, f);
                                filecode[r] = '\0';
                                exec(filecode, output_ptr, tape, ptr, functions, func_count, import_base_dir);
                                free(filecode);
                            }
                        } else {
                            // empty file: nothing to do
                        }
                        fclose(f);
                    }
                    continue;
                }
            default:
                break;
        }
        pc++;
    }
}

static void bfppp_impl(const char *input, char *output, const char *base_dir) {
    // top-level interpreter setup
    char tape[30000] = {0};
    char *ptr = tape;
    Function functions[64] = {0};
    int func_count = 0;
    char *out_ptr = output;

    exec(input, &out_ptr, tape, &ptr, functions, &func_count, base_dir);
    *out_ptr = '\0';
}

void bfppp(const char *input, char *output) {
    bfppp_impl(input, output, NULL);
}

int main(int argc, char **argv) {
    // Accept file path as first argument; default to "program.bfppp"
    const char *inpath = "program.bfppp";
    if (argc > 1 && argv[1] && argv[1][0] != '\0') inpath = argv[1];
    FILE *file = fopen(inpath, "r");
    char output[4096];
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }
    fseek(file, 0, SEEK_END);
    long fsize = ftell(file);
    fseek(file, 0, SEEK_SET);
    char *code = NULL;
    if (fsize > 0) {
        code = (char*)malloc(fsize + 1);
        if (code) {
            size_t r = fread(code, 1, fsize, file);
            code[r] = '\0';
        }
    } else {
        code = (char*)malloc(1);
        if (code) code[0] = '\0';
    }
    fclose(file);

    if (!code) {
        printf("Memory allocation failed\n");
        return 1;
    }

    char base_dir[512] = {0};
    const char *slash = strrchr(inpath, '/');
    if (slash) {
        size_t len = (size_t)(slash - inpath);
        if (len >= sizeof(base_dir)) len = sizeof(base_dir) - 1;
        memcpy(base_dir, inpath, len);
        base_dir[len] = '\0';
    }

    bfppp_impl(code, output, base_dir);
    printf("%s\n", output);
    free(code);
    return 0;
}