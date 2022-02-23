#include "formula.h"
#include "parser.h"
#include "lexer.h"

#include <cstdio>
#include <cstdlib>
#include <random>

fo *getAST(const char *formula)
{
    fo *fmla;
    yyscan_t scanner;
    YY_BUFFER_STATE state;

    if (yylex_init(&scanner)) return NULL;

    state = yy_scan_string(formula, scanner);

    if (yyparse(&fmla, scanner)) return NULL;

    yy_delete_buffer(state, scanner);
    yylex_destroy(scanner);

    return fmla;
}

std::mt19937 gen;

void usage() {
    fprintf(stderr, "dg PREFIX MINTL MODE SEED\n\
  PREFIX: prefix of query and database files\n\
  MINTL:  minimum number of tuples in a training table\n\
  MODE:   Data Golf mode\n\
  SEED:   seed for pseudo-random generator\n");
    exit(EXIT_SUCCESS);
}

int main(int argc, char **argv) {
    int mintl, mode, seed;

    if (argc == 5) {
        mintl = atoi(argv[2]);
        mode = atoi(argv[3]);
        seed = atoi(argv[4]);
    } else {
        usage();
    }

    FILE *f = open_file_type(argv[1], ".fo", "r");
    if (f == NULL) {
        fprintf(stderr, "Error: FO query open\n");
        exit(EXIT_FAILURE);
    }
    char *line = NULL;
    size_t length = 0;
    if (getline(&line, &length, f) == -1) {
        fprintf(stderr, "Error: FO query read\n");
        exit(EXIT_FAILURE);
    }
    fclose(f);
    fo *fmla;
    try {
        fmla = getAST(line);
    } catch(const std::runtime_error &e) {
        fprintf(stderr, "Error: %s\n", e.what());
        exit(EXIT_FAILURE);
    }
    free(line);

    int nav = fmla->nav();
    auto v = fmla->dgeqs(mode);
    int next = 0;

    vector<pair<int, vector<int> > > db;
    vector<vector<int> > pos = gen_rand(mintl, nav, v.first, &next);
    vector<vector<int> > neg = gen_rand(mintl, nav, v.second, &next);
    fmla->dg(nav, pos, neg, db, &next, mode);
    gen.seed(seed);
    for (int i = db.size() - 1; i > 0; i--) {
      swap(db[i], db[gen() % (i + 1)]);
    }

    FILE *fdb = open_file_type(argv[1], ".db", "w");
    print_db(fdb, db, fmla->sig);
    fclose(fdb);

    FILE *flog = open_file_type(argv[1], ".log", "w");
    fprintf(flog, "@0 ");
    print_db(flog, db, fmla->sig);
    fclose(flog);

    FILE *fpos = open_file_type(argv[1], ".pos", "w");
    dump_table(fpos, fmla, pos);
    fclose(fpos);

    FILE *fneg = open_file_type(argv[1], ".neg", "w");
    dump_table(fneg, fmla, neg);
    fclose(fneg);

    return 0;
}
