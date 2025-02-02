#include "formula.h"

#include <cassert>
#include <cstdio>
#include <cstring>
#include <map>
#include <random>
using namespace std;

mt19937 genf, gend;

map<int, int> pred_id;

fo *gen_fmla(int n, int neg, int maxn, int nfv, int gfv, int root = 0) {
    assert(n > 0);
    while (true) {
    int op = genf() % 6;
    switch (op) {
        case 0:
    {
        if (n != 1) break;
        int m = 1 + genf() % maxn;
        if (m > nfv) m = nfv;
        int id = pred_id[m]++;
        vector<int> vars(nfv);
        for (int i = 0; i < nfv; i++) vars[i] = i;
        for (int i = nfv - 1; i > 0; i--) {
          swap(vars[i], vars[genf() % (i + 1)]);
        }
        vector<pair<bool, int> > ts(m);
        if (m > 0) {
          ts[0].first = 1;
          ts[0].second = genf() % gfv;
        }
        int idx = 0;
        for (int i = 1; i < m; i++) {
          ts[i].first = 1;
          if (vars[idx] == ts[0].second) idx++;
          ts[i].second = vars[idx++];
        }
        return new fo_pred(id, ts);
    }

        case 1:
    {
        if (n != 1 || nfv < 2) break;
        int x = genf() % (nfv - 1);
        int y = x + 1 + genf() % (nfv - 1 - x);
        pair<bool, int> t;
        t.first = 1;
        t.second = y;
        return new fo_eq(x, t);
    }

        case 2:
    {
        if (n < 2 || neg) break;
        return new fo_neg(gen_fmla(n - 1, 1, maxn, nfv, gfv));
    }

        case 3:
    {
        if (n < 3) break;
        int sz = 1 + genf() % (n - 2);
        fo *left = gen_fmla(sz, 0, maxn, nfv, gfv);
        fo *right = gen_fmla(n - 1 - sz, 0, maxn, nfv, gfv);
        return new fo_conj(left, right);
    }

        case 4:
    {
        if (n < 3 || !root) break;
        int sz = 1 + genf() % (n - 2);
        fo *left = gen_fmla(sz, 0, maxn, nfv, gfv, 1);
        fo *right = gen_fmla(n - 1 - sz, 0, maxn, nfv, gfv, 1);
        return new fo_disj(left, right);
    }

        case 5:
    {
        if (n < 2) break;
        return new fo_ex(nfv, gen_fmla(n - 1, 0, maxn, nfv + 1, gfv));
    }

        default:
    {
        assert(0);
    }
    }
    }
}

int check_dg(fo *fo, int nfv, int fvgen, int maxn, int b[]) {
  return
    /* Data Golf assumptions */
    ((b[0] && !fvgen) || fo->con_ex()) &&
    (b[1] || fo->no_closed()) &&
    (b[2] || fo->check_eqs()) &&
    /* Size requirements */
    (b[3] || fo->fv.size() == nfv) &&
    (b[4] || fo->arity() <= maxn) &&
    /* Normalization */
    (b[5] || fo->srnf(ROOT)) &&
    (b[6] || !sr(fo)) &&
    /* Evaluable? */
    (fvgen == is_subset(fo->fv, fo->gen));
}

fo *random(int n, int maxn, int nfv, int fvgen, int mode, int seed) {
  fo *fmla = NULL;
  int c = 0;
  int b[7];
  for (int i = 0; i < 7; i++) b[i] = (test ? genf() % 2 : 0);
  do {
    if (fmla != NULL) delete fmla;
    pred_id.clear();
    if (n == 0) {
      fo *fo_a = gen_fmla(1, 0, maxn, nfv, nfv, 0);
      fo *fo_b = gen_fmla(1, 0, maxn, nfv + 2, nfv, 0);
      fo *fo_c = gen_fmla(1, 0, maxn, nfv + 2, nfv, 0);
      fmla = new fo_conj(fo_a, new fo_neg(new fo_ex(nfv, new fo_ex(nfv + 1, new fo_conj(fo_b, new fo_neg(fo_c))))));
    } else {
      fmla = gen_fmla(n, 0, maxn, nfv, nfv, 1);
    }
  } while (!check_dg(fmla, nfv, fvgen, maxn, b));
  return fmla;
}

void usage() {
    fprintf(stderr, "\
Usage:\n\
gen_test PREFIX SZ MAXN NFV FVGEN MINL MINTL MODE SEED\n\
  PREFIX: prefix of query and database files\n\
  SZ:     number of query constructors\n\
  MAXN:   maximum arity of a subquery\n\
  NFV:    precise arity of the query\n\
  FVGEN:  free variables must be generated\n\
  MINL:   minimum number of tuples in a table\n\
  MINTL:  minimum number of tuples in a training table\n\
  MODE:   Data Golf mode\n\
  SEED:   seed for pseudo-random generator\n");
    exit(EXIT_FAILURE);
}

int main(int argc, char **argv) {
    int n, maxn, nfv, fvgen, minl, mintl, mode, seed;

    if (argc == 10) {
        n = atoi(argv[2]);
        maxn = atoi(argv[3]);
        nfv = atoi(argv[4]);
        fvgen = atoi(argv[5]);
        minl = atoi(argv[6]);
        mintl = atoi(argv[7]);
        mode = atoi(argv[8]);
        seed = atoi(argv[9]);
        genf.seed(seed);
    } else {
        usage();
    }

    fo *fmla = random(n, maxn, nfv, fvgen, mode, seed);

    int nav = fmla->nav();
    auto v = fmla->dgeqs(mode);
    int next = 0;

    vector<pair<int, vector<int> > > db;
    vector<vector<int> > pos = gen_rand(minl, nav, v.first, &next);
    vector<vector<int> > neg = gen_rand(minl, nav, v.second, &next);
    fmla->dg(nav, pos, neg, db, &next, mode);
    gend.seed(seed);
    for (int i = db.size() - 1; i > 0; i--) {
      swap(db[i], db[gend() % (i + 1)]);
    }

    next = 0;

    vector<pair<int, vector<int> > > tdb;
    vector<vector<int> > tpos = gen_rand(mintl, nav, v.first, &next);
    vector<vector<int> > tneg = gen_rand(mintl, nav, v.second, &next);
    fmla->dg(nav, tpos, tneg, tdb, &next, mode);
    gend.seed(seed);
    for (int i = tdb.size() - 1; i > 0; i--) {
      swap(tdb[i], tdb[gend() % (i + 1)]);
    }

    if (minl <= 10) sql_insert = 1;
    dump(argv[1], fmla, pos, neg, db, tdb);
    delete fmla;

    return 0;
}
