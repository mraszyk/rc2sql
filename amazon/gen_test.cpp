#include "formula.h"

#include <cassert>
#include <cstdio>
#include <cstring>
#include <map>
#include <random>
using namespace std;

mt19937 gend;

void usage() {
  fprintf(stderr, "\
Usage:\n\
gen_test PREFIX NATTR BRAND MINL MINTL MODE SEED\n\
PREFIX: prefix of query and database files\n\
NATTR:  number of distinct attributes per review\n\
USR:    compute all users of a suspicious brand\n\
MINL:   minimum number of tuples in a table\n\
MINTL:  minimum number of tuples in a training table\n\
MODE:   Data Golf mode\n\
SEED:   seed for pseudo-random generator\n");
  exit(EXIT_FAILURE);
}

int main(int argc, char **argv) {
  int nattr, brand, minl, mintl, mode, seed;

  if (argc == 8) {
    nattr = atoi(argv[2]);
    brand = atoi(argv[3]);
    minl = atoi(argv[4]);
    mintl = atoi(argv[5]);
    mode = atoi(argv[6]);
    seed = atoi(argv[7]);
  } else {
    usage();
  }

  // 0     => b
  // 1     => p
  // 2     => r
  // 3 + i => ai
  vector<pair<bool, int> > ts_b;
  ts_b.push_back(make_pair(true, 0));
  vector<pair<bool, int> > ts_p;
  ts_p.push_back(make_pair(true, 0));
  ts_p.push_back(make_pair(true, 1));
  vector<vector<pair<bool, int> > > ts_ri;
  for (int i = 0; i < nattr; i++) {
    vector<pair<bool, int> > ts_r;
    ts_r.push_back(make_pair(true, 2));
    ts_r.push_back(make_pair(true, 3 + i));
    ts_r.push_back(make_pair(true, 1));
    ts_ri.push_back(ts_r);
  }

  // P(b, p) AND NOT Ri(r, ai, p)
  fo *attr = new fo_pred(0, ts_p);
  for (int i = 0; i < nattr; i++) {
    attr = new fo_conj(attr, new fo_neg(new fo_pred(i, ts_ri[i])));
  }
  // NOT EXISTS p. P(b, p) AND NOT Ri(r, ai, p)
  fo *all = new fo_neg(new fo_ex(1, attr));
  // EXISTS ai. NOT EXISTS p. P(b, p) AND NOT Ri(r, ai, p)
  fo *ex = all;
  for (int i = nattr - 1; i >= 0; i--) {
    ex = new fo_ex(3 + i, ex);
  }
  // [B(b) AND EXISTS r.] EXISTS ai. NOT EXISTS p. P(b, p) AND NOT Ri(r, ai, p)
  fo *fmla = new fo_conj(new fo_pred(0, ts_b), brand ? ex : new fo_ex(2, ex));


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
