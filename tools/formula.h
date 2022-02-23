#ifndef __FORMULA_H__
#define __FORMULA_H__

#include <cassert>
#include <map>
#include <random>
#include <set>
#include <sstream>
#include <string>
#include <vector>
using namespace std;

const int NEG = 0;
const int EX = 1;
const int BIN = 2;
const int ROOT = 3;

extern int sql_insert;

/* Data Golf assumptions can be randomly turned off if this flag is set */
extern const int test;

template<typename T>
set<T> set_union(set<T> l, set<T> r) {
  set<T> ans(l);
  ans.insert(r.begin(), r.end());
  return ans;
}

template<typename T>
multiset<T> mset_union(multiset<T> l, multiset<T> r) {
  multiset<T> ans(l);
  ans.insert(r.begin(), r.end());
  return ans;
}

template<typename T>
set<T> set_inter(set<T> l, set<T> r) {
  set<T> ans;
  for (auto it : l) {
    if (r.find(it) != r.end()) ans.insert(it);
  }
  return ans;
}

template<typename T>
set<T> set_diff(set<T> l, set<T> r) {
  set<T> ans;
  for (auto it : l) {
    if (r.find(it) == r.end()) ans.insert(it);
  }
  return ans;
}

template<typename T>
set<T> set_rem(set<T> l, T x) {
  set<T> ans = l;
  ans.erase(x);
  return ans;
}

template<typename T>
bool in_set(set<T> l, T x) {
  return l.find(x) != l.end();
}

template<typename T>
bool is_subset(set<T> l, set<T> r) {
  for (auto it : l) {
    if (r.find(it) == r.end()) return false;
  }
  return true;
}

static void app_rand(vector<vector<int> > &data, int copy, int *next) {
  for (int i = 0; i < data.size(); i++) {
    if (copy == -1) {
      data[i].push_back(*next);
      (*next) += 2;
    } else {
      assert(0 <= copy && copy < data[i].size());
      data[i].push_back(data[i][copy]);
    }
  }
}

static vector<vector<int> > gen_rand(int n, int nv, set<int> eq, int *next) {
  vector<vector<int> > data(n);
  int copy = -1;
  for (int i = 0; i < nv; i++) {
    app_rand(data, (in_set(eq, i) ? copy : -1), next);
    if (in_set(eq, i)) copy = i;
  }
  return data;
}

struct fo {
    set<int> fv;
    set<int> gen, con;
    set<int> _gen, _con;
    set<pair<int, int> > sig;
    virtual ~fo() {}
    virtual bool gen_ex() const = 0;
    virtual bool con_ex() const = 0;
    virtual bool srnf(int par) = 0;
    virtual bool ranf(set<int> gv) = 0;
    virtual bool no_closed() const = 0;
    virtual int nav() const = 0;
    virtual int arity() const = 0;
    virtual multiset<int> col_eqs() const = 0;
    bool check_eqs() const {
      auto eqs = col_eqs();
      for (auto it : eqs) {
        if (eqs.count(it) > 1) return 0;
      }
      return 1;
    }
    virtual pair<set<int>, set<int> > dgeqs(int mode) const = 0;
    virtual void dgeq(int nv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, int mode) const = 0;
    virtual void dgneq(int nv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, int mode) const = 0;
    void dg(int nv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, int mode) {
      if (mode == 0) {
        return dgeq(nv, pos, neg, db, next, mode);
      } else {
        return dgneq(nv, pos, neg, db, next, mode);
      }
    }
    virtual void print(std::ostringstream &fmla) const = 0;
};

bool sr(const fo *fo);

struct fo_pred : public fo {
    int id;
    vector<pair<bool, int> > ts;
    fo_pred(int id, vector<pair<bool, int> > ts) : id(id), ts(ts) {
      for (int i = 0; i < ts.size(); i++) {
          if (ts[i].first) {
            fv.insert(ts[i].second);
            gen.insert(ts[i].second);
            con.insert(ts[i].second);
          }
      }
      sig.insert(make_pair(id, ts.size()));
    }
    bool gen_ex() const {
      return true;
    }
    bool con_ex() const {
      return true;
    }
    bool srnf(int par) override {
      return true;
    }
    bool ranf(set<int> gv) override {
      return true;
    }
    bool no_closed() const {
      return !fv.empty();
    }
    int nav() const {
      int m = -1;
      for (auto it : fv) m = max(m, it);
      return m + 1;
    }
    int arity() const {
      return fv.size();
    }
    multiset<int> col_eqs() const {
      return multiset<int>();
    }
    pair<set<int>, set<int> > dgeqs(int mode) const {
      return make_pair(set<int>(), set<int>());
    }
    vector<int> subst(int nv, vector<int> it, vector<pair<bool, int> > ts) const {
      assert(it.size() == nv);
      vector<int> cur(ts.size());
      for (int i = 0; i < ts.size(); i++) {
        if (ts[i].first) {
          cur[i] = it[ts[i].second];
        } else {
          cur[i] = ts[i].second;
        }
      }
      return cur;
    }
    void dgaux(int nv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next) const {
      set<vector<int> > table;
      for (auto it : pos) {
        vector<int> cur = subst(nv, it, ts);
        table.insert(cur);
        db.push_back(make_pair(id, cur));
      }
      for (auto it : neg) {
        vector<int> cur = subst(nv, it, ts);
        if (!test) assert(table.find(cur) == table.end());
      }
    }
    void dgeq(int nv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, int mode) const {
      dgaux(nv, pos, neg, db, next);
    }
    void dgneq(int nv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, int mode) const {
      dgaux(nv, pos, neg, db, next);
    }
    void print(std::ostringstream &fmla) const override {
      fmla << "P" << id << "A" << ts.size() << "(";
      const char *sep = "";
      for (int i = 0; i < ts.size(); i++) {
        fmla << sep;
        sep = ", ";
        fmla << (ts[i].first ? "x" : "") << ts[i].second;
      }
      fmla << ")";
    }
};

struct fo_eq : public fo {
    int x;
    pair<bool, int> t;
    fo_eq(int x, pair<bool, int> t) : x(x), t(t) {
      fv.insert(x);
      if (t.first) {
        fv.insert(t.second);
      } else {
        gen.insert(x);
        con.insert(x);
      }
    }
    bool gen_ex() const {
      return true;
    }
    bool con_ex() const {
      return true;
    }
    bool srnf(int par) override {
      return true;
    }
    bool ranf(set<int> gv) override {
      return !t.first || in_set(gv, x) || in_set(gv, t.second);
    }
    bool no_closed() const {
      return true;
    }
    int nav() const {
      int m = x;
      if (t.first) m = max(m, t.second);
      return m + 1;
    }
    int arity() const {
      return fv.size();
    }
    multiset<int> col_eqs() const {
      multiset<int> res;
      res.insert(x);
      if (t.first) res.insert(t.second);
      return res;
    }
    pair<set<int>, set<int> > dgeqs(int mode) const {
      return make_pair(fv, set<int>());
    }
    void dgaux(int nv, vector<vector<int> > &pos, vector<vector<int> > &neg) const {
      assert(t.first);
      for (auto it : pos) {
        assert(it.size() == nv);
        if (!test) assert(it[x] == it[t.second]);
      }
      for (auto it : neg) {
        assert(it.size() == nv);
        if (!test) assert(it[x] != it[t.second]);
      }
    }
    void dgeq(int nv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, int mode) const {
      dgaux(nv, pos, neg);
    }
    void dgneq(int nv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, int mode) const {
      dgaux(nv, pos, neg);
    }
    void print(std::ostringstream &fmla) const override {
      fmla << "x" << x << " = " << (t.first ? "x" : "") << t.second;
    }
};

struct fo_neg : public fo {
    fo *sub;
    fo_neg(fo *sub) : sub(sub) {
      fv = sub->fv;
      gen = sub->_gen;
      con = sub->_con;
      _gen = sub->gen;
      _con = sub->con;
      sig = sub->sig;
    }
    ~fo_neg() {
      delete sub;
    }
    bool gen_ex() const {
      return sub->gen_ex();
    }
    bool con_ex() const {
      return sub->con_ex();
    }
    bool srnf(int par) override {
      return par != NEG && sub->srnf(NEG);
    }
    bool ranf(set<int> gv) override {
      return sub->ranf(set<int>()) && is_subset(fv, gv);
    }
    bool no_closed() const {
      return sub->no_closed();
    }
    int nav() const {
      return sub->nav();
    }
    int arity() const {
      return sub->arity();
    }
    multiset<int> col_eqs() const {
      return sub->col_eqs();
    }
    pair<set<int>, set<int> > dgeqs(int mode) const {
      auto x = sub->dgeqs(mode);
      return make_pair(x.second, x.first);
    }
    void dgeq(int nv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, int mode) const {
      sub->dg(nv, neg, pos, db, next, mode);
    }
    void dgneq(int nv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, int mode) const {
      sub->dg(nv, neg, pos, db, next, mode);
    }
    void print(std::ostringstream &fmla) const override {
      fmla << "NOT (";
      sub->print(fmla);
      fmla << ")";
    }
};

struct fo_conj : public fo {
    fo *subl, *subr;
    fo_conj(fo *subl, fo *subr) : subl(subl), subr(subr) {
      fv = set_union(subl->fv, subr->fv);
      for (auto it : set_diff(fv, subl->fv)) {
        subl->con.insert(it);
        subl->_con.insert(it);
      }
      for (auto it : set_diff(fv, subr->fv)) {
        subr->con.insert(it);
        subr->_con.insert(it);
      }
      gen = set_union(subl->gen, subr->gen);
      con = set_union(gen, set_inter(subl->con, subr->con));
      _gen = set_inter(subl->_gen, subr->_gen);
      _con = set_inter(subl->_con, subr->_con);
      sig = set_union(subl->sig, subr->sig);
    }
    ~fo_conj() {
      delete subl;
      delete subr;
    }
    bool gen_ex() const {
      return subl->gen_ex() && subr->gen_ex();
    }
    bool con_ex() const {
      return subl->con_ex() && subr->con_ex();
    }
    bool srnf(int par) override {
      return par != NEG && subl->srnf(BIN) && subr->srnf(BIN);
    }
    bool ranf(set<int> gv) override {
      return subl->ranf(set<int>()) && subr->ranf(subl->fv);
    }
    bool no_closed() const {
      return subl->no_closed() && subr->no_closed();
    }
    int nav() const {
      return max(subl->nav(), subr->nav());
    }
    int arity() const {
      return max((int)fv.size(), max(subl->arity(), subr->arity()));
    }
    multiset<int> col_eqs() const {
      return mset_union(subl->col_eqs(), subr->col_eqs());
    }
    pair<set<int>, set<int> > dgeqs(int mode) const {
      auto v1 = subl->dgeqs(mode), v2 = subr->dgeqs(mode);
      if (mode == 0) return make_pair(set_union(v1.first, v2.first), set_union(v1.second, v2.second));
      else return make_pair(set_union(v1.first, v2.first), set_union(v1.first, v2.second));
    }
    void init_z12(int n, int nv, int *next, int mode, vector<vector<int> > &z1, vector<vector<int> > &z2) const {
      auto v1 = subl->dgeqs(mode), v2 = subr->dgeqs(mode);
      set<int> vz1, vz2;
      if (mode == 0) {
        vz1 = set_union(v1.first, v2.second);
        vz2 = set_union(v1.second, v2.first);
      } else {
        vz1 = set_union(v1.second, v2.second);
        vz2 = set_union(v1.second, v2.first);
      }
      z1 = gen_rand(n, nv, vz1, next);
      z2 = gen_rand(n, nv, vz2, next);
    }
    void dgeq(int nv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, int mode) const {
      int n = min(pos.size(), neg.size());
      vector<vector<int> > z1, z2;
      init_z12(n, nv, next, mode, z1, z2);
      vector<vector<int> > pos1 = pos, pos2 = pos;
      pos1.insert(pos1.end(), z1.begin(), z1.end());
      pos2.insert(pos2.end(), z2.begin(), z2.end());
      vector<vector<int> > neg1 = neg, neg2 = neg;
      neg1.insert(neg1.end(), z1.begin(), z1.end());
      neg2.insert(neg2.end(), z2.begin(), z2.end());
      subl->dg(nv, pos1, neg2, db, next, mode);
      subr->dg(nv, pos2, neg1, db, next, mode);
    }
    void dgneq(int nv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, int mode) const {
      int n = min(pos.size(), neg.size());
      vector<vector<int> > z1, z2;
      init_z12(n, nv, next, mode, z1, z2);
      vector<vector<int> > posneg = pos;
      posneg.insert(posneg.end(), neg.begin(), neg.end());
      vector<vector<int> > z12 = z1;
      z12.insert(z12.end(), z2.begin(), z2.end());
      vector<vector<int> > pos2 = pos;
      pos2.insert(pos2.end(), z2.begin(), z2.end());
      vector<vector<int> > neg1 = neg;
      neg1.insert(neg1.end(), z1.begin(), z1.end());
      subl->dg(nv, posneg, z12, db, next, mode);
      subr->dg(nv, pos2, neg1, db, next, mode);
    }
    void print(std::ostringstream &fmla) const override {
      fmla << "(";
      subl->print(fmla);
      fmla << ") AND (";
      subr->print(fmla);
      fmla << ")";
    }
};

struct fo_disj : public fo {
    fo *subl, *subr;
    fo_disj(fo *subl, fo *subr) : subl(subl), subr(subr) {
      fv = set_union(subl->fv, subr->fv);
      for (auto it : set_diff(fv, subl->fv)) {
        subl->con.insert(it);
        subl->_con.insert(it);
      }
      for (auto it : set_diff(fv, subr->fv)) {
        subr->con.insert(it);
        subr->_con.insert(it);
      }
      gen = set_inter(subl->gen, subr->gen);
      con = set_inter(subl->con, subr->con);
      _gen = set_union(subl->_gen, subr->_gen);
      _con = set_union(_gen, set_inter(subl->_con, subr->_con));
      sig = set_union(subl->sig, subr->sig);
    }
    ~fo_disj() {
      delete subl;
      delete subr;
    }
    bool gen_ex() const {
      return subl->gen_ex() && subr->gen_ex();
    }
    bool con_ex() const {
      return subl->con_ex() && subr->con_ex();
    }
    bool srnf(int par) override {
      return par != NEG && par != EX && subl->srnf(BIN) && subr->srnf(BIN);
    }
    bool ranf(set<int> gv) override {
      return subl->ranf(set<int>()) && subr->ranf(set<int>()) && is_subset(subl->fv, subr->fv) && is_subset(subr->fv, subl->fv);
    }
    bool no_closed() const {
      return subl->no_closed() && subr->no_closed();
    }
    int nav() const {
      return max(subl->nav(), subr->nav());
    }
    int arity() const {
      return max((int)fv.size(), max(subl->arity(), subr->arity()));
    }
    multiset<int> col_eqs() const {
      return mset_union(subl->col_eqs(), subr->col_eqs());
    }
    pair<set<int>, set<int> > dgeqs(int mode) const {
      auto v1 = subl->dgeqs(mode), v2 = subr->dgeqs(mode);
      if (mode == 0) return make_pair(set_union(v1.first, v2.first), set_union(v1.second, v2.second));
      else return make_pair(set_union(v1.first, v2.second), set_union(v1.second, v2.second));
    }
    void init_z12(int n, int nv, int *next, int mode, vector<vector<int> > &z1, vector<vector<int> > &z2) const {
      auto v1 = subl->dgeqs(mode), v2 = subr->dgeqs(mode);
      set<int> vz1, vz2;
      if (mode == 0) {
        vz1 = set_union(v1.first, v2.second);
        vz2 = set_union(v1.second, v2.first);
      } else {
        vz1 = set_union(v1.first, v2.first);
        vz2 = set_union(v1.second, v2.first);
      }
      z1 = gen_rand(n, nv, vz1, next);
      z2 = gen_rand(n, nv, vz2, next);
    }
    void dgeq(int nv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, int mode) const {
      int n = min(pos.size(), neg.size());
      vector<vector<int> > z1, z2;
      init_z12(n, nv, next, mode, z1, z2);
      vector<vector<int> > pos1 = pos, pos2 = pos;
      pos1.insert(pos1.end(), z1.begin(), z1.end());
      pos2.insert(pos2.end(), z2.begin(), z2.end());
      vector<vector<int> > neg1 = neg, neg2 = neg;
      neg1.insert(neg1.end(), z1.begin(), z1.end());
      neg2.insert(neg2.end(), z2.begin(), z2.end());
      subl->dg(nv, pos1, neg2, db, next, mode);
      subr->dg(nv, pos2, neg1, db, next, mode);
    }
    void dgneq(int nv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, int mode) const {
      int n = min(pos.size(), neg.size());
      vector<vector<int> > z1, z2;
      init_z12(n, nv, next, mode, z1, z2);
      vector<vector<int> > pos1 = pos;
      pos1.insert(pos1.end(), z1.begin(), z1.end());
      vector<vector<int> > neg2 = neg;
      neg2.insert(neg2.end(), z2.begin(), z2.end());
      vector<vector<int> > posneg = pos;
      posneg.insert(posneg.end(), neg.begin(), neg.end());
      vector<vector<int> > z12 = z1;
      z12.insert(z12.end(), z2.begin(), z2.end());
      subl->dg(nv, pos1, neg2, db, next, mode);
      subr->dg(nv, z12, posneg, db, next, mode);
    }
    void print(std::ostringstream &fmla) const override {
      fmla << "(";
      subl->print(fmla);
      fmla << ") OR (";
      subr->print(fmla);
      fmla << ")";
    }
};

struct fo_ex : public fo {
    int var;
    fo *sub;
    fo_ex(int var, fo *sub) : var(var), sub(sub) {
      fv = set_rem(sub->fv, var);
      gen = set_rem(sub->gen, var);
      con = set_rem(sub->con, var);
      _gen = set_rem(sub->_gen, var);
      _con = set_rem(sub->_con, var);
      sig = sub->sig;
    }
    ~fo_ex() {
      delete sub;
    }
    bool gen_ex() const {
      return in_set(sub->gen, var) && sub->gen_ex();
    }
    bool con_ex() const {
      return in_set(sub->con, var) && in_set(sub->fv, var) && sub->con_ex();
    }
    bool srnf(int par) override {
      return in_set(sub->fv, var) && sub->srnf(EX);
    }
    bool ranf(set<int> gv) override {
      return sub->ranf(set<int>());
    }
    bool no_closed() const {
      return !fv.empty() && sub->no_closed();
    }
    int nav() const {
      return sub->nav();
    }
    int arity() const {
      return max((int)fv.size(), sub->arity());
    }
    multiset<int> col_eqs() const {
      return sub->col_eqs();
    }
    pair<set<int>, set<int> > dgeqs(int mode) const {
      return sub->dgeqs(mode);
    }
    void dgeq(int nv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, int mode) const {
      sub->dg(nv, pos, neg, db, next, mode);
    }
    void dgneq(int nv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, int mode) const {
      sub->dg(nv, pos, neg, db, next, mode);
    }
    void print(std::ostringstream &fmla) const override {
      fmla << "EXISTS x" << var << ". ";
      sub->print(fmla);
    }
};

FILE *open_file_type(const char *prefix, const char *ftype, const char *mode);
void print_db(FILE *db, vector<pair<int, vector<int> > > es, set<pair<int, int> > sig);
void dump_table(FILE *f, fo *fo, vector<vector<int> > tbl);
void dump(const char *base, fo *fo, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > db, vector<pair<int, vector<int> > > tdb);

#endif
