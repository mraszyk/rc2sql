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
const int AND = 4;
const int OR = 5;

int sql_insert = 0;

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

static vector<vector<int> > gen_rand(int n, int nfv, set<int> eq, int *next) {
  vector<vector<int> > data(n);
  int copy = -1;
  for (int i = 0; i < nfv; i++) {
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
    bool check_dupl() const {
      auto eqs = col_eqs();
      for (auto it : eqs) {
        if (eqs.count(it) > 1) return 0;
      }
      return 1;
    }
    virtual bool no_dupl() const = 0;
    virtual bool check_gfv(set<int> gfv) const = 0;
    virtual int arity() const = 0;
    virtual multiset<pair<int, pair<bool, int> > > col_eqs() const = 0;
    virtual set<int> evar(int l) const = 0;
    virtual int dgeq(int nfv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, std::mt19937 &gen,int mode) const = 0;
    virtual int dgneq(int nfv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, std::mt19937 &gen, int mode) const = 0;
    int dg(int nfv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, std::mt19937 &gen, int mode) {
      if (mode == 0 || (mode == 2 && gen() % 2 == 0)) {
        return dgeq(nfv, pos, neg, db, next, gen, mode);
      } else {
        return dgneq(nfv, pos, neg, db, next, gen, mode);
      }
    }
    virtual void print(std::ostringstream &fmla) const = 0;
};

bool sr(const fo *fo) {
  return is_subset(fo->fv, fo->gen) && fo->gen_ex();
}

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
    bool no_dupl() const {
      return true;
    }
    bool no_leq() const {
      return true;
    }
    bool check_gfv(set<int> gfv) const {
      return set_inter(fv, gfv).size() != 0;
    }
    int arity() const {
      return fv.size();
    }
    multiset<pair<int, pair<bool, int> > > col_eqs() const {
      return multiset<pair<int, pair<bool, int> > >();
    }
    set<int> evar(int l) const {
      return set<int>();
    }
    vector<int> unify(int nfv, vector<int> it, vector<pair<bool, int> > ts) const {
      assert(it.size() == nfv);
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
    int dgaux(int nfv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next) const {
      set<vector<int> > table;
      for (auto it : pos) {
        vector<int> cur = unify(nfv, it, ts);
        table.insert(cur);
        db.push_back(make_pair(id, cur));
      }
      for (auto it : neg) {
        vector<int> cur = unify(nfv, it, ts);
        assert (table.find(cur) == table.end());
      }
      return 1;
    }
    int dgeq(int nfv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, std::mt19937 &gen, int mode) const {
      return dgaux(nfv, pos, neg, db, next);
    }
    int dgneq(int nfv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, std::mt19937 &gen, int mode) const {
      return dgaux(nfv, pos, neg, db, next);
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
    bool no_dupl() const {
      return true;
    }
    bool check_gfv(set<int> gfv) const {
      return true;
    }
    int arity() const {
      return fv.size();
    }
    multiset<pair<int, pair<bool, int> > > col_eqs() const {
      multiset<pair<int, pair<bool, int> > > res;
      res.insert(make_pair(x, t));
      return res;
    }
    set<int> evar(int l) const {
      if (l & 1) return fv;
      else return set<int>();
    }
    int dgaux(int nfv, vector<vector<int> > &pos, vector<vector<int> > &neg) const {
      assert(t.first);
      for (auto it : pos) {
        assert(it.size() == nfv);
        if (it[x] != it[t.second]) return 0;
      }
      for (auto it : neg) {
        assert(it.size() == nfv);
        if (it[x] == it[t.second]) return 0;
      }
      return 1;
    }
    int dgeq(int nfv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, std::mt19937 &gen, int mode) const {
      return dgaux(nfv, pos, neg);
    }
    int dgneq(int nfv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, std::mt19937 &gen, int mode) const {
      return dgaux(nfv, pos, neg);
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
    bool no_dupl() const {
      return sub->no_dupl();
    }
    bool check_gfv(set<int> gfv) const {
      return sub->check_gfv(gfv);
    }
    int arity() const {
      return sub->arity();
    }
    multiset<pair<int, pair<bool, int> > > col_eqs() const {
      return sub->col_eqs();
    }
    set<int> evar(int l) const {
      return sub->evar(l + 1);
    }
    int dgeq(int nfv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, std::mt19937 &gen, int mode) const {
      return sub->dg(nfv, neg, pos, db, next, gen, mode);
    }
    int dgneq(int nfv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, std::mt19937 &gen, int mode) const {
      return sub->dg(nfv, neg, pos, db, next, gen, mode);
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
    bool no_dupl() const {
      return check_dupl() && subl->no_dupl() && subr->no_dupl();
    }
    bool check_gfv(set<int> gfv) const {
      return subl->check_gfv(gfv) && subr->check_gfv(gfv);
    }
    int arity() const {
      return max((int)fv.size(), max(subl->arity(), subr->arity()));
    }
    multiset<pair<int, pair<bool, int> > > col_eqs() const {
      return mset_union(subl->col_eqs(), subr->col_eqs());
    }
    set<int> evar(int l) const {
      return set_union(subl->evar(l), subr->evar(l));
    }
    int dgeq(int nfv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, std::mt19937 &gen, int mode) const {
      int n = min(pos.size(), neg.size());
      vector<vector<int> > z1 = gen_rand((mode == 2 && gen() % 2 == 0 ? 0 : n), nfv, set_union(subl->evar(1), subr->evar(0)), next);
      vector<vector<int> > z2 = gen_rand((mode == 2 && gen() % 2 == 0 ? 0 : n), nfv, set_union(subl->evar(0), subr->evar(1)), next);
      vector<vector<int> > pos1 = pos, pos2 = pos;
      pos1.insert(pos1.end(), z1.begin(), z1.end());
      pos2.insert(pos2.end(), z2.begin(), z2.end());
      vector<vector<int> > neg1 = neg, neg2 = neg;
      neg1.insert(neg1.end(), z1.begin(), z1.end());
      neg2.insert(neg2.end(), z2.begin(), z2.end());
      return subl->dg(nfv, pos1, neg2, db, next, gen, mode) && subr->dg(nfv, pos2, neg1, db, next, gen, mode);
    }
    int dgneq(int nfv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, std::mt19937 &gen, int mode) const {
      auto subl = this->subl;
      auto subr = this->subr;
      if (mode == 2 && gen() % 2 == 0) swap(subl, subr);
      int n = min(pos.size(), neg.size());
      vector<vector<int> > z1 = gen_rand((mode == 2 && gen() % 2 == 0 ? 0 : n), nfv, set_union(subl->evar(0), subr->evar(0)), next);
      vector<vector<int> > z2 = gen_rand((mode == 2 && gen() % 2 == 0 ? 0 : n), nfv, set_union(subl->evar(0), subr->evar(1)), next);
      vector<vector<int> > pos1 = pos, pos2 = pos;
      pos1.insert(pos1.end(), neg.begin(), neg.end());
      pos2.insert(pos2.end(), z2.begin(), z2.end());
      vector<vector<int> > neg1 = neg, neg2 = z2;
      neg1.insert(neg1.end(), z1.begin(), z1.end());
      neg2.insert(neg2.end(), z1.begin(), z1.end());
      return subl->dg(nfv, pos1, neg2, db, next, gen, mode) && subr->dg(nfv, pos2, neg1, db, next, gen, mode);
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
    bool no_dupl() const {
      return check_dupl() && subl->no_dupl() && subr->no_dupl();
    }
    bool check_gfv(set<int> gfv) const {
      return subl->check_gfv(gfv) && subr->check_gfv(gfv);
    }
    int arity() const {
      return max((int)fv.size(), max(subl->arity(), subr->arity()));
    }
    multiset<pair<int, pair<bool, int> > > col_eqs() const {
      return mset_union(subl->col_eqs(), subr->col_eqs());
    }
    set<int> evar(int l) const {
      return set_union(subl->evar(l), subr->evar(l));
    }
    int dgeq(int nfv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, std::mt19937 &gen, int mode) const {
      int n = min(pos.size(), neg.size());
      vector<vector<int> > z1 = gen_rand((mode == 2 && gen() % 2 == 0 ? 0 : n), nfv, set_union(subl->evar(1), subr->evar(0)), next);
      vector<vector<int> > z2 = gen_rand((mode == 2 && gen() % 2 == 0 ? 0 : n), nfv, set_union(subl->evar(0), subr->evar(1)), next);
      vector<vector<int> > pos1 = pos, pos2 = pos;
      pos1.insert(pos1.end(), z1.begin(), z1.end());
      pos2.insert(pos2.end(), z2.begin(), z2.end());
      vector<vector<int> > neg1 = neg, neg2 = neg;
      neg1.insert(neg1.end(), z1.begin(), z1.end());
      neg2.insert(neg2.end(), z2.begin(), z2.end());
      return subl->dg(nfv, pos1, neg2, db, next, gen, mode) && subr->dg(nfv, pos2, neg1, db, next, gen, mode);
    }
    int dgneq(int nfv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, std::mt19937 &gen, int mode) const {
      auto subl = this->subl;
      auto subr = this->subr;
      if (mode == 2 && gen() % 2 == 0) swap(subl, subr);
      int n = min(pos.size(), neg.size());
      vector<vector<int> > z1 = gen_rand((mode == 2 && gen() % 2 == 0 ? 0 : n), nfv, set_union(subl->evar(1), subr->evar(1)), next);
      vector<vector<int> > z2 = gen_rand((mode == 2 && gen() % 2 == 0 ? 0 : n), nfv, set_union(subl->evar(0), subr->evar(1)), next);
      vector<vector<int> > neg1 = neg, neg2 = neg;
      neg1.insert(neg1.end(), pos.begin(), pos.end());
      neg2.insert(neg2.end(), z2.begin(), z2.end());
      vector<vector<int> > pos1 = pos, pos2 = z2;
      pos1.insert(pos1.end(), z1.begin(), z1.end());
      pos2.insert(pos2.end(), z1.begin(), z1.end());
      return subl->dg(nfv, pos1, neg2, db, next, gen, mode) && subr->dg(nfv, pos2, neg1, db, next, gen, mode);
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
    bool no_dupl() const {
      return sub->no_dupl();
    }
    bool check_gfv(set<int> gfv) const {
      return sub->check_gfv(gfv);
    }
    int arity() const {
      return max((int)fv.size(), sub->arity());
    }
    multiset<pair<int, pair<bool, int> > > col_eqs() const {
      multiset<pair<int, pair<bool, int> > > res;
      for (auto it : sub->col_eqs()) {
        if (it.first != var && (!it.second.first || it.second.second != var)) res.insert(it);
      }
      return res;
    }
    set<int> evar(int l) const {
      return set_rem(sub->evar(l), var);
    }
    int dgeq(int nfv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, std::mt19937 &gen, int mode) const {
      int cpos = -1, cneg = -1;
      if (in_set(sub->evar(1), var)) cpos = *evar(1).begin();
      if (in_set(sub->evar(0), var)) cneg = *evar(0).begin();
      app_rand(pos, cpos, next);
      app_rand(neg, cneg, next);
      return sub->dg(nfv + 1, pos, neg, db, next, gen, mode);
    }
    int dgneq(int nfv, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > &db, int *next, std::mt19937 &gen, int mode) const {
      int cpos = -1, cneg = -1;
      if (in_set(sub->evar(1), var)) cpos = *evar(1).begin();
      if (in_set(sub->evar(0), var)) cneg = *evar(0).begin();
      app_rand(pos, cpos, next);
      app_rand(neg, cneg, next);
      return sub->dg(nfv + 1, pos, neg, db, next, gen, mode);
    }
    void print(std::ostringstream &fmla) const override {
      fmla << "EXISTS x" << var << ". ";
      sub->print(fmla);
    }
};

FILE *open_file_type(const char *prefix, const char *ftype, const char *mode) {
    std::ostringstream oss;
    oss << prefix << ftype;
    return fopen(oss.str().c_str(), mode);
}

void print_db(FILE *db, vector<pair<int, vector<int> > > es, set<pair<int, int> > sig) {
    const char *sep = "";
    for (auto it : es) {
        int id = it.first;
        int n = it.second.size();
        if (sig.find(make_pair(id, n)) == sig.end()) continue;
        fprintf(db, "%sP%dA%d(", sep, id, n);
        sep = " ";
        const char *sep2 = "";
        for (int j = 0; j < n; j++) {
            fprintf(db, "%s%d", sep2, it.second[j]);
            sep2 = ", ";
        }
        fprintf(db, ")");
    }
    fprintf(db, "\n");
}

void dump(const char *base, fo *fo, vector<vector<int> > pos, vector<vector<int> > neg, vector<pair<int, vector<int> > > db, vector<pair<int, vector<int> > > tdb) {
    FILE *fmla = open_file_type(base, ".fo", "w");
    std::ostringstream oss;
    fo->print(oss);
    fprintf(fmla, "%s\n", oss.str().c_str());
    fclose(fmla);

    FILE *sigf = open_file_type(base, ".sig", "w");
    for (set<pair<int, int> >::iterator it = fo->sig.begin(); it != fo->sig.end(); it++) {
        int id = it->first;
        int n = it->second;
        fprintf(sigf, "P%dA%d(", id, n);
        const char *sep = "";
        for (int i = 0; i < n; i++) {
            fprintf(sigf, "%sint", sep);
            sep = ",";
        }
        fprintf(sigf, ")\n");
    }
    fclose(sigf);

    FILE *fdb = open_file_type(base, ".db", "w");
    print_db(fdb, db, fo->sig);
    fclose(fdb);
    FILE *log = open_file_type(base, ".log", "w");
    fprintf(log, "@0 ");
    print_db(log, db, fo->sig);
    fclose(log);

    FILE *ftdb = open_file_type(base, ".tdb", "w");
    print_db(ftdb, tdb, fo->sig);
    fclose(ftdb);

    FILE *fpos = open_file_type(base, ".pos", "w");
    fprintf(fpos, "Finite\n");
    fprintf(fpos, "(");
    for (int i = 0; i < fo->fv.size(); i++) fprintf(fpos, "%sx%d", (i == 0 ? "" : ","), i);
    fprintf(fpos, ")\n");
    for (auto it : pos) {
      fprintf(fpos, "(");
      const char *sep = "";
      for (auto it2 : it) {
        fprintf(fpos, "%s%d", sep, it2);
        sep = ",";
      }
      fprintf(fpos, ")\n");
    }
    fclose(fpos);

    FILE *fneg = open_file_type(base, ".neg", "w");
    fprintf(fneg, "Finite\n");
    fprintf(fneg, "(");
    for (int i = 0; i < fo->fv.size(); i++) fprintf(fneg, "%sx%d", (i == 0 ? "" : ","), i);
    fprintf(fneg, ")\n");
    for (auto it : neg) {
      fprintf(fneg, "(");
      const char *sep = "";
      for (auto it2 : it) {
        fprintf(fneg, "%s%d", sep, it2);
        sep = ",";
      }
      fprintf(fneg, ")\n");
    }
    fclose(fneg);

    FILE *psql = open_file_type(base, ".psql", "w");
    FILE *msql = open_file_type(base, ".msql", "w");
    FILE *radb = open_file_type(base, ".radb", "w");
    fprintf(psql, "DROP TABLE IF EXISTS tbl_T;\n");
    fprintf(psql, "CREATE TABLE tbl_T (t INT);\n");
    fprintf(psql, "INSERT INTO tbl_T VALUES (1);\n");
    fprintf(msql, "USE db;\n");
    fprintf(msql, "DROP TABLE IF EXISTS tbl_T;\n");
    fprintf(msql, "CREATE TABLE tbl_T (t INT);\n");
    fprintf(msql, "INSERT INTO tbl_T VALUES (1);\n");
    fprintf(radb, "\\sqlexec_{DROP TABLE IF EXISTS tbl_T};\n");
    fprintf(radb, "\\sqlexec_{CREATE TABLE tbl_T (t INT)};\n");
    fprintf(radb, "\\sqlexec_{INSERT INTO tbl_T VALUES (1)};\n");
    for (set<pair<int, int> >::iterator it = fo->sig.begin(); it != fo->sig.end(); it++) {
        int id = it->first;
        int n = it->second;
        fprintf(psql, "DROP TABLE IF EXISTS tbl_P%dA%d;\n", id, n);
        fprintf(psql, "CREATE TABLE tbl_P%dA%d (", id, n);
        fprintf(msql, "DROP TABLE IF EXISTS tbl_P%dA%d;\n", id, n);
        fprintf(msql, "CREATE TABLE tbl_P%dA%d (", id, n);
        fprintf(radb, "\\sqlexec_{DROP TABLE IF EXISTS tbl_P%dA%d};\n", id, n);
        fprintf(radb, "\\sqlexec_{CREATE TABLE tbl_P%dA%d (", id, n);
        const char *sep = "";
        for (int i = 0; i < n; i++) {
          fprintf(psql, "%sx%d INT", sep, i);
          fprintf(msql, "%sx%d INT", sep, i);
          fprintf(radb, "%sx%d INT", sep, i);
          sep = ", ";
        }
        fprintf(psql, ");\n");
        fprintf(msql, ");\n");
        fprintf(radb, ")};\n");
        if (!sql_insert) {
          fprintf(psql, "COPY tbl_P%dA%d FROM '%s_P%dA%d.csv' DELIMITER ',' CSV;\n", id, n, base, id, n);
          fprintf(msql, "LOAD DATA LOCAL INFILE '%s_P%dA%d.csv' INTO TABLE tbl_P%dA%d FIELDS TERMINATED BY ',';\n", base, id, n, id, n);
        }
        std::ostringstream oss;
        oss << base << "_P" << id << "A" << n << ".csv";
        FILE *tbl = fopen(oss.str().c_str(), "w");
        for (auto it : db) {
          int _id = it.first;
          int _n = it.second.size();
          if (id == _id && n == _n) {
            if (sql_insert) {
              fprintf(psql, "INSERT INTO tbl_P%dA%d VALUES (", id, n);
              fprintf(msql, "INSERT INTO tbl_P%dA%d VALUES (", id, n);
              fprintf(radb, "\\sqlexec_{INSERT INTO tbl_P%dA%d VALUES (", id, n);
            }
            const char *sep2 = "";
            for (int j = 0; j < n; j++) {
                fprintf(tbl, "%s%d", sep2, it.second[j]);
                if (sql_insert) {
                  fprintf(psql, "%s%d", sep2, it.second[j]);
                  fprintf(msql, "%s%d", sep2, it.second[j]);
                  fprintf(radb, "%s%d", sep2, it.second[j]);
                }
                sep2 = ",";
            }
            fprintf(tbl, "\n");
            if (sql_insert) {
              fprintf(psql, ");\n");
              fprintf(msql, ");\n");
              fprintf(radb, ")};\n");
            }
          }
        }
        fclose(tbl);
    }
    fclose(psql);
    fclose(msql);
    fclose(radb);
}

#endif
