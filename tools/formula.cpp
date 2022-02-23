#include "formula.h"

int sql_insert = 0;

const int test = 0;

bool sr(const fo *fo) {
  return is_subset(fo->fv, fo->gen) && fo->gen_ex();
}

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

void dump_table(FILE *f, fo *fo, vector<vector<int> > tbl) {
    fprintf(f, "Finite\n");
    fprintf(f, "(");
    for (int i = 0; i < fo->fv.size(); i++) fprintf(f, "%sx%d", (i == 0 ? "" : ","), i);
    fprintf(f, ")\n");
    for (auto it : tbl) {
      fprintf(f, "(");
      const char *sep = "";
      for (int i = 0; i < fo->fv.size(); i++) {
        fprintf(f, "%s%d", sep, it[i]);
        sep = ",";
      }
      fprintf(f, ")\n");
    }
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
    dump_table(fpos, fo, pos);
    fclose(fpos);

    FILE *fneg = open_file_type(base, ".neg", "w");
    dump_table(fneg, fo, neg);
    fclose(fneg);

    FILE *psql = open_file_type(base, ".psql", "w");
    FILE *msql = open_file_type(base, ".msql", "w");
    FILE *radb = open_file_type(base, ".radb", "w");
    fprintf(psql, "DROP TABLE IF EXISTS tbl;\n");
    fprintf(psql, "CREATE TABLE tbl (t INT);\n");
    fprintf(psql, "INSERT INTO tbl VALUES (1);\n");
    fprintf(msql, "USE db;\n");
    fprintf(msql, "DROP TABLE IF EXISTS tbl;\n");
    fprintf(msql, "CREATE TABLE tbl (t INT);\n");
    fprintf(msql, "INSERT INTO tbl VALUES (1);\n");
    fprintf(radb, "\\sqlexec_{DROP TABLE IF EXISTS tbl};\n");
    fprintf(radb, "\\sqlexec_{CREATE TABLE tbl (t INT)};\n");
    fprintf(radb, "\\sqlexec_{INSERT INTO tbl VALUES (1)};\n");
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
