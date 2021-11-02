#ifndef __LOG_H__
#define __LOG_H__

#include <cassert>
#include <climits>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>

static int let(char c) {
    return ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z');
}

static int num(char c) {
    return ('0' <= c && c <= '9');
}

static int anum(char c) {
    return let(c) || num(c);
}

static int ws(char c) {
    return c == ' ' || c == '\n' || c == '\r';
}

std::map<std::string, std::vector<std::vector<int> > > parse_db(const char *db_buf, size_t db_len) {
    std::map<std::string, std::vector<std::vector<int> > > res;
    size_t pos = 0;
    while (pos < db_len && db_len != -1) {
        while (pos < db_len && ws(db_buf[pos])) pos++;
        if (pos == db_len) break;
        if (!let(db_buf[pos])) throw std::runtime_error("corrupted database");
        std::string name;
        while (pos < db_len && anum(db_buf[pos])) name.push_back(db_buf[pos++]);
        while (pos < db_len && ws(db_buf[pos])) pos++;
        if (pos == db_len || db_buf[pos] != '(') throw std::runtime_error("corrupted database");
        pos++;
        std::vector<int> tuple;
        while (pos < db_len && db_buf[pos] != ')') {
            while (pos < db_len && ws(db_buf[pos])) pos++;
            if (pos < db_len && db_buf[pos] == ')') break;
            if (!num(db_buf[pos])) throw std::runtime_error("corrupted database");
            std::string tmp;
            while (pos < db_len && num(db_buf[pos])) tmp.push_back(db_buf[pos++]);
            errno = 0;
            long n = strtol (tmp.c_str(), NULL, 10);
            if (!(INT_MIN <= n && n <= INT_MAX && errno != ERANGE)) throw std::runtime_error("corrupted database");
            tuple.push_back(n);
            while (pos < db_len && ws(db_buf[pos])) pos++;
            if (pos < db_len && db_buf[pos] == ')') break;
            if (pos == db_len || db_buf[pos] != ',') throw std::runtime_error("corrupted database");
            pos++;
        }
        res[name].push_back(tuple);
        if (pos < db_len) pos++;
    }
    return res;
}

FILE *open_file_type(const char *prefix, const char *ftype, const char *mode) {
    std::ostringstream oss;
    oss << prefix << ftype;
    return fopen(oss.str().c_str(), mode);
}

void dump_db(const char *base, std::map<std::string, std::vector<std::vector<int> > > db, int sql_insert = 0) {
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
    for (std::map<std::string, std::vector<std::vector<int> > >::iterator sig = db.begin(); sig != db.end(); sig++) {
        const char *name = sig->first.c_str();
        assert(sig->second.size() != 0);
        int n = sig->second[0].size();
        fprintf(psql, "DROP TABLE IF EXISTS tbl_%s;\n", name);
        fprintf(psql, "CREATE TABLE tbl_%s (", name);
        fprintf(msql, "DROP TABLE IF EXISTS tbl_%s;\n", name);
        fprintf(msql, "CREATE TABLE tbl_%s (", name);
        fprintf(radb, "\\sqlexec_{DROP TABLE IF EXISTS tbl_%s};\n", name);
        fprintf(radb, "\\sqlexec_{CREATE TABLE tbl_%s (", name);
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
          fprintf(psql, "COPY tbl_%s FROM '%s_%s.csv' DELIMITER ',' CSV;\n", name, base, name);
          fprintf(msql, "LOAD DATA LOCAL INFILE '%s_%s.csv' INTO TABLE tbl_%s FIELDS TERMINATED BY ',';\n", base, name, name);
        }
        std::ostringstream oss;
        oss << base << "_" << name << ".csv";
        FILE *tbl = fopen(oss.str().c_str(), "w");
        for (auto it : sig->second) {
          int _n = it.size();
          assert(n == _n);
          if (sql_insert) {
            fprintf(psql, "INSERT INTO tbl_%s VALUES (", name);
            fprintf(msql, "INSERT INTO tbl_%s VALUES (", name);
            fprintf(radb, "\\sqlexec_{INSERT INTO tbl_%s VALUES (", name);
          }
          const char *sep2 = "";
          for (int j = 0; j < n; j++) {
              fprintf(tbl, "%s%d", sep2, it[j]);
              if (sql_insert) {
                fprintf(psql, "%s%d", sep2, it[j]);
                fprintf(msql, "%s%d", sep2, it[j]);
                fprintf(radb, "%s%d", sep2, it[j]);
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
        fclose(tbl);
    }
    fclose(psql);
    fclose(msql);
    fclose(radb);
}

#endif
