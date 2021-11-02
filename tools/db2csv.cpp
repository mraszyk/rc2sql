#include "db.h"

#include <cstdio>
#include <string>

int main(int argc, char **argv)
{
  FILE *db_file = open_file_type(argv[1], ".db", "r");
  char *db_buf = NULL;
  size_t db_buf_len = 0;
  size_t db_len = getline(&db_buf, &db_buf_len, db_file);
  fclose(db_file);

  std::map<std::string, std::vector<std::vector<int> > > db = parse_db(db_buf, db_len);
  free(db_buf);

  dump_db(argv[1], db, 0);

  return 0;
}
