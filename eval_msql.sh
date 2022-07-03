PREFIX="${1}"
ABS="$(readlink -m "$(dirname "${PREFIX}")")"/"$(basename "${PREFIX}")"

/home/rcsql/src/rtrans.native "${ABS}"

/home/rcsql/tools/db2csv "${ABS}"

/home/rcsql/radb.sh "${ABS}"

mysql -h 127.0.0.1 -P 3306 -u rcsql --local-infile=1 < "${ABS}.msql" &> /dev/null

/home/rcsql/run_msql.sh "${ABS}.a"
