PREFIX="${1}"
ABS="$(readlink -m "$(dirname "${PREFIX}")")"/"$(basename "${PREFIX}")"

/home/rcsql/src/rtrans.native "${ABS}"

/home/rcsql/tools/db2csv "${ABS}"

/home/rcsql/radb.sh "${ABS}"

psql < "${ABS}.psql" &> /dev/null

/home/rcsql/run_psql.sh "${ABS}.a"
