PREFIX="${1}"

./src/rtrans.native "${PREFIX}"

./radb.sh "${PREFIX}"

psql < "${PREFIX}.psql" &> /dev/null

./run_psql.sh "${PREFIX}.a"
