PREFIX="${1}"

./src/rtrans.native "${PREFIX}"

./radb.sh "${PREFIX}"

mysql -u rcsql --local-infile=1 < "${PREFIX}.msql" &> /dev/null

./run_msql.sh "${PREFIX}.a"
