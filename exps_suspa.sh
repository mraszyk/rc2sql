to=400
small=1000
big=10000

ulimit -s 1048576

. "./functions.sh"

prefix="/home/rcsql"

seeds="0 1 2 3 4 5"

./amazon/gen_test "/home/rcsql/z_0" 1 0 "${small}" 2 1 0
./amazon/gen_test "/home/rcsql/z_1" 1 0 "${big}" 2 1 0
./amazon/gen_test "/home/rcsql/z_2" 1 1 "${small}" 2 1 0
./amazon/gen_test "/home/rcsql/z_3" 1 1 "${big}" 2 1 0
./amazon/gen_test "/home/rcsql/z_4" 2 0 "${small}" 2 1 0
./amazon/gen_test "/home/rcsql/z_5" 2 0 "${big}" 2 1 0

echo -n "\\multicolumn{1}{r@{\\cspace}|@{\\cspace}}{\\trtime}"
echo -n "&"
echo -n "\\multicolumn{2}{c|@{\\cspace}}{"
runNoTO "./src/rtrans.native z_0"
echo -n "}"
echo -n "&"
echo -n "\\multicolumn{2}{c|@{\\cspace}}{"
runNoTO "./src/rtrans.native z_2"
echo -n "}"
echo -n "&"
echo -n "\\multicolumn{2}{c}{"
runNoTO "./src/rtrans.native z_4"
echo -n "}"
echo "\\\\"
echo "\\hline"

for i in {0..5}
do
  init > /dev/null
done

line "\\tool\\psqlsub" run01APSQL
line "\\tool\\msqlsub" run01AMSQL
line "\\toolnonopt\\psqlsub" run01SPSQL
line "\\toolnonopt\\msqlsub" run01SMSQL

line "\\vgtool\\psqlsub" run02APSQL
line "\\vgtool\\msqlsub" run02AMSQL
line "\\vgtoolnonopt\\psqlsub" run02SPSQL
line "\\vgtoolnonopt\\msqlsub" run02SMSQL

echo "\\hline"

#line "\\ail" run03
line "\\ddd" run04DDD
line "\\ldd" run04LDD
line "\\mpreg" run05
