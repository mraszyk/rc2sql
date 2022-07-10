to=200
ulimit -s 1048576

. "./functions.sh"

prefix="/home/rcsql"

sz="${1}"
maxn="${2}"
nfv="${3}"
fvgen="${4}"
minl="${5}"
mintl="${6}"
seeds="${7}"

echo -n "\\multicolumn{1}{r@{\\cspace}|@{\\cspace}}{\\trtime}"
for i in ${seeds}
do
  /home/rcsql/tools/gen_test "/home/rcsql/z_${i}" "${sz}" "${maxn}" "${nfv}" "${fvgen}" "${minl}" "${mintl}" "$((${i}%2))" "$((${i}/2))"
  init
done
echo "\\\\"
echo "\\hline"

line "\\tool" run01APSQL
line "\\toolnonopt" run01SPSQL

echo "\\hline"

#line "\\ail" run03
line "\\ddd" run04DDD
line "\\ldd" run04LDD
line "\\mpreg" run05

rm -f z_*
