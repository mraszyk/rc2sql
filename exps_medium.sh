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

for i in ${seeds}
do
  /home/rcsql/tools/gen_test "/home/rcsql/z_${i}" "${sz}" "${maxn}" "${nfv}" "${fvgen}" "${minl}" "${mintl}" 1 "${i}"
  init > /dev/null
done

line "\\tool" run01APSQL
line "\\toolnonopt" run01SPSQL

line "\\vgtool" run02APSQL
line "\\vgtoolnonopt" run02SPSQL

#echo "\\hline"

#line "\\ail" run03
#line "\\ddd" run04DDD
#line "\\ldd" run04LDD
#line "\\mpreg" run05

rm -f z_*
