to=300
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

line "\\tool\\psqlsub" run01APSQL
line "\\tool\\msqlsub" run01AMSQL
line "\\toolnonopt\\psqlsub" run01SPSQL
line "\\toolnonopt\\msqlsub" run01SMSQL

line "\\vgtool\\psqlsub" run02APSQL
line "\\vgtool\\msqlsub" run02AMSQL
line "\\vgtoolnonopt\\psqlsub" run02SPSQL
line "\\vgtoolnonopt\\msqlsub" run02SMSQL

#echo "\\hline"

#line "\\ail" run03
#line "\\ddd" run04DDD
#line "\\ldd" run04LDD
#line "\\mpreg" run05

rm -f z_*
