to=300
ulimit -s 1048576

. "./functions.sh"

prefix="/home/rcsql"

sz="${1}"
maxn="${2}"
nfv="${3}"
fvgen="${4}"
minls="${5}"
mintl="${6}"
seeds="${7}"

for minl in ${minls}
do
  for i in ${seeds}
  do
    /home/rcsql/tools/gen_test "/home/rcsql/z_${i}" "${sz}" "${maxn}" "${nfv}" "${fvgen}" "${minl}" "${mintl}" 1 "${i}"
    ./src/rtrans.native "z_${i}"
    symlinks "/home/rcsql/z_${i}"
    ./radb.sh "z_${i}"
  done

  line "\$\\eparam=${minl}\$" run01APSQL

  rm -f z_*
done
