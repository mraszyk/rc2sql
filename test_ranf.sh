to=60
ulimit -s 1048576

. "./functions.sh"

sz="${1}"
maxn="${2}"
nfv="${3}"
fvgen="${4}"
minl="${5}"
mintl="${6}"
str="${7}"
i="${8}"

pref="/home/rcsql/z_${1}_${2}_${3}_${4}_${5}_${6}_${7}_${8}"

timeout "${to}" /home/rcsql/tools/gen_test "${pref}" "${sz}" "${maxn}" "${nfv}" "${fvgen}" "${minl}" "${mintl}" "${str}" "${i}"
status="${?}"
checkstat "GEN"

timeout "${to}" /home/rcsql/src/rtrans.native "${pref}" "${i}"
status="${?}"
checkstat "RC2SQL"

# "a" must come before "s"
sufs="a s"

if [[ "${fvgen}" == "1" ]]
then
  timeout "${to}" /home/rcsql/src/vgtrans.native "${pref}" "${i}"
  status="${?}"
  checkstat "VGTRANSL"

  sufs="${sufs} va vs"
fi

symlinks "${pref}"
./radb.sh "${pref}"

psql < "${pref}.psql" > /dev/null
mysql -h 127.0.0.1 -P 3306 -u rcsql --local-infile=1 < "${pref}.msql"

for s in ${sufs}
do
  timeout "${to}" bash -c "/home/rcsql/run_psql.sh ${pref}.${s}" > "${pref}.${s}p"
  status="${?}"
  checkstat "${s}p"
  killPSQL
  pstatus="${status}"

  timeout "${to}" bash -c "/home/rcsql/run_msql.sh ${pref}.${s}" > "${pref}.${s}m"
  status="${?}"
  checkstat "${s}m"
  killMSQL
  mstatus="${status}"

  d=$(/home/rcsql/tools/cmp "${pref}.${s}p" "${pref}.${s}m")
  if [[ "${d}" != "OK" && "${pstatus}" == "0" && "${mstatus}" == "0" ]]
  then
    echo "${d}(p-m): ${pref}/${s}" >> log.txt
  fi

  timeout "${to}" bash -c "/home/rcsql/run_vmon.sh ${pref}.${s}" > "${pref}.${s}v"
  status="${?}"
  checkstat "${s}v"
  vstatus="${status}"
  if [[ "${s}" == "a" ]]
  then
    avstatus="${status}"
  fi

  d=$(/home/rcsql/tools/cmp "${pref}.${s}p" "${pref}.${s}v")
  if [[ "${d}" != "OK" && "${pstatus}" == "0" && "${vstatus}" == "0" ]]
  then
    echo "${d}(p-v): ${pref}/${s}" >> log.txt
  fi

  timeout "${to}" bash -c "/home/rcsql/run_sqlite.sh ${pref} ${s}" > "${pref}.${s}l"
  status="${?}"
  checkstat "${s}l"

  d=$(/home/rcsql/tools/cmp "${pref}.${s}p" "${pref}.${s}l")
  if [[ "${d}" != "OK" && "${pstatus}" == "0" && "${status}" == "0" ]]
  then
    echo "${d}(p-l): ${pref}/${s}" >> log.txt
  fi

  d=$(/home/rcsql/tools/cmp "${pref}.${s}v" "${pref}.av")
  if [[ "${d}" != "OK" && "${vstatus}" == "0" && "${avstatus}" == "0" ]]
  then
    echo "${d}(v-v): ${pref}/${s}" >> log.txt
  fi

  d=$(/home/rcsql/tools/cmp "${pref}.${s}v" "${pref}.pos" "${pref}.neg")
  if [[ "${d}" != "OK" && "${vstatus}" == "0" ]]
  then
    echo "${d}(dg): ${pref}/${s}" >> log.txt
  fi
done
