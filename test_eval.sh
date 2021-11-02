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
if [ "$?" -ne 0 ]
then
  echo "${pref} GEN. TIMEOUT" >> log.txt
  rm -f "${pref}."* "${pref}_"*
  exit 1
fi
timeout "${to}" /home/rcsql/ailamazyan/src/ail.native -fmla "${pref}.fo" -db "${pref}.db" > "${pref}.1" 2> /dev/null
if [ "$?" -ne 0 ]
then
  echo "${pref} AIL. TIMEOUT" >> log.txt
  rm -f "${pref}."* "${pref}_"*
  exit 1
fi
timeout "${to}" /home/rcsql/src/rtrans.native "${pref}" "${i}"
status="$?"
if [ "${status}" -eq 124 ]
then
  echo "${pref} RC2SQL TIMEOUT" >> log.txt
  exit 1
elif [ "${status}" -ne 0 ]
then
  echo "${pref} RC2SQL" >> log.txt
  exit 1
fi
symlinks "${pref}"
/home/rcsql/run_vmon.sh "${pref}.a" > "${pref}.a"
/home/rcsql/run_vmon.sh "${pref}.s" > "${pref}.s"
sufs="a s"
if [[ "${fvgen}" == "1" ]]
then
  timeout "${to}" /home/rcsql/src/vgtrans.native "${pref}" "${i}"
  status="$?"
  if [ "${status}" -eq 124 ]
  then
    echo "${pref} VGTRANSL. TIMEOUT" >> log.txt
    exit 1
  elif [ "${status}" -ne 0 ]
  then
    echo "${pref} VGTRANSL." >> log.txt
    exit 1
  fi
  /home/rcsql/run_vmon.sh "${pref}.va" > "${pref}.va"
  /home/rcsql/run_vmon.sh "${pref}.vs" > "${pref}.vs"
  sufs="${sufs} va vs"
fi
for s in ${sufs}
do
  d=$(/home/rcsql/tools/cmp "${pref}.1" "${pref}.${s}")
  if [[ "${d}" != "OK" ]]
  then
    echo "${d}: ${pref}/${s}" >> log.txt
    exit 1
  fi
done
rm -f "${pref}."* "${pref}_"*
