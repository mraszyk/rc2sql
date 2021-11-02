function run() {
  local cmd="${1}"

  local ts1="$(date +%s%N)"
  timeout ${to} bash -c "${cmd}" &> /dev/null
  local status="${?}"

  local ts2="$(date +%s%N)"
  local delta="$((ts2 - ts1))"

  local t="$(echo "scale=1; ${delta}/1000000000" | bc -l)"
  if [[ "${status}" == "0" && "$(echo "${t} < ${to}" | bc -l)" == "1" ]]; then
    printf "%.1f" "${t}"
  elif [[ "${status}" == "124" || "$(echo "${t} >= ${to}" | bc -l)" == "1" ]]; then
    echo -n "TO"
  else
    echo -n "RE"
  fi
}

function runNoTO() {
  local cmd="${1}"

  local ts1="$(date +%s%N)"
  bash -c "${cmd}" &> /dev/null
  local status="${?}"

  local ts2="$(date +%s%N)"
  local delta="$((ts2 - ts1))"

  local t="$(echo "scale=1; ${delta}/1000000000" | bc -l)"
  if [[ "${status}" == "0" ]]; then
    printf "%.1f" "${t}"
  else
    echo -n "RE"
  fi
}

function init() {
  echo -n "&"
  runNoTO "./src/rtrans.native z_${i}"
  ./src/vgtrans.native "z_${i}"
  symlinks "${prefix}/z_${i}"
  ./radb.sh "z_${i}"
}

function killPSQL() {
  x="$(psql < /dev/null 2>&1)"
  while [[ "$(echo "${x}" | grep -o "FATAL")" != "" ]]
  do
    sleep 1
    x="$(psql < /dev/null 2>&1)"
  done
  cmd="SELECT pid FROM pg_stat_activity WHERE state = 'active' AND query NOT LIKE '%pg_stat%';"
  pids="$(echo ${cmd} | psql | grep -o '^ *[0-9]* *$')"
  for i in ${pids}
  do
    echo "SELECT pg_terminate_backend(${i});" | psql &> /dev/null
  done
}

function killMSQL() {
  while [[ 1 ]]
  do
    kil=$(mysql -u rcsql -e "SELECT id FROM information_schema.processlist WHERE db = 'db';" -ss | awk '{print "KILL "$1";"}')
    if [[ "${kil}" == "" ]]
    then
      break
    fi
    echo ${kil} | mysql -u rcsql &> /dev/null
  done
}

function checkstat() {
  msg="${1}"
  if [ "${status}" -ne 0 ]
  then
    if [ "${status}" -eq 124 ]
    then
      echo "TO: ${pref}/${msg}" >> log.txt
    elif [ "${status}" -ne 0 ]
    then
      echo "RE: ${pref}/${msg}" >> log.txt
      sleep 10
    fi
  fi
}

function symlinks() {
  pref="${1}"
  ln -sf "${pref}.sig" "${pref}.ssig"
  ln -sf "${pref}.sig" "${pref}.asig"
  ln -sf "${pref}.log" "${pref}.slog"
  ln -sf "${pref}.log" "${pref}.alog"
  ln -sf "${pref}.sig" "${pref}.vssig"
  ln -sf "${pref}.sig" "${pref}.vasig"
  ln -sf "${pref}.log" "${pref}.vslog"
  ln -sf "${pref}.log" "${pref}.valog"
}

function run01SPSQL() {
  run "psql < z_${i}.psql; ./run_psql.sh z_${i}.s"
  killPSQL
}
function run01APSQL() {
  run "psql < z_${i}.psql; ./run_psql.sh z_${i}.a"
  killPSQL
}

function run02SPSQL() {
  if [[ -f z_${i}.vsfin ]]
  then
    run "psql < z_${i}.psql; ./run_psql.sh z_${i}.vs"
    killPSQL
  else
    echo -n "\\vgtna"
  fi
}
function run02APSQL() {
  if [[ -f z_${i}.vafin ]]
  then
    run "psql < z_${i}.psql; ./run_psql.sh z_${i}.va"
    killPSQL
  else
    echo -n "\\vgtna"
  fi
}

function run03() {
  run "./ailamazyan/src/ail.native -fmla z_${i}.fo -db z_${i}.db"
}
function run04DDD() {
  run "./ddd-rc/ddd z_${i}.fo z_${i}.db"
}
function run04LDD() {
  run "./ldd-rc/ldd z_${i}.fo z_${i}.db"
}
function run05() {
  run "./mpreg.sh z_${i}"
}

function line() {
  echo -n "${1}"
  for i in ${seeds}
  do
    echo -n "&"
    ${2} ${i}
  done
  echo "\\\\"
}

function header() {
  echo -n "\\begin{tabular}{@{}l|"
  sep=""
  for i in ${seeds}
  do
    echo -n "${sep}r"
    sep="@{\\cspace}"
  done
  echo "@{}}"
}

function footer() {
  echo "\\end{tabular}"

  rm -f z_*
}
