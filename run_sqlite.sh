pref="${1}"
suf="${2}"
inf=$(/usr/local/bin/radb --run ${@:3} "${pref}.rdb" < "${pref}.${suf}rinf")
if [ "$?" -ne 0 ]
then
  exit 1
fi
if [ "$(echo ${inf} | grep -o "1 tuple returned")" == "" ]
then
  q=$(/usr/local/bin/radb --run ${@:3} "${pref}.rdb" < "${pref}.${suf}rfin")
  if [ "$?" -ne 0 ]
  then
    exit 1
  fi
  fv=$(echo "${q}" | grep ":number" | sed "s/ //g" | sed "s/:number//g")
  fin=$(echo "${q}" | grep "^[0-9, ]*$")
  echo "Finite"
  echo "${fv}"
  echo "${fin}" | sed "s/ //g" | sed "s/\(.\+\)/(\1)/"
else
  echo "Infinite"
fi
