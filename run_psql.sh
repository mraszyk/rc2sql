pref="${1}"
inf=$(psql < ${pref}psqlinf | tail -n 2)
if [ "$?" -ne 0 ]
then
  exit 1
fi
if [ "$(echo "${inf}" | grep -o '1 row')" != "" ]
then
  echo "Infinite"
else
  fin=$(psql < ${pref}psqlfin)
  if [[ "$?" -ne 0 ]]
  then
    exit 1
  fi
  echo "Finite"
  fv=$(cat ${pref}psqlfin | head -n 1 | sed "s/--//")
  if [ "${fv}" == "(t)" ]
  then
    echo "()"
    if [ "$(echo "${fin}" | grep -o '1 row')" != "" ]
    then
      echo "()"
    fi
  else
    echo "${fv}"
    echo "${fin}" | tail -n +3 | head -n -1 | sed "s/ //g" | sed "s/|/,/g" | sed -e "s/^/(/" | sed "s/$/)/"
  fi
fi
