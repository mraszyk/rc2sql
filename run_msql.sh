pref="${1}"
inf=$(cat ${pref}msqlinf | sed "s/^--.*$/USE db;/" | mysql -u rcsql 2> /dev/null | tail -n 1)
if [ "$?" -ne 0 ]
then
  exit 1
fi
if [ "$(echo "${inf}" | grep -o '1')" != "" ]
then
  echo "Infinite"
else
  fin=$(cat ${pref}msqlfin | sed "s/^--.*$/USE db;/" | mysql -u rcsql 2> /dev/null)
  if [[ "$?" -ne 0 ]]
  then
    exit 1
  fi
  echo "Finite"
  fv=$(cat ${pref}msqlfin | head -n 1 | sed "s/--//")
  if [ "${fv}" == "(t)" ]
  then
    echo "()"
    if [ "$(echo "${fin}" | grep -o '1')" != "" ]
    then
      echo "()"
    fi
  else
    echo "${fv}"
    echo "${fin}" | tail -n +2 | sed "s/\t/,/g" | sed -e "s/^/(/" | sed "s/$/)/"
  fi
fi
