pref="${1}"
inf=$(./monpoly/monpoly -sig "${pref}sig" -log "${pref}log" -formula "${pref}inf" -no_rw -verified)
if [ "$?" -ne 0 ]
then
  exit 1
fi
if [ "${inf}" == "" ]
then
  fv=$(./monpoly/monpoly -sig "${pref}sig" -formula "${pref}fin" -no_rw -verified -check)
  fin=$(./monpoly/monpoly -sig "${pref}sig" -log "${pref}log" -formula "${pref}fin" -no_rw -verified)
  if [ "$?" -ne 0 ]
  then
    exit 1
  fi
  echo "Finite"
  echo "${fv}" | grep "free variables" | sed "s/.*is: \(.*\)/\1/"
  echo "${fin}" | grep -o "([0-9,]*)\|false\|true" | sed "s/false//" | sed "s/true/()/"
else
  echo "Infinite"
fi
