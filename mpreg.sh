pref="${1}"
out=$(./monpoly-reg-1.0/monpoly-reg -mona_dir /home/rcsql/monpoly-reg-1.0/monaaut -sig ${pref}.sig -log ${pref}.log -formula ${pref}.mrfotl)
if [ "$?" -ne 0 ]
then
  exit 1
fi
inf=$(echo "${out}" | grep -o "infinite")
if [ "${inf}" == "" ]
then
  fv=$(./monpoly-reg-1.0/monpoly-reg -mona_dir /home/rcsql/monpoly-reg-1.0/monaaut -sig ${pref}.sig -formula ${pref}.mrfotl -check | grep -o "([x0-9,]*)$")
  if [ "$?" -ne 0 ]
  then
    exit 1
  fi
  echo "Finite"
  echo "${fv}"
  echo "${out}" | grep -o "([0-9,]*)\|false\|true" | sed "s/false//" | sed "s/true/()/"
else
  echo "Infinite"
fi
