function sqlofra() {
  pref="${1}"
  suf="${2}"
  echo -n "--"
  /usr/local/bin/radb "${pref}.rdb" < "${pref}.${suf}" | grep ":number" | sed "s/ //g" | sed "s/:number//g" | grep -o "(.*)"
  /usr/local/bin/radb "${pref}.rdb" -d < "${pref}.${suf}" | grep -oz "SQL generated:.*:number" | tail -n +2 | head -n -1
}

pref="${1}"

/usr/local/bin/radb -i "${pref}.radb" "${pref}.rdb" > /dev/null

sqlofra "${pref}" "srfin" > "${pref}.ssqlfin"
sqlofra "${pref}" "srinf" > "${pref}.ssqlinf"
sqlofra "${pref}" "arfin" > "${pref}.asqlfin"
sqlofra "${pref}" "arinf" > "${pref}.asqlinf"

if [[ -f "${pref}.vsrfin" ]]
then
  sqlofra "${pref}" "vsrfin" > "${pref}.vssqlfin"
  sqlofra "${pref}" "vsrinf" > "${pref}.vssqlinf"
  sqlofra "${pref}" "varfin" > "${pref}.vasqlfin"
  sqlofra "${pref}" "varinf" > "${pref}.vasqlinf"
fi
