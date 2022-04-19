function sqlofra() {
  pref="${1}"
  suf="${2}"
  flags="${3}"
  echo -n "--"
  /usr/local/bin/radb ${flags:+"${flags}"} "${pref}.rdb" < "${pref}.${suf}" | grep ":number" | sed "s/ //g" | sed "s/:number//g" | grep -o "(.*)"
  /usr/local/bin/radb ${flags:+"${flags}"} "${pref}.rdb" -d < "${pref}.${suf}" | grep -oz "SQL generated:.*:number" | tail -n +2 | head -n -1
}

pref="${1}"

/usr/local/bin/radb ${flags:+"${flags}"} -i "${pref}.radb" "${pref}.rdb" > /dev/null

sqlofra "${pref}" "srfin" > "${pref}.spsqlfin"
sqlofra "${pref}" "srinf" > "${pref}.spsqlinf"
sqlofra "${pref}" "srfin" "-m" > "${pref}.smsqlfin"
sqlofra "${pref}" "srinf" "-m" > "${pref}.smsqlinf"
sqlofra "${pref}" "arfin" > "${pref}.apsqlfin"
sqlofra "${pref}" "arinf" > "${pref}.apsqlinf"
sqlofra "${pref}" "arfin" "-m" > "${pref}.amsqlfin"
sqlofra "${pref}" "arinf" "-m" > "${pref}.amsqlinf"

if [[ -f "${pref}.vsrfin" ]]
then
  sqlofra "${pref}" "vsrfin" > "${pref}.vspsqlfin"
  sqlofra "${pref}" "vsrinf" > "${pref}.vspsqlinf"
  sqlofra "${pref}" "vsrfin" "-m" > "${pref}.vsmsqlfin"
  sqlofra "${pref}" "vsrinf" "-m" > "${pref}.vsmsqlinf"
  sqlofra "${pref}" "varfin" > "${pref}.vapsqlfin"
  sqlofra "${pref}" "varinf" > "${pref}.vapsqlinf"
  sqlofra "${pref}" "varfin" "-m" > "${pref}.vamsqlfin"
  sqlofra "${pref}" "varinf" "-m" > "${pref}.vamsqlinf"
fi
