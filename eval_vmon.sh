PREFIX="${1}"
ABS="$(readlink -m "$(dirname "${PREFIX}")")"/"$(basename "${PREFIX}")"

grep -o "[A-Za-z0-9]*([A-Za-z0-9, ]*)" "${ABS}.fo" | sed "s/[A-Za-z0-9]* *,/int,/g" | sed "s/[A-Za-z0-9]* *)/int)/g" > "${ABS}.sig"
echo "@0 " > "${ABS}.log"
cat "${ABS}.db" >> "${ABS}.log"

/home/rcsql/src/rtrans.native "${ABS}"

. /home/rcsql/functions.sh
symlinks "${ABS}"

/home/rcsql/run_vmon.sh "${ABS}.a"
