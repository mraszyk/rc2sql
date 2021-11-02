PREFIX="${1}"

grep -o "[A-Za-z0-9]*([A-Za-z0-9, ]*)" "${PREFIX}.fo" | sed "s/[A-Za-z0-9]* *,/int,/g" | sed "s/[A-Za-z0-9]* *)/int)/g" > "${PREFIX}.sig"
echo "@0 " > "${PREFIX}.log"
cat "${PREFIX}.db" >> "${PREFIX}.log"

./src/rtrans.native "${PREFIX}"

. ./functions.sh
symlinks "$(readlink -m "$(dirname "${PREFIX}")")"/"$(basename "${PREFIX}")"

./radb.sh "${PREFIX}"

./run_vmon.sh "${PREFIX}.a"
