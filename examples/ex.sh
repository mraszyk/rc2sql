./tools/gen_test /home/rcsql/examples/ex 8 4 2 1 10 10 0 0 # 1st argument of gen_test must be ABSOLUTE path
./src/fo2reg.native examples/ex

./eval_vmon.sh examples/ex > examples/ex.eoa
./eval_psql.sh examples/ex > examples/ex.eopa
./eval_msql.sh examples/ex > examples/ex.eoma

./src/rtrans.native examples/ex

./src/vgtrans.native examples/ex

. ./functions.sh
symlinks /home/rcsql/examples/ex # 1st argument of symlinks must be ABSOLUTE path

./radb.sh examples/ex

psql < examples/ex.psql
mysql -h 127.0.0.1 -P 3306 -u rcsql --local-infile=1 < examples/ex.msql

./run_vmon.sh examples/ex.a > examples/ex.oa    # RC2SQL
diff examples/ex.oa examples/ex.eoa
./run_vmon.sh examples/ex.s > examples/ex.os    # RC2SQL*
./run_vmon.sh examples/ex.va > examples/ex.ova  # VGTrans
./run_vmon.sh examples/ex.vs > examples/ex.ovs  # VGTrans*
./run_psql.sh examples/ex.a > examples/ex.opa   # RC2SQL
diff examples/ex.opa examples/ex.eopa
./run_psql.sh examples/ex.s > examples/ex.ops   # RC2SQL*
./run_psql.sh examples/ex.va > examples/ex.opva # VGTrans
./run_psql.sh examples/ex.vs > examples/ex.opvs # VGTrans*
./run_msql.sh examples/ex.a > examples/ex.oma   # RC2SQL
diff examples/ex.oma examples/ex.eoma
./run_msql.sh examples/ex.s > examples/ex.oms   # RC2SQL*
./run_msql.sh examples/ex.va > examples/ex.omva # VGTrans
./run_msql.sh examples/ex.vs > examples/ex.omvs # VGTrans*

./ailamazyan/src/ail.native -fmla examples/ex.fo -db examples/ex.db > examples/ex.ail # Ailamazyan et al.
./ddd-rc/ddd examples/ex.fo examples/ex.db > examples/ex.ddd # DDD
./ldd-rc/ldd examples/ex.fo examples/ex.db > examples/ex.ldd # LDD
./mpreg.sh examples/ex > examples/ex.mpreg # MonPoly-REG

./tools/cmp examples/ex.ail examples/ex.oa
./tools/cmp examples/ex.ail examples/ex.os
./tools/cmp examples/ex.ail examples/ex.ova
./tools/cmp examples/ex.ail examples/ex.ovs
./tools/cmp examples/ex.ail examples/ex.opa
./tools/cmp examples/ex.ail examples/ex.ops
./tools/cmp examples/ex.ail examples/ex.opva
./tools/cmp examples/ex.ail examples/ex.opvs
./tools/cmp examples/ex.ail examples/ex.oma
./tools/cmp examples/ex.ail examples/ex.oms
./tools/cmp examples/ex.ail examples/ex.omva
./tools/cmp examples/ex.ail examples/ex.omvs
./tools/cmp examples/ex.ail examples/ex.mpreg

python3 cnt.py examples/ex.asqlfin | psql
python3 cnt.py examples/ex.asqlinf | psql
python3 cnt.py examples/ex.asqlfin | sed "s/WITH/USE db;\nWITH/" | mysql -h 127.0.0.1 -P 3306 -u rcsql
python3 cnt.py examples/ex.asqlinf | sed "s/WITH/USE db;\nWITH/" | mysql -h 127.0.0.1 -P 3306 -u rcsql
python3 cnt.py examples/ex.ssqlfin | psql
python3 cnt.py examples/ex.ssqlinf | psql
python3 cnt.py examples/ex.ssqlfin | sed "s/WITH/USE db;\nWITH/" | mysql -h 127.0.0.1 -P 3306 -u rcsql
python3 cnt.py examples/ex.ssqlinf | sed "s/WITH/USE db;\nWITH/" | mysql -h 127.0.0.1 -P 3306 -u rcsql
python3 cnt.py examples/ex.vasqlfin | psql
python3 cnt.py examples/ex.vasqlinf | psql
python3 cnt.py examples/ex.vasqlfin | sed "s/WITH/USE db;\nWITH/" | mysql -h 127.0.0.1 -P 3306 -u rcsql
python3 cnt.py examples/ex.vasqlinf | sed "s/WITH/USE db;\nWITH/" | mysql -h 127.0.0.1 -P 3306 -u rcsql
python3 cnt.py examples/ex.vssqlfin | psql
python3 cnt.py examples/ex.vssqlinf | psql
python3 cnt.py examples/ex.vssqlfin | sed "s/WITH/USE db;\nWITH/" | mysql -h 127.0.0.1 -P 3306 -u rcsql
python3 cnt.py examples/ex.vssqlinf | sed "s/WITH/USE db;\nWITH/" | mysql -h 127.0.0.1 -P 3306 -u rcsql

./test_eval.sh 8 4 2 1 10 10 0 0
./test_ranf.sh 8 4 2 1 10 10 0 0

cat log.txt # the file should only exist if there are errors
