to=600
real=10

ulimit -s 1048576

. "./functions.sh"

prefix="/home/rcsql"

seeds="6 7 8 9 10 11"

./amazon/gen_test "/home/rcsql/z_6" 1 0 "${real}" 2 1 0
./amazon/gen_test "/home/rcsql/z_7" 1 0 "${real}" 2 1 0
./amazon/gen_test "/home/rcsql/z_8" 1 1 "${real}" 2 1 0
./amazon/gen_test "/home/rcsql/z_9" 1 1 "${real}" 2 1 0
./amazon/gen_test "/home/rcsql/z_10" 2 0 "${real}" 2 1 0
./amazon/gen_test "/home/rcsql/z_11" 2 0 "${real}" 2 1 0
cp /home/rcsql/z_11.sig /home/rcsql/z_6.sig
cp /home/rcsql/z_11.sig /home/rcsql/z_7.sig
cp /home/rcsql/z_11.sig /home/rcsql/z_8.sig
cp /home/rcsql/z_11.sig /home/rcsql/z_9.sig

for i in 6 8 10
do
  ln -sf amazon/Gift_Cards.db z_${i}.db
  ln -sf amazon/Gift_Cards.log z_${i}.log
  ln -sf amazon/init_gift.psql z_${i}.psql
  ln -sf amazon/init_gift.msql z_${i}.msql
done

for i in 7 9 11
do
  ln -sf amazon/Musical_Instruments.db z_${i}.db
  ln -sf amazon/Musical_Instruments.log z_${i}.log
  ln -sf amazon/init_music.psql z_${i}.psql
  ln -sf amazon/init_music.msql z_${i}.msql
done

for i in {6..11}
do
  init > /dev/null
done

line "\\tool" run01APSQL
line "\\toolnonopt" run01SPSQL

line "\\vgtool" run02APSQL
line "\\vgtoolnonopt" run02SPSQL

echo "\\hline"

#line "\\ail" run03
line "\\ddd" run04DDD
line "\\ldd" run04LDD
line "\\mpreg" run05
