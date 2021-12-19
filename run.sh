. ./functions.sh

./exps_small.sh 14 4 2 1 500 2 "0 1 2 3 4 5 6 7 8 9" > exps_small.tex
./exps_medium.sh 14 4 2 1 20000 2 "0 1 2 3 4 5 6 7 8 9" > exps_medium.tex
./exps_large.sh 14 4 2 1 "40000 80000 120000" 2 "0 1 2 3 4 5 6 7 8 9" > exps_large.tex
./exps_inf.sh 0 4 2 0 4000 2 "14 30 64 108 184 15 31 65 109 185" > exps_inf.tex
./exps_susp.sh > exps_susp.tex

#./bold.sh
