all: cmp db2csv gen_test

cmp: cmp.cpp
	g++ -o cmp -O2 cmp.cpp

db2csv: db2csv.cpp
	g++ -o db2csv -O2 db2csv.cpp

gen_test: gen_test.cpp formula.h
	g++ -o gen_test -O2 gen_test.cpp
