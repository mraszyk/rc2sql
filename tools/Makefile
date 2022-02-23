FILES = *.cpp
HEADERS = *.h
CC = g++
CFLAGS = -std=c++17 -O3 -Wno-unused-result

all: cmp db2csv gen_test dg

cmp: cmp.cpp
	$(CC) $(CFLAGS) -o cmp cmp.cpp

db2csv: db2csv.cpp
	$(CC) $(CFLAGS) -o db2csv db2csv.cpp

gen_test: gen_test.cpp formula.h formula.cpp
	$(CC) $(CFLAGS) -o gen_test gen_test.cpp formula.cpp

dg: dg.cpp formula.h formula.cpp util.h util.cpp lexer.cpp parser.cpp
	$(CC) $(CFLAGS) -o dg formula.cpp util.cpp lexer.cpp parser.cpp dg.cpp

lexer.cpp: lexer.l
	flex lexer.l

parser.cpp: parser.y lexer.cpp
	bison parser.y
