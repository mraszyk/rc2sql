%{

#include "formula.h"
#include "parser.h"
#include "lexer.h"
#include "util.h"

#include <exception>
#include <stdexcept>
#include <vector>

int yyerror(fo **fmla, yyscan_t scanner, const char *msg) {
    throw std::runtime_error("parsing");
}

%}

%code requires {
    typedef void *yyscan_t;
}

%output  "parser.cpp"
%defines "parser.h"
 
%define api.pure
%lex-param   { yyscan_t scanner }
%parse-param { fo **fmla }
%parse-param { yyscan_t scanner }

%union {
    int value;
    fo *fmla;
    std::vector<std::pair<bool, int> > *ts;
}

%token TOKEN_NEG "NEG"
%token TOKEN_CONJ "CONJ"
%token TOKEN_DISJ "DISJ"
%token TOKEN_EXISTS "EXISTS"
%token TOKEN_OPEN "OPEN"
%token TOKEN_CLOSE "CLOSE"

%token TOKEN_P "P"
%token TOKEN_A "A"
%token TOKEN_x "x"

%token <value> TOKEN_NUMBER "NUM"
%token TOKEN_SEP "SEP"
%token TOKEN_DOT "DOT"

%type <fmla> formula
%type <ts> terms

%nonassoc "NEG"
%left "EXISTS"
%left "DISJ"
%left "CONJ"
%nonassoc "OPEN" "CLOSE"

%%
 
input
    : formula { *fmla = $1; }
    ;

formula
    : "P" "NUM" "A" "NUM" "OPEN" terms[ts] "CLOSE"       { $$ = new fo_pred($2, *$ts); assert($ts->size() == $4); delete $ts; }
    | "NEG" formula[f]                                   { $$ = new fo_neg($f); }
    | formula[f] "CONJ" formula[g]                       { $$ = new fo_conj($f, $g); }
    | formula[f] "DISJ" formula[g]                       { $$ = new fo_disj($f, $g); }
    | "EXISTS" "x" "NUM" "DOT" formula[f] %prec "EXISTS" { $$ = new fo_ex($3, $f); }
    | "OPEN" formula[f] "CLOSE"                          { $$ = $f; }
    ;

terms
    : %empty                                             { $$ = new vector<std::pair<bool, int> >(); }
    | "NUM"                                              { $$ = new vector<std::pair<bool, int> >(1, std::make_pair(false, $1)); }
    | "x" "NUM"                                          { $$ = new vector<std::pair<bool, int> >(1, std::make_pair(true, $2)); }
    | terms[ts] "SEP" "NUM"                              { $ts->push_back(std::make_pair(false, $3)); $$ = $ts; }
    | terms[ts] "SEP" "x" "NUM"                          { $ts->push_back(std::make_pair(true, $4)); $$ = $ts; }
 
%%
