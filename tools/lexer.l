%{

#include "formula.h"
#include "parser.h"
#include "util.h"

#include <exception>
#include <stdexcept>

%}

%option outfile="lexer.cpp" header-file="lexer.h"
%option warn nodefault

%option reentrant noyywrap never-interactive nounistd
%option bison-bridge

OPEN        \(
CLOSE       \)
 
INTOPEN     \[
INTCLOSE    \]

NUMBER      [0-9]*
ALPHA       [a-zA-Z]
ALPHANUMS   [a-zA-Z0-9]*
WS          [ \r\n\t]*
 
%%
 
{WS}                { continue; }
NOT                 { return TOKEN_NEG; }
AND                 { return TOKEN_CONJ; }
OR                  { return TOKEN_DISJ; }
EXISTS              { return TOKEN_EXISTS; }
{OPEN}              { return TOKEN_OPEN; }
{CLOSE}             { return TOKEN_CLOSE; }

P                   { return TOKEN_P; }
A                   { return TOKEN_A; }
x                   { return TOKEN_x; }
{NUMBER}            { if (parseNumber(yytext, NULL, &yylval->value)) throw std::runtime_error("integer bounds"); return TOKEN_NUMBER; }
,                   { return TOKEN_SEP; }
\.                  { return TOKEN_DOT; }

.                   { throw std::runtime_error("unexpected character"); }
 
%%
