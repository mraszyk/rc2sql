{
open Lexing
open Fo_parser
}

let blank = [' ' '\r' '\n' '\t']
let num = ['0'-'9']+
let alpha = ['a'-'z' 'A'-'Z']
let alphanums = ['a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
  | blank                                         { token lexbuf }
  | ","                                           { COM }
  | "."                                           { DOT }
  | "-"                                           { MINUS }
  | "("                                           { LPA }
  | ")"                                           { RPA }
  | "="                                           { EQ }
  | "TRUE"                                        { TRUE }
  | "FALSE"                                       { FALSE }
  | "NOT"                                         { NEG }
  | "AND"                                         { CONJ }
  | "OR"                                          { DISJ }
  | "IMPLIES"                                     { IMPLIES }
  | "EXISTS"                                      { EXISTS }
  | "FORALL"                                      { FORALL }
  | (alpha alphanums) as name                     { ID name }
  | num as n                                      { CST (int_of_string n) }
  | eof                                           { EOF }
  | _                                             { failwith "unexpected character" }
