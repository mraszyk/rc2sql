{
open Lexing
open Db_parser
open Verified
}

let blank = [' ' '\t']+
let newline = ['\r' '\n'] | "\r\n"
let num = ['0'-'9']+
let alpha = ['a'-'z' 'A'-'Z']
let alphanums = ['a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
  | newline                                       { Lexing.new_line lexbuf; token lexbuf }
  | blank                                         { token lexbuf }
  | "("                                           { LPA }
  | ")"                                           { RPA }
  | ","                                           { COM }
  | num as n                                      { CST (Eval_FO.nat_of_integer (Z.of_string n)) }
  | (alpha alphanums) as name                     { PRED name }
  | eof                                           { EOF }
  | _                                             { failwith "Illegal character in database file." }
