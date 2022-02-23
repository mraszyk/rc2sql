%token LPA RPA COM MINUS
%token <Z.t> CST
%token <string> PRED
%token EOF

%type <(string * (Verified.Monitor.event_data list)) list> db
%start db

%%

db:
  | db=onedb EOF              { db }

onedb:
  | PRED LPA fields RPA onedb { ($1, List.map (fun n -> Verified.Monitor.EInt n) $3) :: $5 }
  |                           { [] }

number:
  | n=CST                     { n }
  | MINUS n=CST               { Z.neg n }

fields:
  | n=number COM fields       { n :: $3 }
  | n=number                  { [n] }
  |                           { [] }
