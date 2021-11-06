%token COM DOT EQ LPA RPA
%token NEG CONJ DISJ
%token EXISTS FORALL
%token <string> ID
%token <int> CST
%token EOF

%right EXISTS FORALL
%left DISJ
%left CONJ
%nonassoc NEG

%type <int FO.FO.fmla> formula
%start formula

%%

formula:
  | f=f EOF { f }

f:
  | LPA f=f RPA                           { f }
  | ID LPA termlist RPA                   { FO.FO.Pred ($1, $3) }
  | ID EQ t=term                          { FO.FO.Eq ($1, t) }
  | NEG f=f                               { FO.FO.Neg f }
  | f=f CONJ g=f                          { FO.FO.Conj (f, g) }
  | f=f DISJ g=f                          { FO.FO.Disj (f, g) }
  | EXISTS ID DOT f=f %prec EXISTS        { FO.FO.Exists ($2, f) }
  | FORALL ID DOT f=f %prec FORALL        { FO.FO.Neg (FO.FO.Exists ($2, FO.FO.Neg f)) }


termlist:
  | term COM termlist       { $1 :: $3 }
  | term                    { [$1] }
  |                         { [] }

term:
  | CST                     { FO.FO.Const $1 }
  | ID                      { FO.FO.Var $1 }
