%{
open Lang
%}

%token <int> INT
%token <bool> BOOL

%token LPAREN     (* ( *)
%token RPAREN     (* ) *)
%token PLUS       (* + *)
%token MINUS      (* - *)
%token MULTIPLY   (* * *)
%token DIVIDE     (* / *)
%token LEQ        (* <= *)
%token IF         (* if *)



%token EOF

%start <Lang.exp> prog

%%

prog:
  | e=exp EOF                           { e }

exp:
  | b=BOOL                                    { EBoolean b }
  | n=INT                                     { EInt n }
  | LPAREN PLUS e1=exp e2=exp RPAREN          { EAdd (e1, e2) }
  | LPAREN MINUS e1=exp e2=exp RPAREN         { ESubtract (e1, e2) }
  | LPAREN MULTIPLY e1=exp e2=exp RPAREN      { EMultiplication (e1, e2) }
  | LPAREN DIVIDE e1=exp e2=exp RPAREN        { EDivision (e1, e2) }
  | LPAREN LEQ e1=exp e2=exp RPAREN           { ELeq (e1, e2) }
  | LPAREN IF e1=exp e2=exp e3=exp RPAREN     { Eif (e1, e2, e3) }
