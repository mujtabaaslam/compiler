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
%token THEN       (* then *)
%token ELSE       (* else *)



%token EOF

%start <Lang.exp> prog

%%

prog:
  | e=exp EOF                           { e }

exp:
  | b=BOOL                                    { EBoolean b }
  | n=INT                                     { EInt n }
  | LPAREN e1=exp PLUS e2=exp RPAREN          { EAdd (e1, e2) }
  | LPAREN e1=exp MINUS e2=exp RPAREN         { ESubtract (e1, e2) }
  | LPAREN e1=exp MULTIPLY e2=exp RPAREN      { EMultiplication (e1, e2) }
  | LPAREN e1=exp DIVIDE e2=exp RPAREN        { EDivision (e1, e2) }
  | LPAREN e1=exp LEQ e2=exp RPAREN           { ELeq (e1, e2) }
  | LPAREN IF e1=exp THEN e2=exp ELSE e3=exp RPAREN     { Eif (e1, e2, e3) }
