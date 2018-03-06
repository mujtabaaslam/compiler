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
  | b=BOOL                              { EBoolean b }
  | n=INT                               { EInt n }
  | e1=exp PLUS e2=exp                  { EAdd (e1, e2) }
  | e1=exp MINUS e2=exp                 { ESubtract (e1, e2) }
  | e1=exp MULTIPLY e2=exp              { EMultiplication (e1, e2) }
  | e1=exp DIVIDE e2=exp                { EDivision (e1, e2) }
  | e1=exp LEQ e2=exp                   { ELeq (e1, e2) }
  | IF e1=exp THEN e2=exp ELSE e3=exp   { Eif (e1, e2, e3) }
  | LPAREN e1=exp RPAREN                { (match e1 with
                                          | ELeq (_, _)   -> Bexp (e1)
                                          | EBoolean b    -> Bexp (e1)
                                          | _             -> Eexp (e1))}
