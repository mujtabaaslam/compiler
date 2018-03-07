%{
open Lang
%}

%token <int> INT
%token <bool> BOOL
%token <string> VAR

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
%token LET        (* let *)
%token EQ         (* = *)
%token IN         (* in *)
%token FIX        (* fix *)
%token FUNC       (* fun *)
%token ARROW      (* -> *)

%token EOF

%start <Lang.exp> prog

%%

prog:
  | e=exp EOF                           { e }

exp:
  | b=BOOL                              { EBoolean b }
  | n=INT                               { EInt n }
  | x=VAR                               { EVar x }
  | LPAREN e1=exp RPAREN                { e1 }
  | e1=exp PLUS e2=exp                  { EOp (EAdd, e1, e2) }
  | e1=exp MINUS e2=exp                 { EOp (ESubtract, e1, e2) }
  | e1=exp MULTIPLY e2=exp              { EOp (EMultiplication, e1, e2) }
  | e1=exp DIVIDE e2=exp                { EOp (EDivision, e1, e2) }
  | e1=exp LEQ e2=exp                   { EOp (ELeq, e1, e2) }
  | IF e1=exp THEN e2=exp ELSE e3=exp   { EIf (e1, e2, e3) }
  | LET x=VAR EQ e1=exp IN e2=exp       { ELet (x, e1, e2) }
  | FUNC x=VAR ARROW e1=exp             { EFunc (x, e1) }
  | FIX f=VAR x=VAR ARROW e1=exp        { EFix (f, x, e1) }
  | e1=exp LPAREN e2=exp RPAREN         { EApp (e1, e2) }
