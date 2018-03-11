%{
open Lang
%}

%token <int> INT
%token <bool> BOOL
%token PLUS MINUS MULTIPLY DIVIDE LEQ
%token LPAREN RPAREN
%token IF THEN ELSE
%token EOF

%nonassoc ELSE
%nonassoc LEQ
%left PLUS MINUS
%left MULTIPLY DIVIDE


%start <Lang.exp> prog

%%

prog:
  | e=exp EOF                           { e }

exp:
  | b=BOOL                              { EBoolean b }
  | n=INT                               { EInt n }
  | e1=exp PLUS e2=exp                  { EOp (EAdd, e1, e2) }
  | e1=exp MINUS e2=exp                 { EOp (ESubtract, e1, e2) }
  | e1=exp MULTIPLY e2=exp              { EOp (EMultiplication, e1, e2) }
  | e1=exp DIVIDE e2=exp                { EOp (EDivision, e1, e2) }
  | e1=exp LEQ e2=exp                   { EOp (ELeq, e1, e2) }
  | IF e1=exp THEN e2=exp ELSE e3=exp   { EIf (e1, e2, e3) }
  | LPAREN e1=exp RPAREN                { e1 }
