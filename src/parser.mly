%{
open Lang
%}

%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token PLUS MINUS MULTIPLY DIVIDE LEQ LESS GEQ GREAT EQUAL
%token LPAREN RPAREN
%token IF THEN ELSE
%token LET EQ IN
%token FIX FUNC ARROW
%token EOF

%nonassoc ELSE IN ARROW
%nonassoc LEQ LESS GEQ GREAT EQUAL
%left PLUS MINUS
%left MULTIPLY DIVIDE
%nonassoc LPAREN

%start <Lang.exp> prog

%%

prog:
  | e=exp EOF                           { e }

exp:
  | b=BOOL                              { EBoolean b }
  | n=INT                               { EInt n }
  | x=VAR                               { EVar x }
  | e1=exp PLUS e2=exp                  { EOp (EAdd, e1, e2) }
  | e1=exp MINUS e2=exp                 { EOp (ESubtract, e1, e2) }
  | e1=exp MULTIPLY e2=exp              { EOp (EMultiplication, e1, e2) }
  | e1=exp DIVIDE e2=exp                { EOp (EDivision, e1, e2) }
  | e1=exp LEQ e2=exp                   { EOp (ELeq, e1, e2) }
  | e1=exp LESS e2=exp                  { EOp (ELess, e1, e2) }
  | e1=exp GEQ e2=exp                   { EOp (EGeq, e1, e2) }
  | e1=exp GREAT e2=exp                 { EOp (EGreat, e1, e2) }
  | e1=exp EQUAL e2=exp                 { EOp (EEqual, e1, e2) }
  | IF e1=exp THEN e2=exp ELSE e3=exp   { EIf (e1, e2, e3) }
  | LET x=VAR EQ e1=exp IN e2=exp       { ELet (x, e1, e2) }
  | FUNC x=VAR ARROW e1=exp             { EFunc (x, e1) }
  | FIX f=VAR x=VAR ARROW e1=exp        { EFix (f, x, e1) }
  | e1=exp LPAREN e2=exp RPAREN         { EApp (e1, e2) }
  | LPAREN e1=exp RPAREN                { e1 }
