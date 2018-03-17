%{
  open Lang
%}

%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token PLUS MINUS MULTIPLY DIVIDE
%token LEQ LESS GEQ GREAT EQUAL
%token LPAREN RPAREN
%token IF THEN ELSE
%token COLON TINT TBOOL
%token LET EQ IN
%token FIX FUNC ARROW
%token COMMA FST SND
%token LBRK RBRK DCOLON HD TL EMPTY TLIST
%token REF COLONEQ EXC SCOLON
%token WHILE DO END
%token NEW ARRAY
%token EOF

%left IN ARROW
%right SCOLON
%left ELSE
%left LEQ LESS GEQ GREAT EQUAL
%right DCOLON
%left PLUS MINUS
%left MULTIPLY DIVIDE
%nonassoc LPAREN LBRK
%nonassoc FST SND HD TL EMPTY REF
%nonassoc EXC

%start <Lang.exp> prog

%%

prog:
  | EOF                                 { End }
  | e=exp EOF                           { e }

exp:
  | LPAREN RPAREN                                                         { EUnit}
  | b=BOOL                                                                { EBoolean b }
  | n=INT                                                                 { EInt n }
  | x=VAR                                                                 { EVar x }
  | LPAREN e1=exp RPAREN                                                  { e1 }
  | e1=exp PLUS e2=exp                                                    { EOp (EAdd, e1, e2) }
  | e1=exp MINUS e2=exp                                                   { EOp (ESubtract, e1, e2) }
  | e1=exp MULTIPLY e2=exp                                                { EOp (EMultiplication, e1, e2) }
  | e1=exp DIVIDE e2=exp                                                  { EOp (EDivision, e1, e2) }
  | e1=exp LEQ e2=exp                                                     { EOp (ELeq, e1, e2) }
  | e1=exp LESS e2=exp                                                    { EOp (ELess, e1, e2) }
  | e1=exp GEQ e2=exp                                                     { EOp (EGeq, e1, e2) }
  | e1=exp GREAT e2=exp                                                   { EOp (EGreat, e1, e2) }
  | e1=exp EQUAL e2=exp                                                   { EOp (EEqual, e1, e2) }
  | IF e1=exp THEN e2=exp ELSE e3=exp                                     { EIf (e1, e2, e3) }
  | LET x=VAR COLON t=typ EQ e1=exp IN e2=exp                             { ELet (x, t, e1, e2) }
  | FUNC LPAREN x=VAR COLON t1=typ RPAREN COLON t2=typ ARROW e1=exp       { EFunc (x, t1, t2, e1) }
  | FIX f=VAR LPAREN x=VAR COLON t1=typ RPAREN COLON t2=typ ARROW e1=exp  { EFix (f, x, t1, t2, e1) }
  | e1=exp LPAREN e2=exp RPAREN                                           { EApp (e1, e2) }
  | LPAREN e1=exp COMMA e2=exp RPAREN                                     { EPair (e1, e2) }
  | FST e1=exp                                                            { EFst e1 }
  | SND e1=exp                                                            { ESnd e1 }
  | LBRK RBRK COLON t1=typ %prec DCOLON                                   { EList t1 }
  | e1=exp DCOLON e2=exp                                                  { ECons (e1, e2) }
  | HD e1=exp                                                             { EHd (e1) }
  | TL e1=exp                                                             { ETl (e1) }
  | EMPTY e1=exp                                                          { EEmpty (e1) }
  | REF e1=exp                                                            { ERef e1 }
  | e1=exp COLONEQ e2=exp                                                 { EAsn (e1, e2) }
  | EXC e1=exp                                                            { EDeref e1 }
  | e1=exp SCOLON e2=exp                                                  { EScol (e1, e2) }
  | WHILE e1=exp DO e2=exp END                                            { EWhile (e1, e2) }
  | NEW t=typ LBRK e1=exp RBRK                                            { EArr (t, e1) }
  | e1=exp LBRK e2=exp RBRK                                               { EArac (e1, e2) }

  typ:
    | TINT                          { TInt }
    | TBOOL                         { TBoolean }
    | LPAREN t=typ RPAREN           { t }
    | t1=typ ARROW t2=typ           { TFunc (t1, t2) }
    | t1=typ MULTIPLY t2=typ        { TPair (t1, t2) }
    | t=typ TLIST                   { TList t }
    | LESS t=typ GREAT              { TRef t }
    | ARRAY LESS t=typ GREAT        { TArr t }
