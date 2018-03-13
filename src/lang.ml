open Printf

type op = EAdd | ESubtract | EMultiplication | EDivision | ELeq | ELess | EGeq | EGreat | EEqual

type exp =
| EInt of int
| EBoolean of bool
| EVar of string
| EOp of op * exp * exp
| EIf of exp * exp * exp
| ELet   of string * exp * exp
| EFunc  of string * exp
| EFix   of string * string * exp
| EApp  of exp * exp

let error err_msg =
  fprintf stderr "Error: %s\n" err_msg; exit 1

let rec string_of_exp (e:exp) : string =
  match e with
  | EOp (o, e1, e2)          -> string_of_op o e1 e2
  | EIf (e1, e2, e3)         -> sprintf "if %s then %s else %s" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
  | EBoolean b               -> string_of_bool b
  | EInt n                   -> string_of_int n
  | EVar x                   -> x
  | ELet (x, v, e1)          -> sprintf "let %s = %s in %s" x (string_of_exp v) (string_of_exp e1)
  | EFunc (x, e1)            -> sprintf "fun %s -> %s" x (string_of_exp e1)
  | EFix (f, x, e1)          -> sprintf "fix %s %s -> %s" f x (string_of_exp e1)
  | EApp (e1, e2)            -> sprintf "%s (%s)" (string_of_exp e1) (string_of_exp e2)

and string_of_op (o:op) (e1:exp) (e2:exp) : string =
match o with
| EAdd             -> sprintf "%s + %s" (string_of_exp e1) (string_of_exp e2)
| ESubtract        -> sprintf "%s - %s" (string_of_exp e1) (string_of_exp e2)
| EMultiplication  -> sprintf "%s * %s" (string_of_exp e1) (string_of_exp e2)
| EDivision        -> sprintf "%s / %s" (string_of_exp e1) (string_of_exp e2)
| ELeq             -> sprintf "%s <= %s" (string_of_exp e1) (string_of_exp e2)
| ELess            -> sprintf "%s < %s" (string_of_exp e1) (string_of_exp e2)
| EGeq             -> sprintf "%s >= %s" (string_of_exp e1) (string_of_exp e2)
| EGreat           -> sprintf "%s > %s" (string_of_exp e1) (string_of_exp e2)
| EEqual           -> sprintf "%s == %s" (string_of_exp e1) (string_of_exp e2)

let rec subst (v:exp) (x:string) (e:exp) : exp =
  let sub expr = subst v x expr in
  match e with
  | EOp (o, e1, e2)                         -> EOp (o, sub e1, sub e2)
  | EIf (e1, e2, e3)                        -> EIf (sub e1, sub e2, sub e3)
  | EApp (e1, e2)                           -> EApp (sub e1, sub e2)
  | ELet (x', e1, e2) when x <> x'          -> ELet (x', sub e1, sub e2)
  | EFunc (x', e') when x <> x'             -> EFunc (x', sub e')
  | EFix (f, x', e') when x <> x' && x <> f -> EFunc (x', sub e')
  | EVar x' when x = x'                     -> v
  | _ as e                                  -> e


let rec interpret (e:exp) : exp =
  match e with
  | EOp (o, e1, e2)  -> interpretOp o e1 e2
  | EIf (e1, e2, e3) -> interpretIf e1 e2 e3
  | ELet (x, e1, e2) -> interpretLet x e1 e2
  | EApp (e1, e2)    -> interpretApp e1 e2
  | EVar x           -> error (sprintf "No value found for variable '%s'" x)
  | _ as e           -> e

  and interpretIf (e1:exp) (e2:exp) (e3:exp) : exp =
    match e1 with
    | EBoolean b               -> if b then interpret e2 else interpret e3
    | EOp(ELeq, _, _) as e1    -> interpret (EIf ((interpret e1), e2, e3))
    | EOp(ELess, _, _) as e1   -> interpret (EIf ((interpret e1), e2, e3))
    | EOp(EGeq, _, _) as e1    -> interpret (EIf ((interpret e1), e2, e3))
    | EOp(EGreat, _, _) as e1  -> interpret (EIf ((interpret e1), e2, e3))
    | EOp(EEqual, _, _) as e1  -> interpret (EIf ((interpret e1), e2, e3))
    | _ -> error (sprintf "Expected a boolean expression got %s" (string_of_exp e1))

  and interpretLet (x:string) (e1:exp) (e2:exp) =
    let v1 = interpret e1 in interpret (subst v1 x e2)

  and interpretApp (e1:exp) (e2:exp) : exp =
    let f = interpret e1 in
    let v2 = interpret e2 in
    match f with
    | EFunc (x, e3)    -> interpret (subst v2 x e3)
    | EFix (f', x, e3) -> interpret (subst f f' (subst v2 x e3))
    | _ -> error (sprintf "Expected a function, got %s" (string_of_exp e1))

  and interpretOp (o:op) (e1:exp) (e2:exp) : exp =
    let v1 = interpret e1 in
    let v2 = interpret e2 in
    match (v1, v2) with
    | (EInt n1, EInt n2) -> interpretInt o n1 n2
    | _ -> error (sprintf "Expected 2 numeric expressions, got %s and %s"
                  (string_of_exp e1) (string_of_exp e2))

 and interpretInt (o:op) (v1:int) (v2:int) : exp =
    match o with
    | EAdd            -> EInt (v1 + v2)
    | ESubtract       -> EInt (v1 - v2)
    | EMultiplication -> EInt (v1 * v2)
    | EDivision       -> EInt (v1 / v2)
    | ELeq            -> EBoolean (v1 <= v2)
    | ELess           -> EBoolean (v1 < v2)
    | EGeq            -> EBoolean (v1 >= v2)
    | EGreat          -> EBoolean (v1 > v2)
    | EEqual          -> EBoolean (v1 = v2)

let is_value (e:exp) : bool =
  match e with
   | EInt _ | EBoolean _
   | EFunc (_, _) | EFix (_, _, _) -> true
   | _                             -> false

let rec step (e:exp) : exp =
  match e with
  | EOp (o, e1, e2)  -> stepOp o e1 e2
  | EIf (e1, e2, e3) -> stepIf e1 e2 e3
  | ELet (x, e1, e2) -> stepLet x e1 e2
  | EApp (e1, e2)    -> stepApp e1 e2
  | EVar x           -> error (sprintf "Empty variable '%s'" x)
  | _ as e           -> e
   and stepOp (o:op) (e1:exp) (e2:exp) : exp =
     if is_value e1 && is_value e2 then interpretOp o e1 e2
     else if is_value e1 then EOp (o, e1, step e2)
     else EOp (o, step e1, e2)
   and stepIf (e1:exp) (e2:exp) (e3:exp) : exp =
     if is_value e1 then
       match e1 with
       | EBoolean b -> if b then step e2 else step e3
       | _          -> error (sprintf "Expected a boolean expression got %s" (string_of_exp e1))
     else EIf (step e1, e2, e3)
   and stepLet (x:string) (e1:exp) (e2:exp) : exp =
     if is_value e1 then subst e1 x e2 else ELet (x, step e1, e2)
   and stepApp (e1:exp) (e2:exp) : exp =
     if is_value e1 && is_value e2 then
       match e1 with
       | EFunc (x, e3)   -> subst e2 x e3
       | EFix (f, x, e3) -> subst e1 f (subst e2 x e3)
       | _ -> error (sprintf "Expected a function, got %s" (string_of_exp e1))
     else if is_value e1 then EApp (e1, step e2)
     else EApp (step e1, e2)

let rec step_interpret (e:exp) =
  if is_value e then
    sprintf "-> %s" (string_of_exp e) |> print_endline
  else begin
    sprintf "-> %s" (string_of_exp e) |> print_endline;
    step e |> step_interpret
    end
