open Printf

type op = EAdd | ESubtract | EMultiplication | EDivision | ELeq

type exp =
| EInt of int
| EBoolean of bool
| EOp of op * exp * exp
| EIf of exp * exp * exp

let error err_msg =
  fprintf stderr "Error: %s\n" err_msg; exit 1

let rec string_of_exp (e:exp) : string =
  match e with
  | EOp (o, e1, e2)          -> string_of_op o e1 e2
  | EIf (e1, e2, e3)         -> sprintf "if %s then %s else %s" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
  | EBoolean b               -> string_of_bool b
  | EInt n                   -> string_of_int n

and string_of_op (o:op) (e1:exp) (e2:exp) : string =
match o with
| EAdd             -> sprintf "%s + %s" (string_of_exp e1) (string_of_exp e2)
| ESubtract        -> sprintf "%s - %s" (string_of_exp e1) (string_of_exp e2)
| EMultiplication  -> sprintf "%s * %s" (string_of_exp e1) (string_of_exp e2)
| EDivision        -> sprintf "%s / %s" (string_of_exp e1) (string_of_exp e2)
| ELeq             -> sprintf "%s <= %s" (string_of_exp e1) (string_of_exp e2)


let rec interpret (e:exp) : exp =
  match e with
  | EOp (o, e1, e2)  -> interpretOp o e1 e2
  | EIf (e1, e2, e3) -> interpretIf e1 e2 e3
  | _ as e           -> e

  and interpretIf (e1:exp) (e2:exp) (e3:exp) : exp =
    match e1 with
    | EBoolean b             -> if b then interpret e2 else interpret e3
    | EOp(ELeq, _, _) as e1  -> interpret (EIf ((interpret e1), e2, e3))
    | _ -> error (sprintf "Expected a boolean expr for the 1st sub-expr of 'if'-expr, got %s" (string_of_exp e1))

  and interpretOp (o:op) (e1:exp) (e2:exp) : exp =
    let v1 = interpret e1 in
    let v2 = interpret e2 in
    match (v1, v2) with
    | (EInt n1, EInt n2) -> interpretInt o n1 n2
    | _ -> error (sprintf "Expected 2 numeric sub-exprs for a binary expr, got %s and %s"
                  (string_of_exp e1) (string_of_exp e2))

 and interpretInt (o:op) (v1:int) (v2:int) : exp =
    match o with
    | EAdd            -> EInt (v1 + v2)
    | ESubtract       -> EInt (v1 - v2)
    | EMultiplication -> EInt (v1 * v2)
    | EDivision       -> EInt (v1 / v2)
    | ELeq            -> EBoolean (v1 <= v2)
