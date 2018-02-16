type exp =
| EInt of int
| EAdd of exp * exp
| ESubtract of exp * exp
| EMultiplication of exp * exp
| EDivision of exp * exp
| ELeq of exp * exp
| EBoolean of bool
| Eif of exp * exp * exp


let rec interpret (e:exp) : int =
  match e with
  | EInt n                   -> n
  | EAdd (e1, e2)            -> interpret e1 + interpret e2
  | ESubtract (e1, e2)       -> interpret e1 - interpret e2
  | EMultiplication (e1, e2) -> interpret e1 * interpret e2
  | EDivision (e1, e2)       -> interpret e1 / interpret e2
  | Eif (e1, e2, e3)         -> (match interpretBool e1 with
                                | true  -> interpret e2
                                | false -> interpret e3)
  | _                        -> failwith ("unexpected expression type")
and interpretBool (e:exp) : bool =
  match e with
  | ELeq (e1, e2) -> interpret e1 <= interpret e2
  | EBoolean b    -> b
  | _             -> failwith ("unexpected expression type")

(*let rec interpretIf (e:exp) : int =
  | match e with
  | Eif (e1, e2, e3) -> match e1 with
                        | ELeq (e1, e2)
                        | EBoolean b
                        | *)