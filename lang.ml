type exp =
| EInt of int
| EAdd of exp * exp
| ESubtract of exp * exp
| EMultiplication of exp * exp
| EDivision of exp * exp


let rec interpret (e:exp) : int =
  match e with
  | EInt n                   -> n
  | EAdd (e1, e2)            -> interpret e1 + interpret e2
  | ESubtract (e1, e2)       -> interpret e1 - interpret e2
  | EMultiplication (e1, e2) -> interpret e1 * interpret e2
  | EDivision (e1, e2)       -> interpret e1 / interpret e2
