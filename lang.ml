type exp =
| EInt of int
| EAdd of exp * exp

let rec interpret (e:exp) : int =
  match e with
  | EInt n        -> n
  | EAdd (e1, e2) -> interpret e1 + interpret e2
