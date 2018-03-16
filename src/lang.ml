open Printf

type op = EAdd | ESubtract | EMultiplication | EDivision | ELeq | ELess | EGeq | EGreat | EEqual

type typ =
  | TInt
  | TBoolean
  | TFunc of typ * typ
  | TUnit
  | TPair of typ * typ
  | TList of typ

type exp =
| EUnit
| EInt of int
| EBoolean of bool
| EVar of string
| EOp of op * exp * exp
| EIf of exp * exp * exp
| ELet of string * typ * exp * exp
| EFunc of string * typ * typ *  exp
| EFix of string * string * typ * typ * exp
| EApp of exp * exp
| EPair of exp * exp
| EFst of exp
| ESnd of exp
| EList of typ
| ECons of exp * exp
| EHd of exp
| ETl of exp
| EEmpty of exp

let error (msg:string) =
  fprintf stderr "Error: %s\n" msg; exit 1

let rec string_of_typ (t:typ) : string =
  match t with
  | TInt           -> "int"
  | TBoolean       -> "bool"
  | TUnit          -> "unit"
  | TFunc (t1, t2) -> sprintf "(%s -> %s)" (string_of_typ t1) (string_of_typ t2)
  | TPair (t1, t2) -> sprintf "(%s * %s)" (string_of_typ t1) (string_of_typ t2)
  | TList t        -> string_of_typ t ^ " list"

let rec string_of_exp (e:exp) : string =
  match e with
  | EOp (o, e1, e2)          -> string_of_op o e1 e2
  | EIf (e1, e2, e3)         -> sprintf "if %s then %s else %s" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
  | EBoolean b               -> string_of_bool b
  | EInt n                   -> string_of_int n
  | EVar x                   -> x
  | EUnit                    -> "()"
  | ELet (x, t, v, e1)       -> sprintf "let %s : %s = %s in %s" x (string_of_typ t) (string_of_exp v) (string_of_exp e1)
  | EFunc (x, t1, t2, e1)    -> sprintf "fun (%s:%s) : %s -> %s" x (string_of_typ t1) (string_of_typ t2) (string_of_exp e1)
  | EFix (f, x, t1, t2, e1)  -> sprintf "fix %s (%s:%s) : %s -> %s" f x (string_of_typ t1) (string_of_typ t2) (string_of_exp e1)
  | EApp (e1, e2)            -> sprintf "%s (%s)" (string_of_exp e1) (string_of_exp e2)
  | EPair (e1, e2)           -> sprintf "(%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | EFst e1                  -> sprintf "(fst %s)" (string_of_exp e1)
  | ESnd e1                  -> sprintf "(snd %s)" (string_of_exp e1)
  | EList t                  -> sprintf "[] : %s" (string_of_typ t)
  | ECons (e1, e2)           -> string_of_cons e1 e2
  | EHd e1                   -> sprintf "(hd %s)" (string_of_exp e1)
  | ETl e1                   -> sprintf "(tl %s)" (string_of_exp e1)
  | EEmpty e1                -> sprintf "(empty %s)" (string_of_exp e1)
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
and string_of_cons (e1:exp) (e2:exp) : string =
  let str  =
    match e2 with
    | EList t -> sprintf "[] : %s" (string_of_typ t)
    | ECons (e1, e2) -> string_of_cons e1 e2
    | _ -> error (sprintf "Expected a cons, got %s" (string_of_exp e2))
  in
  sprintf "(%s :: %s)" (string_of_exp e1) str

module Context = Map.Make(String)

let rec typecheck (g:typ Context.t) (e:exp) : typ =
  match e with
  | EUnit       -> TUnit
  | EInt _      -> TInt
  | EBoolean _  -> TBoolean
  | EVar x   ->
    begin try Context.find x g
      with Not_found -> error (sprintf "No assigned type for %s" x)
    end
  | EOp (o, e1, e2) ->
    let t1 = typecheck g e1 in
    let t2 = typecheck g e2 in
    begin match o with
      | EAdd | ESubtract | EMultiplication | EDivision ->
        begin match (t1, t2) with
          | (TInt, TInt) -> TInt
          | _ -> error (sprintf "Int expected in %s, got %s and %s" (string_of_exp e) (string_of_typ t1) (string_of_typ t2))
        end
      | ELeq | ELess | EGeq | EGreat | EEqual ->
        begin match (t1, t2) with
          | (TInt, TInt) -> TBoolean
          | _ -> error (sprintf "Int expected in %s, got %s and %s" (string_of_exp e) (string_of_typ t1) (string_of_typ t2))
        end
    end
  | EIf (e1, e2, e3) ->
    let t1 = typecheck g e1 in
    let t2 = typecheck g e2 in
    let t3 = typecheck g e3 in
    if t1 <> TBoolean then
      error (sprintf "Bool expected in %s, got type %s" (string_of_exp e) (string_of_typ t1))
    else if t2 <> t3 then
      error (sprintf "Same type expected for two subexpressions in %s, got type %s and %s" (string_of_exp e) (string_of_typ t2) (string_of_typ t3))
    else t2
  | ELet (x, t, e1, e2) ->
    let t1 = typecheck g e1 in
    if t1 = t then
      let g = Context.add x t1 g in
      typecheck g e2
    else
      error (sprintf "Type %s expected for variable %s in %s, got type %s" (string_of_typ t) x (string_of_exp e) (string_of_typ t1))
  | EFunc (x, t1, t2, e1) ->
    let g = Context.add x t1 g in
    let t = typecheck g e1 in
    if t = t2 then TFunc (t1, t2)
    else
      error (sprintf "Type %s expected for variable %s in %s, got type %s" (string_of_typ t2) (string_of_exp e1) (string_of_exp e) (string_of_typ t))
  | EFix (f, x, t1, t2, e1) ->
    let g = Context.add f (TFunc (t1, t2)) g in
    let g = Context.add x t1 g in
    let t = typecheck g e1 in
    if t = t2 then TFunc (t1, t2)
    else
      error (sprintf "Type %s expected for variable %s in %s, got type %s" (string_of_typ t2) (string_of_exp e1) (string_of_exp e) (string_of_typ t))
  | EApp (e1, e2) ->
    let t = typecheck g e1 in
    begin match t with
      | TFunc (t1, t3) ->
        let t2 = typecheck g e2 in
        if t2 = t1 then t3
        else
          error (sprintf "Type %s expected for variable %s in %s, got type %s" (string_of_typ t1) (string_of_exp e2) (string_of_exp e) (string_of_typ t2))
          | _ ->  error (sprintf "Function expected for %s in %s, got type %s" (string_of_exp e1) (string_of_exp e) (string_of_typ t))
    end
  | EPair (e1, e2) -> TPair (typecheck g e1, typecheck g e2)
  | EFst e1 ->
    let t = typecheck g e1 in
    begin match t with
      | TPair (t1, _) -> t1
      | _ -> error (sprintf "Pair expected for %s in %s, got %s"
                      (string_of_exp e1) (string_of_exp e) (string_of_typ t))
    end
  | ESnd e1 ->
    let t = typecheck g e1 in
    begin match t with
      | TPair (_, t2) -> t2
      | _ -> error (sprintf "Pair expected for %s in %s, got %s"
                      (string_of_exp e1) (string_of_exp e) (string_of_typ t))
    end
  | EList t -> TList t
  | ECons (e1, e2) ->
    let t1 = typecheck g e1 in
    let t2 = typecheck g e2 in
    let t =
      begin match t2 with
        | TList t -> t
        | _ -> error (sprintf "List expected for %s in %s, got %s"
                        (string_of_exp e1) (string_of_exp e) (string_of_typ t2))
      end
    in
    if t1 = t then t2
    else
      error (sprintf "Expected type %s for %s in %s, got %s"
               (string_of_typ t) (string_of_exp e1) (string_of_exp e) (string_of_typ t1))
  | EHd e1 ->
    let t1 = typecheck g e1 in
    begin match t1 with
      | TList t -> t
      | _ -> error (sprintf "List expected for %s in %s, got %s"
                      (string_of_exp e1) (string_of_exp e) (string_of_typ t1))
    end
  | ETl e1 ->
    let t1 = typecheck g e1 in
    begin match t1 with
      | TList _ -> t1
      | _ -> error (sprintf "List expected for %s in %s, got %s"
                      (string_of_exp e1) (string_of_exp e) (string_of_typ t1))
    end
  | EEmpty e1 ->
    let t1 = typecheck g e1 in
    begin match t1 with
      | TList _ -> TBoolean
      | _ -> error (sprintf "List expected for %s in %s, got %s"
                      (string_of_exp e1) (string_of_exp e) (string_of_typ t1))
    end

let rec sub (v:exp) (x:string) (e:exp) : exp =
  let s exp = sub v x exp in
  match e with
  | EOp (o, e1, e2)                                 -> EOp (o, s e1, s e2)
  | EIf (e1, e2, e3)                                -> EIf (s e1, s e2, s e3)
  | EApp (e1, e2)                                   -> EApp (s e1, s e2)
  | ELet (x1, t, e1, e2) when x <> x1               -> ELet (x1, t, s e1, s e2)
  | EFunc (x1, t1, t2, e1) when x <> x1             -> EFunc (x1, t1, t2, s e1)
  | EFix (f, x1, t1, t2, e1) when x <> x1 && x <> f -> EFix (f, x1, t1, t2, s e1)
  | EVar x1 when x = x1                             -> v
  | EPair (e1, e2)                                  -> EPair (s e1, s e2)
  | EFst e1                                         -> EFst (s e1)
  | ESnd e1                                         -> ESnd (s e1)
  | ECons (e1, e2)                                  -> ECons (s e1, s e2)
  | EHd e1                                          -> EHd (s e1)
  | ETl e1                                          -> ETl (s e1)
  | EEmpty e1                                       -> EEmpty (s e1)
  | _ as e                                          -> e

  let rec is_value (e:exp) : bool =
    match e with
     | EInt _ | EBoolean _ | EUnit
     | EFunc (_, _, _, _) | EFix (_, _, _, _, _)
     | EList _ | ECons (_, _)                    -> true
     | EPair (e1, e2) -> is_value e1 && is_value e2
     | _                                         -> false

let rec interpret (e:exp) : exp =
  match e with
  | EOp (o, e1, e2)     -> interpretOp o e1 e2
  | EIf (e1, e2, e3)    -> interpretIf e1 e2 e3
  | ELet (x, t, e1, e2) -> interpretLet x t e1 e2
  | EApp (e1, e2)       -> interpretApp e1 e2
  | EPair (e1, e2)      -> interpretPair e1 e2
  | EFst e1             -> interpretFst e1
  | ESnd e1             -> interpretSnd e1
  | EHd e1              -> interpretHd e1
  | ETl e1              -> interpretTl e1
  | EEmpty e1           -> interpretEmpty e1
  | EVar x              -> error (sprintf "No value found for variable '%s'" x)
  | _ as e              -> e
and interpretIf (e1:exp) (e2:exp) (e3:exp) : exp =
  match e1 with
  | EBoolean b               -> if b then interpret e2 else interpret e3
  | EOp(ELeq, _, _) as e1    -> interpret (EIf ((interpret e1), e2, e3))
  | EOp(ELess, _, _) as e1   -> interpret (EIf ((interpret e1), e2, e3))
  | EOp(EGeq, _, _) as e1    -> interpret (EIf ((interpret e1), e2, e3))
  | EOp(EGreat, _, _) as e1  -> interpret (EIf ((interpret e1), e2, e3))
  | EOp(EEqual, _, _) as e1  -> interpret (EIf ((interpret e1), e2, e3))
  | EEmpty _ as e1           -> interpret (EIf ((interpret e1), e2, e3))
  | _ -> error (sprintf "Boolean expected, got %s" (string_of_exp e1))
and interpretLet (x:string) (t:typ) (e1:exp) (e2:exp) =
  let v1 = interpret e1 in interpret (sub v1 x e2)
and interpretApp (e1:exp) (e2:exp) : exp =
  let f = interpret e1 in
  let v2 = interpret e2 in
  match f with
  | EFunc (x, t1, t2, e3)    -> interpret (sub v2 x e3)
  | EFix (f1, x, t1, t2, e3) -> interpret (sub f f1 (sub v2 x e3))
  | _ -> error (sprintf "Function expected, got %s" (string_of_exp e1))
and interpretOp (o:op) (e1:exp) (e2:exp) : exp =
  let v1 = interpret e1 in
  let v2 = interpret e2 in
  match (v1, v2) with
  | (EInt n1, EInt n2) -> interpretInt o n1 n2
  | _ -> error (sprintf "Numeric expressions expected, got %s and %s" (string_of_exp e1) (string_of_exp e2))
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
and interpretPair (e1:exp) (e2:exp) : exp =
  if is_value e1 && is_value e2 then EPair (e1, e2)
  else if (is_value e1 = false) && (is_value e2 = false) then EPair (interpret e1, interpret e2)
  else if is_value e1 then EPair (e1, interpret e2)
  else EPair (interpret e1, e2)
and interpretFst (e:exp) : exp =
  if is_value e then
    match e with
    | EPair (e1, _) -> interpret e1
    | _ -> error (sprintf "Pair expected, got %s" (string_of_exp e))
    else EFst (interpret e)
and interpretSnd (e:exp) : exp =
  if is_value e then
    match e with
    | EPair (_, e2) -> interpret e2
    | _ -> error (sprintf "Pair expected, got %s" (string_of_exp e))
  else ESnd (interpret e)
and interpretHd (e:exp) : exp =
  if is_value e then
    match e with
    | ECons (e1, _) -> interpret e1
    | _ -> error (sprintf "List expected, got %s" (string_of_exp e))
  else EHd (interpret e)
and interpretTl (e:exp) : exp =
  if is_value e then
    match e with
    | ECons (_, e2) -> interpret e2
    | _ -> error (sprintf "List expected, got %s" (string_of_exp e))
  else ETl (interpret e)
and interpretEmpty (e:exp) : exp =
  if is_value e then
    match e with
    | EList _       -> EBoolean true
    | ECons (_, _)  -> EBoolean false
    | _ -> error (sprintf "List expected, got %s" (string_of_exp e))
  else EEmpty (interpret e)

let rec step (e:exp) : exp =
  match e with
  | EOp (o, e1, e2)         -> stepOp o e1 e2
  | EIf (e1, e2, e3)        -> stepIf e1 e2 e3
  | ELet (x, t, e1, e2)     -> stepLet x t e1 e2
  | EApp (e1, e2)           -> stepApp e1 e2
  | EPair (e1, e2)          -> stepPair e1 e2
  | EFst e1                 -> stepFst e1
  | ESnd e2                 -> stepSnd e2
  | EHd e1                  -> stepHd e1
  | ETl e1                  -> stepTl e1
  | EEmpty e1               -> stepEmpty e1
  | EVar x                  -> error (sprintf "Empty variable '%s'" x)
  | _ as e                  -> e
and stepOp (o:op) (e1:exp) (e2:exp) : exp =
  if is_value e1 && is_value e2 then interpretOp o e1 e2
  else if is_value e1 then EOp (o, e1, step e2)
  else EOp (o, step e1, e2)
and stepIf (e1:exp) (e2:exp) (e3:exp) : exp =
  if is_value e1 then
    match e1 with
    | EBoolean b -> if b then step e2 else step e3
    | _          -> error (sprintf "Boolean expected, got %s" (string_of_exp e1))
   else EIf (step e1, e2, e3)
and stepLet (x:string) (t:typ) (e1:exp) (e2:exp) : exp =
  if is_value e1 then sub e1 x e2 else ELet (x, t, step e1, e2)
and stepApp (e1:exp) (e2:exp) : exp =
  if is_value e1 && is_value e2 then
    match e1 with
    | EFunc (x, t1, t2, e3)   -> sub e2 x e3
    | EFix (f, x, t1, t2, e3) -> sub e1 f (sub e2 x e3)
    | _ -> error (sprintf "Function expected, got %s" (string_of_exp e1))
  else if is_value e1 then EApp (e1, step e2)
  else EApp (step e1, e2)
and stepPair (e1:exp) (e2:exp) : exp =
  if is_value e1 && is_value e2 then EPair (e1, e2)
  else if is_value e1 then EPair (e1, step e2)
  else EPair (step e1, e2)
and stepFst (e:exp) : exp =
  if is_value e then
    match e with
    | EPair (e1, _) -> step e1
    | _ -> error (sprintf "Pair expected, got %s" (string_of_exp e))
  else EFst (step e)
and stepSnd (e:exp) : exp =
  if is_value e then
    match e with
    | EPair (_, e2) -> step e2
    | _ -> error (sprintf "Pair expected, got %s" (string_of_exp e))
  else EFst (step e)
and stepHd (e:exp) : exp =
  if is_value e then
    match e with
    | ECons (e1, _) -> step e1
    | _ -> error (sprintf "List expected, got %s" (string_of_exp e))
  else EHd (step e)
and stepTl (e:exp) : exp =
  if is_value e then
    match e with
    | ECons (_, e2) -> step e2
    | _ -> error (sprintf "List expected, got %s" (string_of_exp e))
  else ETl (step e)
and stepEmpty (e:exp) : exp =
  if is_value e then
    match e with
    | EList _       -> EBoolean true
    | ECons (_, _)  -> EBoolean false
    | _ -> error (sprintf "List expected, got %s" (string_of_exp e))
  else EEmpty (step e)

let rec step_interpret (e:exp) =
  if is_value e then
    sprintf "-> %s" (string_of_exp e) |> print_endline
  else
  begin
    sprintf "-> %s" (string_of_exp e) |> print_endline;
    step e |> step_interpret
  end
