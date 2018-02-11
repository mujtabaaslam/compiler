open Lang
open Lexer

let rec peek : token list -> token = List.hd
let rec advance : token list -> token list = List.tl

let rec consume (t:token) (toks:token list) : token list =
  match toks with
  | t' :: toks ->
    if t = t' then
      toks
    else
      failwith (Printf.sprintf "Expected '%s', found '%s'" (string_of_token t) (string_of_token t'))
  | _ -> failwith "Encountered unexpected end of token stream"

let rec parse (toks:token list) : (exp * token list) =
  if List.length toks = 0 then
    failwith "Unexpected end of token stream"
  else
    match peek toks with
    | TInt n  -> (EInt n, advance toks)
    | Boolean b -> (EBoolean b, advance toks)
    | TLParen -> begin
        let oper = ref TMinus in
        let toks       = consume TLParen toks in
        let toks       = match peek toks with
                         | Operation TPlus     ->  oper := TPlus; consume (Operation TPlus) toks
                         | Operation TMinus    ->  oper := TMinus; consume (Operation TMinus) toks
                         | Operation TMultiply ->  oper := TMultiply; consume (Operation TMultiply) toks
                         | Operation TDivide   ->  oper := TDivide; consume (Operation TDivide) toks
                         | Operation TLeq      ->  oper := TLeq; consume (Operation TLeq) toks
                         | Operation If        ->  oper := If; consume (Operation If) toks
                         | _                   ->  failwith "Unexpected Operation"
                         in
        if oper.contents = If then
          let (e1, toks) = parse toks in
          let (e2, toks) = parse toks in
          let (e3, toks) = parse toks in
          let toks       = consume TRParen toks in
          (match oper.contents with
            | If        -> (Eif (e1, e2, e3), toks)
            | _         -> failwith "Unexpected Operation")
        else
        let (e1, toks) = parse toks in
        let (e2, toks) = parse toks in
        let toks       = consume TRParen toks in
        (match oper.contents with
          | TPlus     -> (EAdd (e1, e2), toks)
          | TMinus    -> (ESubtract (e1, e2), toks)
          | TMultiply -> (EMultiplication (e1, e2), toks)
          | TDivide   -> (EDivision (e1, e2), toks)
          | TLeq      -> (ELeq (e1, e2), toks)
          | _         -> failwith "Unexpected Operation")
      end
    | t      -> failwith (Printf.sprintf "Unexpected token found: %s" (string_of_token t))
