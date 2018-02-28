type op =
  | TPlus
  | TMinus
  | TMultiply
  | TDivide
  | TLeq
  | If

type token =
  | TInt of int
  | TLParen
  | TRParen
  | Operation of op
  | Boolean of bool

let string_of_operation (t:op) : string =
  match t with
  | TPlus     -> "+"
  | TMinus    -> "-"
  | TMultiply -> "*"
  | TDivide   -> "/"
  | TLeq      -> "<="
  | If        -> "if"

let string_of_token (t:token) : string =
  match t with
  | TInt n      -> string_of_int n
  | TLParen     -> "("
  | TRParen     -> ")"
  | Operation t -> string_of_operation t
  | Boolean t   -> string_of_bool t

let string_of_token_list (toks:token list) : string =
  String.concat "," (List.map string_of_token toks)

(* Peeks at the head of the stream without advancing it forward *)
let peek (src:char Stream.t) : char =
  match Stream.peek src with
  | Some ch -> ch
  | None    -> failwith "Unexpected end of file encountered"

(* Pops the head of the stream and returns it, advancing the stream forward *)
let advance : char Stream.t -> char = Stream.next

(* Returns true iff this stream still has elements left *)
let is_empty (src:char Stream.t) : bool =
  try
    Stream.empty src; true
  with
    Stream.Failure -> false

let is_whitespace (ch:char) : bool =
  ch = ' ' || ch = '\012' || ch = '\n' || ch = '\r' || ch = '\t'

let is_digit (ch:char) : bool =
  let code = Char.code ch in
  48 <= code && code <= 57

(* Note: lex contains two nested helper functions, lex_num and go *)
let lex (src:char Stream.t) : token list =
  let rec lex_num acc =
    if is_digit (peek src) then
      lex_num (acc ^ (Char.escaped (advance src)))
    else
      int_of_string acc
  in
  let rec go () =
    if not (is_empty src) then
      let ch = peek src in
      (* Note: the |> operator takes the result of the left-hand side
       * and feeds it as an argument to the function on the right-hand
       * side.  ignore has type 'a -> unit---it allows us to throw
       * away the return type of a function we don't care about *)
      match ch with
      | '(' -> advance src |> ignore; TLParen :: go ()
      | ')' -> advance src |> ignore; TRParen :: go ()
      | '+' -> advance src |> ignore; Operation TPlus :: go ()
      | '-' -> advance src |> ignore; Operation TMinus :: go ()
      | '*' -> advance src |> ignore; Operation TMultiply :: go ()
      | '/' -> advance src |> ignore; Operation TDivide :: go ()
      | '<' -> advance src |> ignore; let c = peek src in
                                      (match c with
                                      | '=' -> advance src |> ignore; Operation TLeq :: go ()
                                      | _ -> failwith ("unexpected character"))
      | 't' -> advance src |> ignore; let c = peek src in
                                      (match c with
                                      | 'r' -> advance src |> ignore; let c = peek src in
                                                                      (match c with
                                                                      | 'u' -> advance src |> ignore; let c = peek src in
                                                                                                      (match c with
                                                                                                      | 'e' -> advance src |> ignore; Boolean true :: go ()
                                                                                                      | _ -> failwith ("unexpected character"))
                                                                      | _ -> failwith ("unexpected character"))
                                      | _ -> failwith ("unexpected character"))
      | 'f' -> advance src |> ignore; let c = peek src in
                                      (match c with
                                      | 'a' -> advance src |> ignore; let c = peek src in
                                                                      (match c with
                                                                      | 'l' -> advance src |> ignore; let c = peek src in
                                                                                                      (match c with
                                                                                                      | 's' -> advance src |> ignore; let c = peek src in
                                                                                                                                      (match c with
                                                                                                                                      | 'e' -> advance src |> ignore; Boolean false :: go ()
                                                                                                                                      | _ -> failwith ("unexpected character"))
                                                                                                      | _ -> failwith ("unexpected character"))
                                                                      | _ -> failwith ("unexpected character"))
                                      | _ -> failwith ("unexpected character"))
      | 'i' -> advance src |> ignore; let c = peek src in
                                      (match c with
                                       | 'f' -> advance src |> ignore; Operation If :: go ()
                                       | _   -> failwith ("unexpected character"))
      | _   ->
        if is_whitespace ch then
          begin advance src |> ignore; go () end
        else if is_digit ch then
          let n = lex_num "" in
          TInt n :: go ()
        else
            failwith (Printf.sprintf "Unexpected character found: %c" ch)
    else
      []
  in
    go ()
