open Lang
open Lexer
open Parser

let lex = ref false
let parse = ref false
let file = ref ".txt"

let string_of_token (t:token) : string =
  match t with
  | INT n     -> string_of_int n
  | LPAREN    -> "("
  | RPAREN    -> ")"
  | PLUS      -> "+"
  | MINUS     -> "-"
  | MULTIPLY  -> "*"
  | DIVIDE    -> "/"
  | LEQ       -> "<="
  | IF        -> "if"
  | THEN      -> "then"
  | ELSE      -> "else"
  | BOOL b    -> string_of_bool b
  | _         -> failwith ("unexpected token")

let string_of_token_list (toks:token list) : string =
  String.concat "," (List.map string_of_token toks)

let rec string_of_exp (e:exp)=
  match e with
  | EInt n                   -> Printf.printf "%s" (string_of_int n)
  | EAdd (e1, e2)            -> string_of_exp e1; Printf.printf " + "; string_of_exp e2
  | ESubtract (e1, e2)       -> string_of_exp e1; Printf.printf " - "; string_of_exp e2
  | EMultiplication (e1, e2) -> string_of_exp e1; Printf.printf " * "; string_of_exp e2
  | EDivision (e1, e2)       -> string_of_exp e1; Printf.printf " / "; string_of_exp e2
  | Eif (e1, e2, e3)         -> Printf.printf "if "; string_of_exp e1; Printf.printf " then "; string_of_exp e2; Printf.printf " else "; string_of_exp e3
  | ELeq (e1, e2)            -> string_of_exp e1; Printf.printf " <= "; string_of_exp e2
  | EBoolean b               -> Printf.printf "%s" (string_of_bool b)
  | Eexp (e1)                -> Printf.printf "("; string_of_exp e1; Printf.printf ")"
  | Bexp (e1)                -> Printf.printf "("; string_of_exp e1; Printf.printf ")" 

let start_up(f:string) =
  file := f

let compile (file:string) =
  let lexbuf = (Lexing.from_channel (open_in file)) in
  if !lex then
    let rec lexing tokens =
      let t = Lexer.token lexbuf in
        match t with
        | Parser.EOF -> Printf.printf "["; Printf.printf "%s" (string_of_token_list (List.rev tokens)); Printf.printf "]\n"
        | _ -> lexing (t :: tokens)
        in lexing []
  else let ast = Parser.prog Lexer.token lexbuf in
  let parse_mode a =
    if !parse then
         begin
         string_of_exp a;
         Printf.printf "\n"
         end
    else
        Lang.execute a
  in parse_mode ast

let main () =
  let speclist = [
  ("-lex", Arg.Set lex, "processes the input source file through the lexing phase and prints the resulting stream of tokens to the console");
  ("-parse", Arg.Set parse, "processes the input source file through the parsing phase and prints the resulting abstract syntax tree");
  ] in
  let usage_msg = "Usage: my-project [flags] [args]\n Available flags:" in
  Arg.parse speclist start_up usage_msg;
  compile !file

let _ = if !Sys.interactive then () else main ()
