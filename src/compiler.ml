open Lang
open Lexer
open Parser

let lex = ref false
let parse = ref false
let file = ref ".txt"
let step = ref false

let string_of_token (t:token) : string =
  match t with
  | INT n     -> string_of_int n
  | VAR x     -> x
  | LPAREN    -> "("
  | RPAREN    -> ")"
  | PLUS      -> "+"
  | MINUS     -> "-"
  | MULTIPLY  -> "*"
  | DIVIDE    -> "/"
  | LEQ       -> "<="
  | LESS      -> "<"
  | GEQ       -> ">="
  | GREAT     -> ">"
  | EQUAL     -> "=="
  | IF        -> "if"
  | THEN      -> "then"
  | ELSE      -> "else"
  | BOOL b    -> string_of_bool b
  | LET       -> "let"
  | EQ        -> "="
  | IN        -> "in"
  | FIX       -> "fix"
  | FUNC      -> "fun"
  | ARROW     -> "->"
  | _         -> failwith ("unexpected token")

let string_of_token_list (toks:token list) : string =
  String.concat "," (List.map string_of_token toks)

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
      if !parse then
        string_of_exp ast |> print_endline
      else if !step then
        step_interpret ast
      else
        interpret ast |> string_of_exp |> print_endline

let main () =
  let speclist = [
  ("-lex", Arg.Set lex, "processes the input source file through the lexing phase and prints the resulting stream of tokens to the console");
  ("-parse", Arg.Set parse, "processes the input source file through the parsing phase and prints the resulting abstract syntax tree");
  ("-step", Arg.Set step, "processes the input and prints out every step of evaluation")
  ] in
  let usage_msg = "Usage: my-project [flags] [args]\n Available flags:" in
  Arg.parse speclist start_up usage_msg;
  compile !file

let _ = if !Sys.interactive then () else main ()
