open Lang
open Lexer
open Parser

let lex = ref false
let parse = ref false
let file = ref ".txt"
let step = ref false
let repl = ref false

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
  | TINT      -> "int"
  | TBOOL     -> "bool"
  | COLON     -> ":"
  | COMMA     -> ","
  | FST       -> "fst"
  | SND       -> "snd"
  | LBRK      -> "["
  | RBRK      -> "]"
  | DCOLON    -> "::"
  | HD        -> "hd"
  | TL        -> "tl"
  | EMPTY     -> "empty"
  | TLIST     -> "list"
  | REF       -> "ref"
  | COLONEQ   -> ":="
  | EXC       -> "!"
  | SCOLON    -> ";"
  | WHILE     -> "while"
  | DO        -> "do"
  | END       -> "end"
  | NEW       -> "new"
  | ARRAY     -> "array"
  | _         -> failwith ("unexpected token")

let string_of_token_list (toks:token list) : string =
  String.concat "," (List.map string_of_token toks)

let start_up(f:string) =
  file := f

let read () : string =
  print_string "# "; flush stdout;
  let rec create_list acc =
    let ch = input_char stdin in
    if ch = '\n' then
      String.concat "" (List.map (String.make 1) (List.rev acc))
    else
      create_list (ch :: acc)
    in
    create_list []

let inter ast =
  let typ = Lang.type_check ast in
  let ans = Lang.interpret ast in
  let value = (string_of_exp (left ans) (right ans)) in
    Printf.printf "- : %s = %s\n" (string_of_typ typ) value

let rec repl_mode () =
  let input = read () in
  if input = "exit" then ()
  else
      try
        input
        |> Lexing.from_string
        |> Parser.prog Lexer.token
        |> inter; repl_mode ()
      with
      | _ -> print_endline "Error"; repl_mode ()

  let compile (file:string) =
    if !repl then
    begin
    Printf.printf "        Compiler version 1.00\n";
    repl_mode()
    end
    else
      begin
      let lexbuf = (Lexing.from_channel (open_in file)) in
      if !lex then
        let rec lexing tokens =
          let t = Lexer.token lexbuf in
            match t with
            | Parser.EOF -> Printf.printf "["; Printf.printf "%s" (string_of_token_list (List.rev tokens)); Printf.printf "]\n"
            | _ -> lexing (t :: tokens)
            in lexing []
      else
      let ast = Parser.prog Lexer.token lexbuf in
        if !parse then
          string_of_exp Env.empty ast |> print_endline
        else if !step then
          begin
          type_check ast |> ignore;
          step_interpret ast
          end
        else
          begin
          type_check ast |> ignore;
          let state = interpret ast in
          string_of_exp (left state) (right state) |> print_endline
          end
      end

let main () =
  let speclist = [
  ("-lex", Arg.Set lex, "processes the input source file through the lexing phase and prints the resulting stream of tokens to the console");
  ("-parse", Arg.Set parse, "processes the input source file through the parsing phase and prints the resulting abstract syntax tree");
  ("-step", Arg.Set step, "processes the input and prints out every step of evaluation");
  ("-repl", Arg.Set repl, "enters repl mode which allows you to interactively enter expressions and see their final evaluated values")
  ] in
  let usage_msg = "Usage: my-project [flags] [args]\n Available flags:" in
  Arg.parse speclist start_up usage_msg;
  compile !file

let _ = if !Sys.interactive then () else main ()
