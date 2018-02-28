let main () =
  Sys.argv.(1)
    |> open_in
    |> Lexing.from_channel
    |> Parser.prog Lexer.token
    |> Lang.execute

let _ = if !Sys.interactive then () else main ()
