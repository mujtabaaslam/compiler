let length = ref false
let ret = ref []

let makeList (x: string) =
  ret := x :: !ret

let printItems() =
  List.iter print_endline (List.rev !ret)

let printLengths() =
  List.iter (Printf.printf "%d\n") (List.map String.length (List.rev !ret))

let main =
let speclist = [
("-length", Arg.Set length, "prints the lengths of each of the arguments");
]
in let usage_msg = "Usage: my-project [flags] [args] \n Available flags:"
in Arg.parse speclist makeList usage_msg;
   match !length with
   | false -> printItems()
   | true  -> printLengths()

let _ = main
