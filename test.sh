
ocamlc -o cli "cli.ml"
ocamlopt -c lexer.ml
ocamlopt -c lang.ml
ocamlopt -c parser.ml
ocamlopt -c compiler.ml
ocamlopt -o compiler lexer.cmx lang.cmx parser.cmx compiler.cmx
./generate-output.sh > results.out
diff solutions.out results.out
