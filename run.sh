
ocamlc -o cli "cli.ml"

./generate-output.sh > assignment.out
diff solutions.out assignment.out
