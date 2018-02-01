
ocamlc -o cli "cli.ml"

./generate-output.sh > results.out
diff solutions.out results.out
