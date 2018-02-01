ocamlc *.ml
./generate-output.sh > assignment.out
diff solutions.out assignment.out > assignment.diff
