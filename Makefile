CC = ocamlopt

TEST = ./test.sh

FLAGS =

all: cli compiler

cli: cli.ml
	$(CC) -o cli cli.ml

compiler: compiler.ml
	$(CC) -c lexer.ml
	$(CC) -c lang.ml
	$(CC) -c parser.ml
	$(CC) -c compiler.ml
	$(CC) -o compiler lexer.cmx lang.cmx parser.cmx compiler.cmx

clean:
	rm -f cli *.cmi *.cmo results.out a.out *.cmx *.o compiler

test: cli
			 $(TEST)
