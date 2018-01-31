# Compiler:
CC = ocamlc

# Compilation Flags:
FLAGS =

all: cli

cli: cli.ml
	$(CC) -o cli cli.ml

clean:
	rm -f cli *.cmi *.cmo
