CC = ocamlc

TEST = ./test.sh

FLAGS =

all: cli

cli: cli.ml
	$(CC) -o cli cli.ml

clean:
	rm -f cli *.cmi *.cmo results.out a.out

test: cli
			 $(TEST)
