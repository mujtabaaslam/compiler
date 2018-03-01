OCBFLAGS :=
OCB := ocamlbuild $(OCBFLAGS)
TEST = test/test.sh

.PHONY: all debug clean top test

all: compiler.native
debug: all compiler.cma

%.cma: .FORCE
	$(OCB) $@

%.cmxa: .FORCE
	$(OCB) $@

%.native: .FORCE
	$(OCB) $@

%.p.native: .FORCE
	$(OCB) $@

%.byte: .FORCE
	$(OCB) $@

.FORCE:

test:
	$(TEST)

clean:
	$(OCB) -clean

top: compiler.cma
	utop