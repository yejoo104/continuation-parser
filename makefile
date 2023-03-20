all: parser test

parser: parser.ml
	ocamlbuild -use-ocamlfind -package core parser.byte

test: test.ml
	ocamlbuild -use-ocamlfind -package core test.byte

clean:
	rm -rf _build *.byte
