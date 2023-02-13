parser: parser.ml
	ocamlbuild -use-ocamlfind -package core parser.byte

clean:
	rm -rf _build *.byte
