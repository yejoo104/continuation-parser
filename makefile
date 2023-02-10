parser: parser.ml
	ocamlbuild -use-ocamlfind -package core parser.byte

rules: rules.ml
	ocamlbuild -use-ocamlfind rules.byte

clean:
	rm -rf _build *.byte
