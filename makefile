rules: rules.ml
	ocamlbuild -use-ocamlfind rules.byte

clean:
	rm -rf _build *.byte
