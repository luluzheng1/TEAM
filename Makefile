team.native : parser.mly scanner.mll codegen.ml semant.ml team.ml
	opam config exec -- \
	ocamlbuild -use-ocamlfind team.native

.PHONY : clean
clean :
	ocamlbuild -clean