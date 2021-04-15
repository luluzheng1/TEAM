
all: team.native

team.native : parser.mly scanner.mll codegen.ml semant.ml team.ml
	opam config exec -- \
	ocamlbuild -use-ocamlfind team.native

# For built-in functions
string : string.c
	cc -o string -DBUILD_TEST string.c


.PHONY : clean
clean :
	ocamlbuild -clean
	rm *.o