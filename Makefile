
all: team.native substring.o

team.native : parser.mly scanner.mll codegen.ml semant.ml team.ml
	opam config exec -- \
	ocamlbuild -use-ocamlfind team.native

# Testing the "printbig" example

substring : substring.c
	cc -o substring -DBUILD_TEST substring.c


.PHONY : clean
clean :
	ocamlbuild -clean
	rm *.o