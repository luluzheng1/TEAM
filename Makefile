
all: team.native fileio

team.native : parser.mly scanner.mll codegen.ml semant.ml team.ml
	opam config exec -- \
	ocamlbuild -use-ocamlfind team.native

# For built-in functions
string : string.c
	cc -o string -DBUILD_TEST string.c

fileio: fileio.c
	gcc -c -Wall -g fileio.c
	gcc -g -o fileio -DBUILD_TEST fileio.c

.PHONY : clean
clean :
	ocamlbuild -clean
	rm *.o