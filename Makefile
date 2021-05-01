
all: team.native string

team.native : parser.mly scanner.mll codegen.ml semant.ml resolve.ml team.ml
	opam config exec -- \
	ocamlbuild -use-ocamlfind team.native

# For built-in functions
regex : regex.c
	gcc -c -Wall -g regex.c
	gcc -g  -lpcreposix -lpcre2-8 -o regex -DBUILD_TEST regex.c

string : string.c
	gcc -c -Wall -g string.c
	gcc -g -o string -DBUILD_TEST string.c

.PHONY : clean
clean :
	ocamlbuild -clean
	rm *.o
	rm regex
