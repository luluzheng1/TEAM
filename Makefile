
all: team.native regex getlist

team.native : parser.mly scanner.mll codegen.ml semant.ml team.ml
	opam config exec -- \
	ocamlbuild -use-ocamlfind team.native

# For built-in functions
regex : regex.c
	gcc -c -Wall -g regex.c
	gcc -g  -lpcreposix -lpcre2-8 -o regex -DBUILD_TEST regex.c

getlist: getlist.c
	gcc -c -Wall -g getlist.c
	gcc -g -o getlist -DBUILD_TEST getlist.c


.PHONY : clean
clean :
	ocamlbuild -clean
	rm *.o
	rm regex getlist