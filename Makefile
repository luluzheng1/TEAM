
all: team.native fileio regex 
team.native : parser.mly scanner.mll codegen.ml semant.ml resolve.ml team.ml
	opam config exec -- \
	ocamlbuild -use-ocamlfind team.native

# For built-in functions
.PHONY: fileio
fileio: fileio.c
	gcc -c -Wall -g fileio.c
	gcc -g -o fileio -DBUILD_TEST fileio.c

.PHONY: regex
regex : regex.c
	gcc -c -Wall -g regex.c
	gcc -g  -lpcreposix -lpcre2-8 -o regex -DBUILD_TEST regex.c

.PHONY : clean
clean :
	ocamlbuild -clean
	rm *.o
	rm regex
	rm fileio
