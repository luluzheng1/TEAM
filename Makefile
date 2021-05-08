define HEADER

=====================================================
|       ████████╗███████╗ █████╗ ███╗   ███╗        |
|       ╚══██╔══╝██╔════╝██╔══██╗████╗ ████║        |
|          ██║   █████╗  ███████║██╔████╔██║        |
|          ██║   ██╔══╝  ██╔══██║██║╚██╔╝██║        |
|          ██║   ███████╗██║  ██║██║ ╚═╝ ██║        |
|          ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝        |
|    A Text Extraction And Manipulation Language    |
=====================================================

endef
export HEADER
all: team.native fileio regex 
team.native : ./src/parser.mly ./src/scanner.mll ./src/codegen.ml ./src/semant.ml ./src/resolve.ml ./src/team.ml
	opam config exec -- \
	ocamlbuild -use-ocamlfind ./src/team.native
	@echo "$$HEADER"
# For built-in functions
.PHONY: fileio
fileio: ./c_library/fileio.c
	gcc -c -Wall -g ./c_library/fileio.c
	gcc -g -o fileio -DBUILD_TEST ./c_library/fileio.c

.PHONY: regex
regex : ./c_library/regex.c
	gcc -c -Wall -g ./c_library/regex.c
	gcc -g  -lpcreposix -lpcre2-8 -o regex -DBUILD_TEST ./c_library/regex.c

.PHONY : clean
clean :
	ocamlbuild -clean
	rm *.o
	rm regex
	rm fileio
	rm -r *.dSYM/