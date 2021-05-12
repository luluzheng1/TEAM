# TEAM

Note: This file is best viewed in a Markdown reader.

TEAM (Text Extraction And Manipulation) is a domain specific programming language designed for text processing, data extraction, and report generation. With its straightforward syntax and various built-in functions, TEAM offers a clean layer of abstraction for users to perform tasks that are often cumbersome to do in general purpose languages.

## Compilation

To compile TEAM, do

      make

## Environment Setup

Team uses PCRE2 (Perl Compatible Regular Expressions) library to support regular expressions. To setup dependencies, do:

      cd pcre2-10.36 && ./configure && make && make install && cd ..

Ocamlbuild sometimes does not like .o files in the directory when trying to compile TEAM. If you encounter an issue with Ocamlbuild requiring sanitization, please do:

      cd pcre2-10.36 && make clean
      cd .. && make clean && make

For more information on the PCRE2 API, please visit:

https://www.pcre.org/current/doc/html/pcre2api.html

If for some reason the PCRE2 library fails to install on your machine, please visit https://ftp.pcre.org/pub/pcre/ and manually download the zipped file named pcre2-10.36.zip. After downloading and extracting the zipped file, please cd into the extracted folder and run:

      ./configure && make && make install

## Testing

To run all tests, do

      python scripts/runtests.py -m all

To run tests in a specific directory, do

      python scripts/runtests.py -m <mode>

| Mode    | Description                                                                                                                                                                                                                           |
| ------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| ast     | run the tests in `ast_tests/` and validate the pretty printed ast against a gold standard. Tests that pass the validation are marked with OK! and those that don't are marked with FAILED!                                            |
| sast    | run the tests in `sast_tests/` and validates the pretty printed sast against a gold standard. Tests that pass the validation are marked with OK! and those that don't are marked with FAILED!                                         |
| codegen | run the tests in `codegen_tests/`, compile the resulting LLVM code, execute the resulting file, and validate the output against a gold standard. If the validation fails, a diff of the two files will be printed to standard output. |

Files used for testing are located in `<mode>_tests/`.
The generated outputs are located in `<mode>_log/` and
the expected outputs (gold standard) are located in `<mode>_ref/`.

The default mode is ast if none was provided.

To execute a single TEAM file (file.tm), do

      python scripts/runtests.py -t file.tm -r file.log -m <mode>

- -t specifies the file to be executed

- -r specifies the file that is the gold standard

- -m when running a single test, the mode can not be `all`.

## Extended Testsuite

To run the extended testsuite, do

      python scripts/runtests.py -m extended

7 positive tests included in this testsuite are as follows:

| Program           | Description                                                                    |
| ----------------- | ------------------------------------------------------------------------------ |
| arith.tm          | Tests arithmetic operators (add, subtract, multiply, divide) for int and float |
| string.tm         | Tests string slicing and indexing                                              |
| list.tm           | Tests list slicing and indexing                                                |
| function.tm       | Tests calling a user-defined function in the body of another function          |
| while.tm          | Tests while loop                                                               |
| scope.tm          | Tests local and global variables hold correct values                           |
| formattedPrint.tm | Tests print function with formatted strings                                    |

3 negative tests included in this testsuite are as follows:

| Test            | Description                                                                                                          |
| --------------- | -------------------------------------------------------------------------------------------------------------------- |
| badDuplicate.tm | Detects duplicate function definitions                                                                               |
| badScope.tm     | Detects variables used out of scope                                                                                  |
| badReturn.tm    | Detects mistmatch between a function's return type specified by its signature and its actual return type in its body |

## Group Members

- Wenlu (Lulu) Zheng: <lulu.zheng@tufts.edu>
- Yingjie Ling: <yingjie.ling@tufts.edu>
- Saurav Gyawali: <saurav.gyawali@tufts.edu>
- Naoki Okada: <naoki.okada@tufts.edu>
