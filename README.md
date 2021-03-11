# TEAM

## Compilation and Execution

To compile the TEAM, do

      ocamlbuild team.native

To execute a TEAM file (file.tm), do

      ./team.native file.tm

## Testing

All files used for testing are located in the tests/ directory. To run all tests, do

      python runtests.py

To run a single tests, do

      ./team.native tests/if.tm

## Future Work

We are working on adding a "file" type to the ast and an "import" statement to the parser.

## Names and Email Addresses of Group Members

- Wenlu (Lulu) Zheng: <lulu.zheng@tufts.edu>
- Yingjie Ling: <yingjie.ling@tufts.edu>
- Saurav Gyawali: <saurav.gyawali@tufts.edu>
- Naoki Okada: <naoki.okada@tufts.edu>
