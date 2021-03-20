open Ast

type sexpr = typ * sx
and sx =
    SIntLit of int
  | SFloatLit of float
  | SBoolLit of bool
  | SCharLit of char
  | SStringLit of string
  | SListLit of sexpr list
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SListAssign of string * sexpr * sexpr
  | SAssignOp of string * op * sexpr
  | SCall of string * sexpr list
  | SSliceExpr of string * sslce
  | SEnd
  | SNoexpr

and sslce = SIndex of sexpr | SSlice of sexpr * sexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt * sstmt
  | SElif of sexpr * sstmt
  | SFor of sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SDeclaration of typ * string * sexpr
  | SBreak
  | SContinue

type sfunc_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : sstmt list;
  }

type program = sfunc_decl list * sstmt list
