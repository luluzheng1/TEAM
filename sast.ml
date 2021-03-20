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

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SIntLit(l) -> string_of_int l
  | SFloatLit(l) -> string_of_float l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SCharLit(c) -> String.make 1 c
  | SStringLit(s) -> "\"" ^ s ^ "\""
  | SListLit(l) -> "[" ^ (String.concat "," (List.map string_of_sexpr l)) ^ "]"
  | SSliceExpr(e, s) -> (match s with
      SIndex(i) -> e ^ "[" ^ (string_of_sexpr i) ^ "]"
    | SSlice(i,j) -> e ^ "[" ^ (string_of_sexpr i) ^ ":" ^ (string_of_sexpr j) ^ "]")
  | SId(s) -> s
  | SBinop(e1, o, e2) -> (match o with
      Range -> string_of_sexpr e1 ^ string_of_op o ^ string_of_sexpr e2
    | _ -> string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2)
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SListAssign(s, e1, e2) -> s ^ " [ " ^ string_of_sexpr e1 ^ " ] = " ^ string_of_sexpr e2
  | SAssignOp(s, o, e) -> s ^ " " ^ string_of_op o ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SEnd -> ""
  | SNoexpr -> "" ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) -> String.concat "" (List.map string_of_sstmt stmts)
  | SExpr(expr) -> string_of_sexpr expr ^ "\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ "\n";
  | SIf (e, s1, s2, s3) -> "if " ^ string_of_sexpr e ^ ":\n" ^ string_of_sstmt s1 ^ string_of_sstmt s2 ^ "else:\n" ^ string_of_sstmt s3 ^ "end\n"
  | SElif(e, s) -> "elif " ^ string_of_sexpr e ^ ":\n" ^ string_of_sstmt s
  | SFor(e1, e2, s) ->
      "for " ^ string_of_sexpr e1  ^ " in " ^ string_of_sexpr e2 ^ ":\n " ^ string_of_sstmt s ^ "end\n"
  | SWhile(e, s) -> "while " ^ string_of_sexpr e ^ ":\n" ^ string_of_sstmt s ^ "end\n"
  | SDeclaration(t, id, (tp, e)) ->  (match e with
      SNoexpr -> string_of_typ t ^ " " ^ id ^ "\n"
    | _ -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_sexpr (tp, e) ^ "\n")
  | SBreak -> "break\n"
  | SContinue -> "continue\n"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.body) ^
  "end\n"

let string_of_sprogram (funcs, stmts) =
  String.concat "" (List.map string_of_sfdecl funcs) ^ "\n" ^
  String.concat "" (List.map string_of_sstmt stmts)
