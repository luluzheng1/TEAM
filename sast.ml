(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast
module StringMap = Map.Make (String)

type symbol_table =
  { variables: typ StringMap.t; (* Variables bound in current block *)
    parent: symbol_table option (* Enclosing scope *) }

type sexpr = typ * sx

and sx =
  | SIntLit of int
  | SFloatLit of float
  | SBoolLit of bool
  | SCharLit of char
  | SStringLit of string
  | SListLit of sexpr list
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of sexpr * sexpr
  | SCall of sexpr * sexpr list
  | SSliceExpr of sexpr * sslce
  | SEnd
  | SNoexpr

and sslce = SIndex of sexpr | SSlice of sexpr * sexpr

type sstmt =
  | SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of string * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SDeclaration of typ * string * sexpr
  | SBreak
  | SContinue

type sfunc_decl =
  {styp: typ; sfname: string; sformals: bind list; sbody: sstmt list}

type program = sfunc_decl list * sstmt list

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : "
  ^ ( match e with
    | SIntLit i -> string_of_int i
    | SFloatLit f -> string_of_float f
    | SBoolLit true -> "true"
    | SBoolLit false -> "false"
    | SCharLit c -> String.make 1 c
    | SStringLit s -> "\"" ^ s ^ "\""
    | SListLit l ->
        "[" ^ String.concat "," (List.map string_of_sexpr l) ^ "]"
    | SSliceExpr (e, s) -> (
      match s with
      | SIndex i -> (string_of_sexpr e) ^ "[" ^ string_of_sexpr i ^ "]"
      | SSlice (i, j) ->
          (string_of_sexpr e) ^ "[" ^ string_of_sexpr i ^ ":" ^ string_of_sexpr j ^ "]" )
    | SId s -> s
    | SBinop (e1, o, e2) -> (
      match o with
      | Range -> string_of_sexpr e1 ^ string_of_op o ^ string_of_sexpr e2
      | _ ->
          string_of_sexpr e1 ^ " " ^ string_of_op o ^ " "
          ^ string_of_sexpr e2 )
    | SUnop (o, e) -> string_of_uop o ^ string_of_sexpr e
    | SAssign (v, e) -> (string_of_sexpr v) ^ " = " ^ string_of_sexpr e
    | SCall (f, el) ->
        (string_of_sexpr f) ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    | SEnd -> ""
    | SNoexpr -> "" )
  ^ ")"

let indent stmts_as_string =
  let rec take k xs =
    match k with
    | 0 -> []
    | k -> (
      match xs with [] -> failwith "take" | y :: ys -> y :: take (k - 1) ys )
  in
  let l = String.split_on_char '\n' stmts_as_string in
  let indent_stmt stmts_as_list =
    List.map (fun stmt_as_string -> "\t" ^ stmt_as_string) stmts_as_list
  in
  String.concat "\n" (indent_stmt (take (List.length l - 1) l)) ^ "\n"

let rec string_of_sstmt = function
  | SBlock stmts -> String.concat "" (List.map string_of_sstmt stmts)
  | SExpr expr -> string_of_sexpr expr ^ "\n"
  | SReturn expr -> "return " ^ string_of_sexpr expr ^ "\n"
  | SIf (e, s1, s2) -> (
    match s2 with
    | SBlock [] ->
        "if " ^ string_of_sexpr e ^ ":\n"
        ^ indent (string_of_sstmt s1)
        ^ "end\n"
    | _ ->
        "if " ^ string_of_sexpr e ^ ":\n"
        ^ indent (string_of_sstmt s1)
        ^ "else:\n"
        ^ indent (string_of_sstmt s2)
        ^ "end\n" )
  | SFor (s, e2, st) ->
      "for " ^ s ^ " in " ^ string_of_sexpr e2 ^ ":\n "
      ^ indent (string_of_sstmt st)
      ^ "end\n"
  | SWhile (e, s) ->
      "while " ^ string_of_sexpr e ^ ":\n"
      ^ indent (string_of_sstmt s)
      ^ "end\n"
  | SDeclaration (t, id, (tp, e)) -> (
    match e with
    | SNoexpr -> string_of_typ t ^ " " ^ id ^ "\n"
    | _ ->
        string_of_typ t ^ " " ^ id ^ " = " ^ string_of_sexpr (tp, e) ^ "\n" )
  | SBreak -> "break\n"
  | SContinue -> "continue\n"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^ fdecl.sfname ^ "("
  ^ String.concat ", " (List.map snd fdecl.sformals)
  ^ ")\n"
  ^ indent (String.concat "" (List.map string_of_sstmt fdecl.sbody))
  ^ "end\n"

let string_of_sprogram (funcs, stmts) =
  String.concat "" (List.map string_of_sfdecl funcs)
  ^ "\n"
  ^ String.concat "" (List.map string_of_sstmt stmts)
