(* Abstract Syntax Tree and functions for printing it *)

type op =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
  | Exp
  | Range

type uop = Neg | Not

type expr =
  | IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | ListLit of expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | Call of string * expr list
  | SliceExpr of expr * slce
  | End
  | Noexpr

and slce = Index of expr | Slice of expr * expr

type typ =
  | Int
  | Bool
  | Float
  | Void
  | Char
  | String
  | List of typ
  | Func of typ list * typ
  | File
  | Unknown
(* An empty list has an unknown type *)

type bind = typ * string

type stmt =
  | Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of string * expr * stmt
  | While of expr * stmt
  | Declaration of typ * string * expr
  | Break
  | Continue

type func_decl =
  {typ: typ; fname: string; formals: bind list; body: stmt list}

type program = func_decl list * stmt list

(* Pretty-printing functions *)

let string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"
  | Exp -> "^"
  | Range -> ".."

let string_of_uop = function Neg -> "-" | Not -> "not "

let rec string_of_expr = function
  | IntLit l -> string_of_int l
  | FloatLit l -> string_of_float l
  | BoolLit true -> "true"
  | BoolLit false -> "false"
  | CharLit c -> String.make 1 c
  | StringLit s -> "\"" ^ s ^ "\""
  | ListLit l -> "[" ^ String.concat "," (List.map string_of_expr l) ^ "]"
  | SliceExpr (e, s) -> (
    match s with
    | Index i -> (string_of_expr e) ^ "[" ^ string_of_expr i ^ "]"
    | Slice (i, j) ->
        (string_of_expr e) ^ "[" ^ string_of_expr i ^ ":" ^ string_of_expr j ^ "]" )
  | Id s -> s
  | Binop (e1, o, e2) -> (
    match o with
    | Range -> string_of_expr e1 ^ string_of_op o ^ string_of_expr e2
    | _ -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
    )
  | Unop (o, e) -> string_of_uop o ^ string_of_expr e
  | Assign (v, e) -> (string_of_expr v) ^ " = " ^ string_of_expr e
  | Call (f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | End -> ""
  | Noexpr -> ""

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

let rec string_of_stmt = function
  | Block stmts -> String.concat "" (List.map string_of_stmt stmts)
  | Expr expr -> string_of_expr expr ^ ";\n"
  | Return expr -> "return " ^ string_of_expr expr ^ ";\n"
  | If (e, s1, s2) -> (
    match s2 with
    | Block [] ->
        "if " ^ string_of_expr e ^ ":\n"
        ^ indent (string_of_stmt s1)
        ^ "end\n"
    | _ ->
        "if " ^ string_of_expr e ^ ":\n"
        ^ indent (string_of_stmt s1)
        ^ "else:\n"
        ^ indent (string_of_stmt s2)
        ^ "end\n" )
  | For (s, e2, st) ->
      "for " ^ s ^ " in " ^ string_of_expr e2 ^ ":\n"
      ^ indent (string_of_stmt st)
      ^ "end\n"
  | While (e, s) ->
      "while " ^ string_of_expr e ^ ":\n"
      ^ indent (string_of_stmt s)
      ^ "end\n"
  | Declaration (t, id, e) -> (
    match e with
    | Noexpr -> string_of_typ t ^ " " ^ id ^ ";\n"
    | _ -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e ^ ";\n" )
  | Break -> "break;\n"
  | Continue -> "continue;\n"

and string_of_typ = function
  | Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | Char -> "char"
  | String -> "string"
  | List t -> "list<" ^ string_of_typ t ^ ">"
  | Func (a, r) ->
      "("
      ^ String.concat "," (List.map string_of_typ a)
      ^ ")" ^ "->" ^ string_of_typ r
  | File -> "file"
  | Unknown -> "?"

let string_of_formals (formals : bind list) : string =
  String.concat ", "
    (List.map
       (fun (formal : bind) -> string_of_typ (fst formal) ^ " " ^ snd formal)
       formals )

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^ fdecl.fname ^ "("
  ^ string_of_formals fdecl.formals
  ^ "):\n"
  ^ indent (String.concat "" (List.map string_of_stmt fdecl.body))
  ^ "end\n"

let string_of_program (funcs, stmts) =
  String.concat "\n" (List.map string_of_fdecl funcs)
  ^ String.concat "" (List.map string_of_stmt stmts)