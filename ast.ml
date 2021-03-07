(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or  | Exp  | Range

type uop = Neg | Not

type expr =
    IntLit of int
  | FloatLit of string
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | ListLit of expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | SliceExpr of expr * slce
  | Noexpr

and slce = Index of expr | Slice of expr * expr

type typ = Int | Bool | Float | Void | Char | String | List of typ | Func of typ list * typ

type bind = typ * string

type var_decl = typ * string * expr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt * stmt
  | Elif of expr * stmt
  | For of expr * expr * stmt
  | While of expr * stmt
  | Declaration of var_decl
  | Break
  | Continue
  | Nostmt

type func_body = {
  vdecls : var_decl list;
  stmts : stmt list;
}

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : func_body;
  }

type program = var_decl list * func_decl list * stmt list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Exp -> "^"
  | Range -> ".."

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | FloatLit(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StringLit(s) -> s
  | ListLit(l) -> "[" ^ (String.concat "," (List.map string_of_expr l)) ^ "]"
  | SliceExpr(e, s) -> (match s with
      Index(i) -> (string_of_expr e) ^ "[" ^ (string_of_expr i) ^ "]"
    | Slice(i,j) -> (string_of_expr e) ^ "[" ^ (string_of_expr i) ^ ":" ^ (string_of_expr i) ^ "]")
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, s) ->
      "for (" ^ string_of_expr e1  ^ " in " ^ string_of_expr e2 ^ " :\n " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

and string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | Char -> "char"
  | String -> "string"
  | List t -> "list<" ^ string_of_typ t ^ ">"
  | Func (_, t) -> "function<" ^ string_of_typ t ^ ">"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)