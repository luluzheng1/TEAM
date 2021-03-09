(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq | And | Or  | Exp  | Range

type uop = Neg | Not

(* TODO: need listaccess? *)
type expr =
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | ListLit of expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | AssignOp of string * op * expr
  | Call of string * expr list
  | SliceExpr of expr * slce
  | Noexpr

and slce = Index of expr | Slice of expr * expr

type typ = Int | Bool | Float | Void | Char | String | List of typ | Func of typ * typ

type bind = typ * string

type var_decl = typ * string * expr

(* Need append? *)
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

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | FloatLit(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | CharLit(c) -> String.make 1 c
  | StringLit(s) -> "\"" ^ s ^ "\""
  | ListLit(l) -> "[" ^ (String.concat "," (List.map string_of_expr l)) ^ "]"
  | SliceExpr(e, s) -> (match s with
      Index(i) -> (string_of_expr e) ^ "[" ^ (string_of_expr i) ^ "]"
    | Slice(i,j) -> (string_of_expr e) ^ "[" ^ (string_of_expr i) ^ ":" ^ (string_of_expr i) ^ "]")
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | AssignOp(s, o, e) -> s ^ " " ^ string_of_op o ^ " " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) -> String.concat "" (List.map string_of_stmt stmts) ^ "end\n"
  | Expr(expr) -> string_of_expr expr ^ "\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ "\n";
  | If (e, s1, s2, s3) -> "if " ^ string_of_expr e ^ " do\n" ^ string_of_stmt s1 ^ string_of_stmt s2 ^ "else\n" ^ string_of_stmt s3
  | Elif(e, s) -> "elif " ^ string_of_expr e ^ "\n" ^ string_of_stmt s   
  | For(e1, e2, s) ->
      "for " ^ string_of_expr e1  ^ " in " ^ string_of_expr e2 ^ " do\n " ^ string_of_stmt s
  | While(e, s) -> "while " ^ string_of_expr e ^ " do\n" ^ string_of_stmt s
  | Declaration(t, id, e) ->  (match e with
      Noexpr -> string_of_typ t ^ " " ^ id
    | _ -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e)
  | Break -> "break"
  | Continue -> "continue"
  | Nostmt -> ""

and string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | Char -> "char"
  | String -> "string"
  | List t -> "list<" ^ string_of_typ t ^ ">"
  | Func (a, r) -> "(" ^ string_of_typ a ^ "->" ^ string_of_typ r ^ ")"

let string_of_vdecl (t, id, e) = string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e ^ "\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.body.vdecls) ^
  String.concat "" (List.map string_of_stmt fdecl.body.stmts) ^
  "end\n"

let string_of_program (vars, funcs, stmts) =
  String.concat "\n" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_stmt stmts)