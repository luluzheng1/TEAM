open Ast


exception NonUniformTypeContainer of typ * typ
exception UndefinedId of string
exception MismatchedTypes of typ * typ
exception InvalidBinaryOperation of typ * op * typ * expr
exception InvalidUnaryOperation of typ * uop * expr
exception TypeError of string

let handle_error (e:exn) =
  match e with
  | NonUniformTypeContainer(t1, t2) -> 
    let s1 = string_of_typ t1 and s2 = string_of_typ t2 in
    raise (TypeError (Printf.sprintf "Type error: Lists can only contain one type. Expected '%s', but got '%s'" s1 s2))
  | UndefinedId(n) -> raise (TypeError (Printf.sprintf "Error: variable '%s' was used before it was defined" n))
  | MismatchedTypes(t1, t2) -> 
    let s1 = string_of_typ t1 and s2 = string_of_typ t2 in
    raise (TypeError (Printf.sprintf "Type error: Expected value of type '%s', got a value of type '%s' instead" s1 s2))
  | InvalidBinaryOperation(t1, op, t2, e) -> 
    let s1 = string_of_typ t1 and s2 = string_of_op op and s3 = string_of_typ t2 and s4 = string_of_expr e in
    raise (TypeError (Printf.sprintf "Type error: Illegal binary operator " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ " in " ^ s4))
  | InvalidUnaryOperation(t, op, e) ->
    let s1 = string_of_typ t and s2 = string_of_uop op and s3 = string_of_expr e in
    raise (TypeError (Printf.sprintf "Illegal unary operator " ^ s2 ^ " " ^ s1 ^ " in " ^ s3))
  | e -> raise (TypeError (Printexc.to_string e))