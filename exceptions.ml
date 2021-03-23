open Ast


exception NonUniformTypeContainer of typ * typ
exception UndefinedId of string
exception MismatchedTypes of typ * typ
exception TypeError of string

let handle_error (e:exn) =
  match e with
  | NonUniformTypeContainer(t1, t2) -> 
    let s1 = string_of_typ t1 and s2 = string_of_typ t2 in
    raise (TypeError (Printf.sprintf "Type error: Lists can only contain one type. Expected '%s', but got '%s'" s1 s2))
  | UndefinedId(n) -> raise (TypeError (Printf.sprintf "Error: variable '%s' was used before it was defined" n))
  | MismatchedTypes(t1, t2) -> 
    let s1 = string_of_typ t1 and s2 = string_of_typ t2 in
    raise (TypeError (Printf.sprintf "Type error: expected value of type '%s', got a value of type '%s' instead" s1 s2))
  | e -> raise (TypeError (Printexc.to_string e))