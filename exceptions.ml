open Ast


exception NonUniformTypeContainer of typ * typ

exception TypeError of string
let handle_error (e:exn) =
  match e with
  | NonUniformTypeContainer(t1, t2) -> 
    let s1 = string_of_typ t1 and s2 = string_of_typ t2 in
    raise (TypeError (Printf.sprintf "Type error: Lists can only contain one type. Expected '%s', but got '%s'" s1 s2))
  | e -> raise (TypeError (Printexc.to_string e))