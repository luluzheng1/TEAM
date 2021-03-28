open Ast

exception NonUniformTypeContainer of typ * typ

exception UndefinedId of string

exception MismatchedTypes of typ * typ * expr

exception InvalidBinaryOperation of typ * op * typ * expr

exception InvalidUnaryOperation of typ * uop * expr

exception IllegalAssignment of typ * op option * typ * expr

exception NonListAccess of typ * typ * expr

exception InvalidIndex of typ * expr

exception TypeError of string

exception CannotRedefineBuiltIn of string

exception AlreadyDefined of string

let handle_error (e : exn) =
  match e with
  | NonUniformTypeContainer (t1, t2) ->
      let s1 = string_of_typ t1 and s2 = string_of_typ t2 in
      raise
        (TypeError
           (Printf.sprintf
              "Type error: Lists can only contain one type. Expected '%s', \
               but got '%s'"
              s1 s2 ) )
  | UndefinedId n ->
      raise
        (TypeError
           (Printf.sprintf
              "Error: variable '%s' was used before it was defined" n ) )
  | MismatchedTypes (t1, t2, e) ->
      let s1 = string_of_typ t1
      and s2 = string_of_typ t2
      and s3 = string_of_expr e in
      raise
        (TypeError
           (Printf.sprintf
              "Type error: Expected value of type '%s', but got a value of \
               type '%s' in '%s'"
              s1 s2 s3 ) )
  | InvalidBinaryOperation (t1, op, t2, e) ->
      let s1 = string_of_typ t1
      and s2 = string_of_op op
      and s3 = string_of_typ t2
      and s4 = string_of_expr e in
      raise
        (TypeError
           (Printf.sprintf
              "Type error: Illegal binary operator '%s' '%s' '%s' in '%s'" s1
              s2 s3 s4 ) )
  | InvalidUnaryOperation (t, op, e) ->
      let s1 = string_of_typ t
      and s2 = string_of_uop op
      and s3 = string_of_expr e in
      raise
        (TypeError
           (Printf.sprintf "Error: Illegal unary operator '%s' '%s' in '%s'"
              s2 s1 s3 ) )
  | IllegalAssignment (t1, op, t2, e) ->
      let s1 = string_of_typ t1
      and s3 = string_of_typ t2
      and s4 = string_of_expr e in
      let s2 = match op with Some x -> string_of_op x | None -> "" in
      raise
        (TypeError
           (Printf.sprintf
              "Error: Illegal assignment '%s' '%s'= '%s' in '%s'" s1 s2 s3 s4 )
        )
  | NonListAccess (t1, t2, e) ->
      let s1 = string_of_typ t1
      and s2 = string_of_typ t2
      and s3 = string_of_expr e in
      raise
        (TypeError
           (Printf.sprintf
              "Type Error: Expected a lvalue of type List('%s'), but got \
               lvalue of type '%s' in '%s'"
              s1 s2 s3 ) )
  | InvalidIndex (t1, e) ->
      let s1 = string_of_typ t1 and s2 = string_of_expr e in
      raise
        (TypeError
           (Printf.sprintf
              "Expected index of type Int, but got type '%s' in '%s'" s1 s2 )
        )
  | CannotRedefineBuiltIn s ->
      raise
        (TypeError
           (Printf.sprintf
              "Error: Function '%s' may not be defined as it exists as a \
               built in function"
              s ) )
  | AlreadyDefined s ->
      raise
        (TypeError
           (Printf.sprintf
              "Error: '%s' cannot be redefined in the current scope" s ) )
  | VoidType n ->
      raise
        (TypeError
           (Printf.sprintf "Error: variable '%s' cannot have type Void" n) )
  | Duplicate n ->
      raise
        (TypeError
           (Printf.sprintf "Error: variable name '%s' has already used" n) )
  | e -> raise (TypeError (Printexc.to_string e))
