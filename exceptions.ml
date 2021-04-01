open Ast

exception NonUniformTypeContainer of typ * typ

exception UndefinedId of string

exception MismatchedTypes of typ * typ * expr

exception InvalidBinaryOperation of typ * op * typ * expr

exception InvalidUnaryOperation of typ * uop * expr

exception IllegalAssignment of typ * op option * typ * expr

exception IllegalDeclaration of typ * typ * stmt

exception NonListAccess of typ * typ * expr

exception InvalidIndex of typ * expr

exception TypeError of string

exception CannotRedefineBuiltIn of string

exception AlreadyDefined of string

exception VoidType of string

exception Duplicate of string

exception UndefinedFunction of string

exception WrongNumberOfArgs of int * int * expr

exception IllegalArgument of typ * typ * expr

exception IllegalSlice of expr * typ

exception WrongIndex of typ * expr

exception WrongSliceIndex of typ * typ * expr * expr

exception ReturnNotLast

exception ReturnOutsideFunction

exception ReturnMismatchedTypes of typ * typ * stmt

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
  | IllegalDeclaration (t1, t2, s) ->
      let s1 = string_of_typ t1
      and s2 = string_of_typ t2
      and s3 = string_of_stmt s in
      raise
        (TypeError
           (Printf.sprintf "Error: Illegal assignment '%s' '%s' in '%s'" s1
              s2 s3 ) )
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
           (Printf.sprintf "Error: variable name '%s' has already defined" n) )
  | UndefinedFunction n ->
      raise
        (TypeError
           (Printf.sprintf
              "Error: function '%s' was called, but it is undefined" n ) )
  | WrongNumberOfArgs (exp, act, e) ->
      let s1 = string_of_int exp
      and s2 = string_of_int act
      and s3 = string_of_expr e in
      raise
        (TypeError
           (Printf.sprintf
              "Error: expected '%s' arguments but got '%s' in '%s'" s1 s2 s3 )
        )
  | IllegalArgument (t1, t2, e) ->
      let s1 = string_of_typ t1
      and s2 = string_of_typ t2
      and s3 = string_of_expr e in
      raise
        (TypeError
           (Printf.sprintf
              "Type Error: Illegal argument found in '%s'. Expected \
               argument of type '%s' but got '%s'"
              s3 s1 s2 ) )
  | IllegalSlice (e, t) ->
      let s1 = string_of_expr e and s2 = string_of_typ t in
      raise
        (TypeError
           (Printf.sprintf
              "Type Error: Illegal Slice expression, expected '%s' to be \
               either a list or array, but got a '%s'"
              s1 s2 ) )
  | WrongIndex (t, e) ->
      let s1 = string_of_typ t and s2 = string_of_expr e in
      raise
        (TypeError
           (Printf.sprintf
              "Type Error: Expected index to be an int, but got '%s' in '%s'"
              s1 s2 ) )
  | WrongSliceIndex (t1, t2, e1, e2) ->
      let s1 = string_of_typ t1
      and s2 = string_of_typ t2
      and s3 = string_of_expr e1
      and s4 = string_of_expr e2 in
      raise
        (TypeError
           (Printf.sprintf
              "Type Error: Expected left and right values of slice \
               expression to be of type int, but got type '%s' and '%s' in \
               '%s' and '%s'"
              s1 s2 s3 s4 ) )
  | ReturnNotLast ->
      raise
        (TypeError
           (Printf.sprintf
              "Error: There are unreacheable statements after return" ) )
  | ReturnOutsideFunction ->
      raise
        (TypeError
           (Printf.sprintf "Error: Return statement is outside of a function")
        )
  | ReturnMismatchedTypes (t1, t2, s) ->
      let s1 = string_of_typ t1
      and s2 = string_of_typ t2
      and s3 = string_of_stmt s in
      raise
        (TypeError
           (Printf.sprintf
              "Type error: Expected value of type '%s', but got a value of \
               type '%s' in '%s'"
              s1 s2 s3 ) )
  | e -> raise (TypeError (Printexc.to_string e))
