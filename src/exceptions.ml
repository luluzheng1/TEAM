(* Authors: Naoki O., Yingjie L., Lulu Z., Saurav G. *)
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

exception UndefinedFunction

exception WrongNumberOfArgs of int * int * expr

exception PrintMissingArgs of expr

exception PrintBadArgs of char

exception PrintWrongType of expr

exception PrintWrongNumArgs of int * int

exception PrintTypeError of typ * typ

exception IllegalArgument of typ * typ * expr

exception IllegalSlice of expr * typ

exception WrongIndex of typ * expr

exception WrongSliceIndex of typ * typ * expr * expr

exception ReturnNotLast

exception ReturnOutsideFunction

exception NoReturnInNonVoidFunction

exception ReturnMismatchedTypes of typ * typ * stmt

exception NotInLoop of string

exception AppendNonList of typ

exception LengthWrongArgument of typ

exception UnsupportedPrint of typ

exception AssignNonVar of expr

exception IllegalFname

exception IllegalFor

(* Resolver Exceptions *)
exception IllegalSSlice

(* Codegen Exceptions *)
exception InvalidFloatBinop

exception InvalidIntBinop

exception InvalidStringBinop

exception NotFound of string

exception ImpossibleElif

let handle_error (e : exn) =
  match e with
  | NonUniformTypeContainer (t1, t2) ->
      let s1 = string_of_typ t1 and s2 = string_of_typ t2 in
      raise
        (TypeError
           (Printf.sprintf
              "Type error: Lists can only contain one type. Expected '%s', but \
               got '%s'"
              s1 s2 ) )
  | UndefinedId n ->
      raise
        (TypeError
           (Printf.sprintf "Error: variable '%s' was used before it was defined"
              n ) )
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
              "Type error: Illegal binary operator '%s' '%s' '%s' in '%s'" s1 s2
              s3 s4 ) )
  | InvalidUnaryOperation (t, op, e) ->
      let s1 = string_of_typ t
      and s2 = string_of_uop op
      and s3 = string_of_expr e in
      raise
        (TypeError
           (Printf.sprintf "Error: Illegal unary operator '%s' '%s' in '%s'" s2
              s1 s3 ) )
  | IllegalAssignment (t1, op, t2, e) ->
      let s1 = string_of_typ t1
      and s3 = string_of_typ t2
      and s4 = string_of_expr e in
      let s2 = match op with Some x -> string_of_op x | None -> "" in
      raise
        (TypeError
           (Printf.sprintf "Error: Illegal assignment '%s' '%s'= '%s' in '%s'"
              s1 s2 s3 s4 ) )
  | IllegalDeclaration (t1, t2, s) ->
      let s1 = string_of_typ t1
      and s2 = string_of_typ t2
      and s3 = string_of_stmt s in
      raise
        (TypeError
           (Printf.sprintf "Error: Illegal declaration '%s' '%s' in '%s'" s1 s2
              s3 ) )
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
              "Type Error: Expected index of type Int, but got type '%s' in \
               '%s'"
              s1 s2 ) )
  | CannotRedefineBuiltIn s ->
      raise
        (TypeError
           (Printf.sprintf
              "Error: Function '%s' may not be defined as it exists as a built \
               in function"
              s ) )
  | AlreadyDefined s ->
      raise
        (TypeError
           (Printf.sprintf
              "Error: '%s' cannot be redefined in the current scope" s ) )
  | VoidType n ->
      raise
        (TypeError
           (Printf.sprintf "Type Error: variable '%s' cannot have type Void" n)
        )
  | Duplicate n ->
      raise
        (TypeError
           (Printf.sprintf "Error: variable name '%s' has already defined" n) )
  | UndefinedFunction ->
      raise
        (TypeError
           (Printf.sprintf "Error: function was called, but it is undefined") )
  | WrongNumberOfArgs (exp, act, e) ->
      let s1 = string_of_int exp
      and s2 = string_of_int act
      and s3 = string_of_expr e in
      raise
        (TypeError
           (Printf.sprintf "Error: expected '%s' arguments but got '%s' in '%s'"
              s1 s2 s3 ) )
  | PrintMissingArgs e ->
      let s1 = string_of_expr e in
      raise
        (TypeError
           (Printf.sprintf
              "Error: expected more than 0 arguments but got 0 in '%s'" s1 ) )
  | PrintBadArgs e ->
      raise
        (TypeError
           (Printf.sprintf "Error: expected either c, f or i but got '%c'" e) )
  | PrintWrongType e ->
      let s1 = string_of_expr e in
      raise (TypeError (Printf.sprintf "Error: expected string but got '%s'" s1))
  | PrintWrongNumArgs (i1, i2) ->
      let s1 = string_of_int i1 and s2 = string_of_int i2 in
      raise
        (TypeError
           (Printf.sprintf "Error: expected %s addition arguments, but got %s"
              s1 s2 ) )
  | PrintTypeError (t1, t2) ->
      let s1 = string_of_typ t1 and s2 = string_of_typ t2 in
      raise
        (TypeError
           (Printf.sprintf
              "Type Error: expected argument of type '%s' but got '%s' instead"
              s1 s2 ) )
  | IllegalArgument (t1, t2, e) ->
      let s1 = string_of_typ t1
      and s2 = string_of_typ t2
      and s3 = string_of_expr e in
      raise
        (TypeError
           (Printf.sprintf
              "Type Error: Illegal argument found in '%s'. Expected argument \
               of type '%s' but got '%s'"
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
              "Type Error: Expected index to be an int, but got '%s' in '%s'" s1
              s2 ) )
  | WrongSliceIndex (t1, t2, e1, e2) ->
      let s1 = string_of_typ t1
      and s2 = string_of_typ t2
      and s3 = string_of_expr e1
      and s4 = string_of_expr e2 in
      raise
        (TypeError
           (Printf.sprintf
              "Type Error: Expected left and right values of slice expression \
               to be of type int, but got type '%s' and '%s' in '%s' and '%s'"
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
  | NoReturnInNonVoidFunction ->
      raise
        (TypeError
           (Printf.sprintf
              "Error: No return statement in function returning non-void" ) )
  | ReturnMismatchedTypes (t1, t2, s) ->
      let s1 = string_of_typ t1
      and s2 = string_of_typ t2
      and s3 = string_of_stmt s in
      raise
        (TypeError
           (Printf.sprintf
              "Type Error: Expected value of type '%s', but got a value of \
               type '%s' in '%s'"
              s1 s2 s3 ) )
  | InvalidFloatBinop ->
      raise
        (Failure
           "Internal Error: Invalid operation on float. Semant should have \
            rejected this" )
  | InvalidIntBinop ->
      raise
        (Failure
           "Internal Error: Invalid operation on int. Semant should have \
            rejected this" )
  | InvalidStringBinop ->
      raise
        (Failure
           "Internal Error: Invalid operation on string. Semant should have \
            rejected this" )
  | NotFound s ->
      raise
        (Failure (Printf.sprintf "Internal Error: Variable '%s' not in scope" s))
  | ImpossibleElif ->
      raise
        (Failure
           (Printf.sprintf
              "Internal Error: Corrupted Tree. Semant should have rejected this" )
        )
  | NotInLoop s ->
      raise
        (Failure
           (Printf.sprintf
              "Error: Expected '%s' to be in a loop, but it was not" s ) )
  | AppendNonList t ->
      let s = string_of_typ t in
      raise
        (Failure
           (Printf.sprintf
              "Type Error: Expected first argument to append to be of type \
               list, but got type '%s' instead"
              s ) )
  | LengthWrongArgument t ->
      let s = string_of_typ t in
      raise
        (Failure
           (Printf.sprintf
              "Type Error: Expected argument to length to be of type list or \
               string, but got type '%s' instead"
              s ) )
  | UnsupportedPrint t ->
      let s = string_of_typ t in
      raise
        (Failure
           (Printf.sprintf
              "Type Error: Print does not support printing for type '%s'" s ) )
  | AssignNonVar e ->
      let s = string_of_expr e in
      raise
        (Failure
           (Printf.sprintf
              "Error: Cannot assign to a non variable or non slice expression \
               in '%s'"
              s ) )
  | IllegalFname ->
      raise (Failure (Printf.sprintf "Error: Function name is not an SId"))
  | IllegalFor ->
      raise
        (Failure (Printf.sprintf "For loop can only operate on string or list"))
  | IllegalSSlice ->
      raise
        (Failure
           (Printf.sprintf
              "Internal Error: Illegal Slice, should have been rejected in \
               Semant" ) )
  | e -> raise (TypeError (Printexc.to_string e))
