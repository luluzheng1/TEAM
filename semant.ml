(* Semantic checking for the TEAM compiler *)

open Ast
open Sast
open Exceptions
module StringMap = Map.Make (String)

let check (functions, statements) =
  let built_in_decls = 
    let add_bind map (name, formalTypes, returnType) = StringMap.add name {
        typ = returnType; fname = name; 
        formals = formalTypes; body = [] } map
      in List.fold_left add_bind StringMap.empty [("print", [(String, "x")], Void);
                                ("open", [(String, "file_name"); (String, "mode")], File);
                                ("readline", [(File, "file_handle")], String);
                                ("write", [(File, "file_handle"); (String, "content")], Void);
                                ("close", [(File, "file_handle")], Void);
                                ("length", [(Unknown, "input_list")], Int);
                                ("append", [(List of Unknown, "input_list"), List of Unknown])]
  in
  let add_func map fd =
    let n = fd.fname in (* Name of the function *)
    match fd with
    | _ when StringMap.mem n built_in_decls ->
        raise (CannotRedefineBuiltIn fd.fname)
    | _ when StringMap.mem n map -> raise (AlreadyDefined fd.fname)
    | _ -> StringMap.add n fd map
  in

  (* TODO: need to work on function_decls *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (UndefinedFunction s)
  in

  (* TODO function_check *)

  let variable_table = {variables= StringMap.empty; parent= None} in
  (* Create a reference to the global table. The scope will be passed through
     recurisve calls and be mutated when we need to add a new variable *)
  let global_scope = ref variable_table in
  (* Finding a variable, beginning in a given scope and searching upwards *)
  let rec type_of_identifier (scope : symbol_table ref) name =
    try StringMap.find name !scope.variables
    with Not_found -> (
      match !scope.parent with
      | Some parent -> type_of_identifier (ref parent) name
      | _ -> raise (UndefinedId name) )
  in
  (* Raise an exception if the given rvalue type cannot be assigned to the
     given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise err
  in
  let rec expr scope exp =
    match exp with
    | IntLit l -> (Int, SIntLit l)
    | FloatLit l -> (Float, SFloatLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | CharLit l -> (Char, SCharLit l)
    | StringLit l -> (String, SStringLit l)
    | ListLit es -> (
        let ts = List.map (fun x -> fst (expr scope x)) es in
        let ty = List.hd ts in
        let check_type e =
          let ty', e' = expr scope e in
          if ty' = ty then (ty', e')
          else raise (NonUniformTypeContainer (ty, ty'))
        in
        match ts with
        | [] -> (Unknown, SListLit [])
        | x :: xs -> (ty, SListLit (List.map check_type es)) )
    | Id s -> (type_of_identifier scope s, SId s)
    | Binop (e1, op, e2) as e ->
        let t1, e1' = expr scope e1 and t2, e2' = expr scope e2 in
        let same = t1 = t2 in
        let ty =
          match op with
          | (Add | Sub | Mult | Div | Mod) when same && t1 = Int ->
              Int (* TODO: Does Add operate on strings? *)
          | (Add | Sub | Mult | Div) when same && t1 = Float -> Float
          | (Add | Sub | Mult | Div) when t1 = Int && t2 = Float -> Float
          | (Add | Sub | Mult | Div) when t1 = Float && t2 = Int -> Float
          | Exp when same && t1 = Int -> Int
          | Exp when same && t1 = Float -> Float
          | Exp when t1 = Int && t2 = Float -> Float
          | Exp when t2 = Float && t2 = Int -> Float
          | (Equal | Neq) when same -> Bool
          | (Less | Leq | Greater | Geq) when same && (t1 = Int || t1 = Float)
            ->
              Bool
          | (And | Or) when same && t1 = Bool -> Bool
          | Range when same && t1 = Int -> List Int
          | _ -> raise (InvalidBinaryOperation (t1, op, t2, e))
        in
        (ty, SBinop ((t1, e1'), op, (t2, e2')))
    | Unop (op, e) as ex ->
        let t, e' = expr scope e in
        let ty =
          match op with
          | Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (InvalidUnaryOperation (t, op, ex))
        in
        (ty, SUnop (op, (t, e')))
    | Assign (s, e) as ex ->
        let lt = type_of_identifier scope s and rt, e' = expr scope e in
        ( check_assign lt rt (IllegalAssignment (lt, None, rt, ex))
        , SAssign (s, (rt, e')) )
    | ListAssign (s, e1, e2) as ex ->
        let lt = type_of_identifier scope s
        and t1, e1' = expr scope e1
        and t2, e2' = expr scope e2 in
        let inner_ty =
          match lt with
          | List ty -> ty
          | other -> raise (NonListAccess (t1, other, ex))
        in
        let is_index =
          match t1 with
          | Int -> true
          | other -> raise (InvalidIndex (other, ex))
        in
        if is_index && inner_ty = t2 then
          (List inner_ty, SListAssign (s, (t1, e1'), (t2, e2')))
        else raise (MismatchedTypes (inner_ty, t2, ex))
    | AssignOp (s, op, e) as ex ->
        let lt = type_of_identifier scope s and rt, e' = expr scope e in
        let same = lt = rt in
        let ty =
          match op with
          | (Add | Sub | Mult | Div) when same && (lt = Int || lt = Float) ->
              lt
          | Mod when same && lt = Int -> Int
          | _ -> raise (IllegalAssignment (lt, Some op, rt, ex))
        in
        (ty, SAssignOp (s, op, (rt, e')))
    | Call (fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (WrongNumberOfArgs (param_length, List.length args, call))
        else
          let check_call (ft, _) e =
            let et, e' = expr scope e in
            (check_assign ft et (IllegalArgument (et, ft, e)), e')
          in
          let args' = List.map2 check_call fd.formals args in
          (fd.typ, SCall (fname, args'))
    | SliceExpr (id, slce) as slice ->
        let lt = type_of_identifier scope id in
        let check_slice_expr =
          match slce with
          | Index e ->
              let t, e' = expr scope e in
              let id_type =
                match lt with
                | List ty -> ty
                | String -> Char
                | _ -> raise (IllegalSlice (slice, lt))
              in
              if t = Int then (id_type, SSliceExpr (id, SIndex (t, e')))
              else raise (WrongIndex (t, e))
          | Slice (e1, e2) ->
              let t1, e1' = expr scope e1 and t2, e2' = expr scope e2 in
              let id_type =
                match lt with
                | List _ -> lt
                | String -> lt
                | _ -> raise (IllegalSlice (slice, lt))
              in
              if t1 = Int && t1 = t2 then
                (id_type, SSliceExpr (id, SSlice ((t1, e1'), (t2, e2'))))
              else raise (WrongSliceIndex (t1, t2, e1, e2))
        in
        check_slice_expr
    | End -> (Int, SEnd)
    | Noexpr -> (Void, SNoexpr)
    | _ -> raise (Failure "Not Yet Implemented")
  in
  let check_bool_expr scope e =
    let t', e' = expr scope e in
    if t' != Bool then raise (MismatchedTypes (t', Bool, e)) else (t', e')
  in
  (* check duplicate var declaration *)
  let check_var_duplicate scope name =
    match name with
    | _ when StringMap.mem name !scope.variables -> raise (Duplicate name)
    | _ -> name
  in
  (* check void type variable *)
  let check_void_type ty name =
    match ty with Void -> raise (VoidType name) | _ -> ty
  in
  let rec check_stmt scope stmt =
    match stmt with
    | Expr e -> SExpr (expr scope e)
    | If (p, b1, b2, b3) ->
        SIf
          ( check_bool_expr scope p
          , check_stmt scope b1
          , check_stmt scope b2
          , check_stmt scope b3 )
    | For (e1, e2, st) ->
        SFor (expr scope e1, expr scope e2, check_stmt scope st)
    | While (p, s) -> SWhile (check_bool_expr scope p, check_stmt scope s)
    | Block sl ->
        let rec check_stmt_list = function
        | Block sl :: ss -> check_stmt_list (sl @ ss)
        | s :: ss -> check_stmt scope s :: check_stmt_list ss
        | [] -> []
      in
      SBlock (check_stmt_list sl)
  | Declaration (ty, s, e) -> raise (Failure "Not Yet Implemented")
  | _ -> SExpr (Void, SNoexpr)
in
let check_stmts stmt = check_stmt global_scope stmt in
let statements' =
  try List.map check_stmts statements with e -> handle_error e
in
([], statements')











  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void; fname = name; 
      formals = [(ty, "x")];
      locals = []; body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", String);
			                         ("open", String);
			                         ("close", String);
			                         ("len", String)]
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all other function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    let formals' = check_binds "formal" func.formals in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                  StringMap.empty (globals' @ formals' )
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
  

in ([], List.map check_stmt statements)




          