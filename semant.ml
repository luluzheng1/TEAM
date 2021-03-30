(* Semantic checking for the TEAM compiler *)

open Ast
open Sast
module StringMap = Map.Make (String)
module E = Exceptions

(* Semantic checking of the AST. Returns an SAST if successful, throws an
   exception if something is wrong. *)
let check (functions, statements) =
  (* TODO: Need to work on built_in_decls *)
  let built_in_decls = StringMap.empty in
  (* Add function name to symbol table *)
  let add_func map fd =
    let n = fd.fname in
    match fd with
    | _ when StringMap.mem n built_in_decls ->
        raise (E.CannotRedefineBuiltIn fd.fname)
    | _ when StringMap.mem n map -> raise (E.AlreadyDefined fd.fname)
    | _ -> StringMap.add n fd map
  in
  (* TODO: need to work on function_decls *)
  let function_decls = StringMap.empty in
  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (E.UndefinedFunction s)
  in
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
      | _ -> raise (E.UndefinedId name) )
  in
  let add_var_to_scope (scope : symbol_table ref) id ty =
    try
      let _ = StringMap.find id !scope.variables in
      raise (E.Duplicate id)
    with Not_found ->
      scope :=
        { variables= StringMap.add id ty !scope.variables
        ; parent= !scope.parent }
  in
  (* Raise an exception if the given rvalue type cannot be assigned to the
     given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise err
  in
  (* Return a semantically-checked expression with a type *)
  let rec expr scope exp =
    match exp with
    | IntLit l -> (Int, SIntLit l)
    | FloatLit l -> (Float, SFloatLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | CharLit l -> (Char, SCharLit l)
    | StringLit l -> (String, SStringLit l)
    | ListLit es -> (
        let ts = List.map (fun x -> fst (expr scope x)) es in
        match ts with
        | [] -> (List Unknown, SListLit [])
        | x :: _ ->
            let ty = List.hd ts in
            let check_type e =
              let ty', e' = expr scope e in
              if ty' = ty then (ty', e')
              else raise (E.NonUniformTypeContainer (ty, ty'))
            in
            (List x, SListLit (List.map check_type es)) )
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
          | Exp when t1 = Float && t2 = Int -> Float
          | (Equal | Neq) when same -> Bool
          | (Less | Leq | Greater | Geq) when same && (t1 = Int || t1 = Float)
            ->
              Bool
          | (And | Or) when same && t1 = Bool -> Bool
          | Range when same && t1 = Int -> List Int
          | _ -> raise (E.InvalidBinaryOperation (t1, op, t2, e))
        in
        (ty, SBinop ((t1, e1'), op, (t2, e2')))
    | Unop (op, e) as ex ->
        let t, e' = expr scope e in
        let ty =
          match op with
          | Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (E.InvalidUnaryOperation (t, op, ex))
        in
        (ty, SUnop (op, (t, e')))
    | Assign (s, e) as ex ->
        let lt = type_of_identifier scope s and rt, e' = expr scope e in
        ( check_assign lt rt (E.IllegalAssignment (lt, None, rt, ex))
        , SAssign (s, (rt, e')) )
    | ListAssign (s, e1, e2) as ex ->
        let lt = type_of_identifier scope s
        and t1, e1' = expr scope e1
        and t2, e2' = expr scope e2 in
        let inner_ty =
          match lt with
          | List ty -> ty
          | other -> raise (E.NonListAccess (t1, other, ex))
        in
        let is_index =
          match t1 with
          | Int -> true
          | other -> raise (E.InvalidIndex (other, ex))
        in
        if is_index && inner_ty = t2 then
          (List inner_ty, SListAssign (s, (t1, e1'), (t2, e2')))
        else raise (E.MismatchedTypes (inner_ty, t2, ex))
    | AssignOp (s, op, e) as ex ->
        let lt = type_of_identifier scope s and rt, e' = expr scope e in
        let same = lt = rt in
        let ty =
          match op with
          | (Add | Sub | Mult | Div) when same && (lt = Int || lt = Float) ->
              lt
          | (Add | Sub | Mult | Div) when lt = Float && rt = Int -> Float
          | Mod when same && lt = Int -> Int
          | _ -> raise (E.IllegalAssignment (lt, Some op, rt, ex))
        in
        (ty, SAssignOp (s, op, (rt, e')))
    | Call (fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (E.WrongNumberOfArgs (param_length, List.length args, call))
        else
          let check_call (ft, _) e =
            let et, e' = expr scope e in
            (check_assign ft et (E.IllegalArgument (et, ft, e)), e')
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
                | _ -> raise (E.IllegalSlice (slice, lt))
              in
              if t = Int then (id_type, SSliceExpr (id, SIndex (t, e')))
              else raise (E.WrongIndex (t, e))
          | Slice (e1, e2) ->
              let t1, e1' = expr scope e1 and t2, e2' = expr scope e2 in
              let id_type =
                match lt with
                | List _ -> lt
                | String -> lt
                | _ -> raise (E.IllegalSlice (slice, lt))
              in
              if t1 = Int && t1 = t2 then
                (id_type, SSliceExpr (id, SSlice ((t1, e1'), (t2, e2'))))
              else raise (E.WrongSliceIndex (t1, t2, e1, e2))
        in
        check_slice_expr
    | End -> (Int, SEnd)
    | Noexpr -> (Void, SNoexpr)
  in
  let check_bool_expr scope e =
    let t', e' = expr scope e in
    if t' != Bool then raise (E.MismatchedTypes (t', Bool, e)) else (t', e')
  in
  (* check void type variable *)
  let check_void_type ty name =
    match ty with Void -> raise (E.VoidType name) | _ -> ty
  in
  let dummy = {typ= Int; fname= "toplevel"; formals= []; body= []} in
  (* Return a semantically-checked statement containing exprs *)
  let rec check_stmt scope stmt fdecl =
    match stmt with
    | Expr e -> SExpr (expr scope e)
    | Block sl ->
        let new_scope = {variables= StringMap.empty; parent= Some !scope} in
        let new_scope_ref = ref new_scope in
        let rec check_stmt_list = function
          | [(Return _ as s)] -> [check_stmt new_scope_ref s fdecl]
          | Return _ :: _ -> raise E.ReturnNotLast
          | Block sl :: ss -> check_stmt_list (sl @ ss)
          | s :: ss -> check_stmt new_scope_ref s fdecl :: check_stmt_list ss
          | [] -> []
        in
        SBlock (check_stmt_list sl)
    | Return e as return -> (
      match fdecl.fname with
      | "toplevel" -> raise E.ReturnOutsideFunction
      | _ ->
          let t, e' = expr scope e in
          if t = fdecl.typ then SReturn (t, e')
          else raise (E.ReturnMismatchedTypes (fdecl.typ, t, return)) )
    | If (p, b1, b2, b3) ->
        SIf
          ( check_bool_expr scope p
          , check_stmt scope b1 fdecl
          , check_stmt scope b2 fdecl
          , check_stmt scope b3 fdecl )
    | For (e1, e2, st) ->
        SFor (expr scope e1, expr scope e2, check_stmt scope st fdecl)
    | While (p, s) ->
        SWhile (check_bool_expr scope p, check_stmt scope s fdecl)
    | Declaration (ty, s, e) as decl ->
        let expr_ty, e' = expr scope e in
        let _ = check_void_type ty s in
        let same = expr_ty = ty in
        if same then
          let _ = add_var_to_scope scope s ty in
          SDeclaration (ty, s, (expr_ty, e'))
        else
          let _ =
            match expr_ty with
            | List Unknown -> add_var_to_scope scope s ty
            | _ -> raise (E.IllegalDeclaration (ty, expr_ty, decl))
          in
          SDeclaration (ty, s, (expr_ty, e'))
    | Break -> SBreak
    | Continue -> SContinue
    | _ -> SExpr (Void, SNoexpr)
  in
  let check_stmts stmt = check_stmt global_scope stmt dummy in
  let statements' =
    try List.map check_stmts statements with e -> E.handle_error e
  in
  ([], statements')
