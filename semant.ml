(* Semantic checking for the TEAM compiler *)

open Ast
open Sast
module StringMap = Map.Make (String)
open List
module E = Exceptions

(* Semantic checking of the AST. Returns an SAST if successful, throws an
   exception if something is wrong. *)

let check (functions, statements) =
  let func_ty fd =
    let param_types = List.map (fun (a, _) -> a) fd.formals in
    Func (param_types, fd.typ)
  in
  let built_in_decls =
    let add_bind map (name, formalTypes, returnType) =
      StringMap.add name
        (func_ty {typ= returnType; fname= name; formals= formalTypes; body= []})
        map
    in
    List.fold_left add_bind StringMap.empty
      [ ("print", [(Unknown, "x")], Void)
      ; ("open", [(String, "file_name"); (String, "mode")], File)
      ; ("readline", [(File, "file_handle")], String)
      ; ("write", [(File, "file_handle"); (String, "content")], Void)
      ; ("close", [(File, "file_handle")], Void)
      ; ("match", [(String, "target"); (String, "regex")], Bool)
      ; ("find", [(String, "target"); (String, "regex")], String)
      ; ( "replace"
        , [ (String, "target")
          ; (String, "regex")
          ; (String, "replace")
          ; (Int, "count") ]
        , String )
      ; ( "replaceall"
        , [(String, "target"); (String, "regex"); (String, "replace")]
        , String )
      ; ("findall", [(String, "target"); (String, "regex")], List String)
      ; ( "append"
        , [(List Unknown, "input_list"); (Unknown, "element")]
        , List Unknown )
      (* ; ("contains", [(Unknown, "input_list"); (Unknown, "element")], Bool) *)
      ; ( "insert"
        , [(List Unknown, "input_list"); (Unknown, "element"); (Int, "index")]
        , List Unknown )
      ; ("length", [(Unknown, "input_list")], Int)
      ; ("reverse", [(List Unknown, "input_list")], List Unknown) ]
  in
  (* fd.typ *)
  let add_func map fd =
    let n = fd.fname in
    (* Name of the function *)
    match fd with
    | _ when StringMap.mem n built_in_decls ->
        raise (E.CannotRedefineBuiltIn fd.fname)
    | _ when StringMap.mem n map -> raise (E.AlreadyDefined fd.fname)
    | _ -> StringMap.add n (func_ty fd) map
  in
  let function_decls = List.fold_left add_func built_in_decls functions in
  let variable_table =
    { variables= function_decls
    ; functions
    ; list_variables= StringMap.empty
    ; parent= None }
  in
  (* Create a reference to the global table. The scope will be passed through
     recurisve calls and be mutated when we need to add a new variable *)
  let global_scope = ref variable_table in
  let check_binds (to_check : bind list) =
    let name_compare (_, n1) (_, n2) = compare n1 n2 in
    let check_it checked binding =
      match binding with
      (* No void bindings *)
      | Void, name -> raise (E.VoidType name)
      | _, n1 -> (
        match checked with
        (* No duplicate bindings *)
        | (_, n2) :: _ when n1 = n2 -> raise (E.Duplicate n2)
        | _ -> binding :: checked )
    in
    let _ = List.fold_left check_it [] (List.sort name_compare to_check) in
    to_check
  in
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
        ; functions= !scope.functions
        ; list_variables= !scope.list_variables
        ; parent= !scope.parent }
  in
  let update_var (scope : symbol_table ref) id ty =
    scope :=
      { variables= StringMap.add id ty !scope.variables
      ; functions= !scope.functions
      ; list_variables= !scope.list_variables
      ; parent= !scope.parent }
  in
  let add_list_scope id (scope : symbol_table ref) =
    try
      let _ = StringMap.find id !scope.variables in
      raise (E.Duplicate id)
    with Not_found ->
      scope :=
        { variables= !scope.variables
        ; functions= !scope.functions
        ; list_variables= StringMap.add id scope !scope.list_variables
        ; parent= !scope.parent }
  in
  (* For finding a list outside of current scope that hasn't been type inferred *)
  let get_list_scope id scope = StringMap.find_opt id !scope.list_variables in
  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet
    else
      let ret =
        match (lvaluet, rvaluet) with
        | List Unknown, List ty -> List ty
        | List ty, List Unknown -> List ty
        | Void, List Unknown -> List Unknown
        | ty, Unknown -> ty
        | Unknown, ty -> ty
        | _ -> raise err
      in
      ret
  in
  let rec innermost_ty ty =
    match ty with List t -> innermost_ty t | nonlist_ty -> nonlist_ty
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
          | (Add) when t1 = String && t2 = String -> String
          | Exp when same && t1 = Int -> Int
          | Exp when same && t1 = Float -> Float
          | Exp when t1 = Int && t2 = Float -> Float
          | Exp when t1 = Float && t2 = Int -> Float
          | (Equal | Neq) when t1 = Int && t2 = Float -> Bool
          | (Equal | Neq) when t1 = Float && t2 = Int -> Bool
          | (Equal | Neq) when same -> Bool
          | (Equal | Neq) when t1 = Unknown || t2 = Unknown -> Bool
          | (Less | Leq | Greater | Geq) when t1 = Int && t2 = Float -> Bool
          | (Less | Leq | Greater | Geq) when t1 = Float && t2 = Int -> Bool
          | (Less | Leq | Greater | Geq) when same && t1 = Int -> Bool
          | (Less | Leq | Greater | Geq) when same && t1 = Float -> Bool
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
        let lt, s' = expr scope s in
        let rt, e' = expr scope e in
        let _ =
          match s' with
          | SId _ | SSliceExpr _ -> ()
          | _ -> raise (Failure "Can't assign to type")
        in
        let lrt = check_assign lt rt (E.IllegalAssignment (lt, None, rt, ex)) in
        let s_name =
          match s with
          | Id n -> n
          | SliceExpr (Id n, _) -> n
          | _ -> raise (Failure "LHS is not a variable")
        in
        let is_slice = match s with SliceExpr _ -> true | _ -> false in
        let non_slice =
          match (lt, rt) with
          | List _, List _ | Void, List _ ->
              let _ = update_var scope s_name lrt in
              let _ =
                match get_list_scope s_name scope with
                | Some sc -> update_var sc s_name lrt
                | None -> ()
              in
              (lrt, SAssign ((lrt, s'), (rt, e')))
          | _ -> (lrt, SAssign ((lt, s'), (rt, e')))
        in
        if is_slice then
          let _ = update_var scope s_name lt in
          (lt, SAssign ((lt, s'), (rt, e')))
        else non_slice
    | Call (fname, args) as call -> (
        let check_length frmls =
          if List.length args != List.length frmls then
            raise
              (E.WrongNumberOfArgs (List.length frmls, List.length args, call))
          else ()
        in
        let fty, fname' = expr scope fname in
        let formals, ret_type =
          match fty with
          | Func (f, r) -> (f, r)
          | _ -> raise (Failure "Not a function")
        in
        let _ = if fname <> Id "print" then check_length formals else () in
        match fname with
        | Id "print" ->
            let et, _ = expr scope (hd args) in
            let _ =
              match et with
              | String -> ()
              | _ ->
                  raise
                    (Failure
                       ( "Print does not support printing for type"
                       ^ string_of_typ et ) )
            in
            ( Void
            , SCall
                ((Func ([et], Void), SId "print"), List.map (expr scope) args)
            )
        | Id "append" ->
            let args' = List.map (expr scope) args in
            let et1, args1' = hd args' in
            let et2, args2' = hd (tl args') in
            let inner_ty =
              match et1 with List ty -> ty | _ -> raise (E.AppendNonList et2)
            in
            let is_list et = match et with List _ -> true | _ -> false in
            let _ =
              if
                is_list et2
                && innermost_ty et2 = Unknown
                && innermost_ty et1 <> Unknown
              then ()
              else if innermost_ty et1 <> et2 && innermost_ty et1 != Unknown
              then raise (E.MismatchedTypes (inner_ty, et2, call))
              else ()
            in
            if innermost_ty et1 = Unknown then
              ( List et2
              , SCall
                  ( (Func ([List et2; et2], List et2), SId "append")
                  , [(List et2, args1'); (et2, args2')] ) )
            else if is_list et2 && innermost_ty et2 = Unknown then
              ( List et1
              , SCall
                  ( (Func ([List et1; et1], List et1), SId "append")
                  , [(List et1, args1'); (et1, args2')] ) )
            else
              ( List et2
              , SCall ((Func ([List et2; et2], List et2), SId "append"), args')
              )
        | Id "reverse" ->
            let args' = List.map (expr scope) args in
            let et1, _ = hd args' in
            let _ =
              match et1 with
              | List _ -> ()
              (* | String -> () *)
              | _ -> raise (E.LengthWrongArgument et1)
            in
            (et1, SCall ((Func ([et1], et1), SId "reverse"), args'))
        | Id "insert" ->
            let args' = List.map (expr scope) args in
            let et1, _ = hd args' in
            let et2, _ = hd (tl args') in
            let inner_ty =
              match et1 with List ty -> ty | _ -> raise (E.AppendNonList et2)
            in
            let ret =
              if inner_ty <> et2 then
                raise (E.MismatchedTypes (inner_ty, et2, call))
              else
                ( et1
                , SCall
                    ( ( Func ([List inner_ty; inner_ty], List inner_ty)
                      , SId "insert" )
                    , args' ) )
            in
            ret
        | Id "length" ->
            let args' = List.map (expr scope) args in
            let et1, _ = hd args' in
            let _ =
              match et1 with
              | List _ -> ()
              | String -> ()
              | _ -> raise (E.LengthWrongArgument et1)
            in
            (Int, SCall ((Func ([List Int], Int), SId "length"), args'))
        (* | Id "contains" ->
            let args' = List.map (expr scope) args in
            let et1, _ = hd args' in
            let inner_et1_ty = innermost_ty et1 in
            let et2, _ = hd (tl args') in
            let _ =
              if inner_et1_ty <> et2 then
                raise (E.MismatchedTypes (et1, List et2, call))
              else ()
            in
            let _ =
              match et2 with
              | Int | Float | Bool | String | Char -> ()
              | _ ->
                  raise
                    (Failure ("Not Yet Supported on typ" ^ string_of_typ et2))
            in
            (Bool, SCall ((Func ([List et1], et2), SId "contains"), args')) *)
        | _ ->
            let check_call ft e =
              let et, e' = expr scope e in
              (check_assign ft et (E.IllegalArgument (ft, et, e)), e')
            in
            let args' = List.map2 check_call formals args in
            let arg_types = List.map (fun e -> fst (expr scope e)) args in
            (* get all list type arguments *)
            let list_args =
              List.filter
                (fun x -> match x with List _ -> true | _ -> false)
                arg_types
            in
            if length list_args > 0 then
              let inner_types =
                List.map
                  (fun t -> match t with List inner_ty -> inner_ty | ty -> ty)
                  arg_types
              in
              (* convert list types to strings *)
              let type_specific_name =
                String.concat "_"
                  (List.map (fun t -> string_of_typ t) inner_types)
              in
              let func_name =
                match fname with
                | Id s -> s
                | _ -> raise (Failure "Not a function name")
              in
              (* look up the old function *)
              let look_up_func =
                List.find_opt (fun f -> f.fname = func_name) functions
              in
              let generic_func =
                match look_up_func with
                | Some f -> f
                | None -> raise (Failure "Could not find function")
              in
              let new_fname =
                String.concat "_" [generic_func.fname; type_specific_name]
              in
              let new_func_created =
                List.find_opt
                  (fun f -> f.fname = new_fname)
                  !global_scope.functions
              in
              if Option.is_none new_func_created then
                (* make a copy of the new function where type is resolved to a
                   specific kind of list *)
                let modified_func =
                  { typ= ret_type
                  ; fname= new_fname
                  ; formals=
                      List.combine arg_types (List.map snd generic_func.formals)
                  ; body= generic_func.body }
                in
                let _ =
                  global_scope :=
                    { variables= !global_scope.variables
                    ; functions=
                        modified_func
                        ::
                        List.filter
                          (fun f -> f.fname != generic_func.fname)
                          !global_scope.functions
                    ; list_variables= !scope.list_variables
                    ; parent= !global_scope.parent }
                in
                ( ret_type
                , SCall ((Func (arg_types, ret_type), SId new_fname), args') )
              else
                ( ret_type
                , SCall ((Func (arg_types, ret_type), SId new_fname), args') )
            else (ret_type, SCall ((fty, fname'), args')) )
    | SliceExpr (lexpr, slce) as slice ->
        let lt, lexpr' = expr scope lexpr in
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
              if t = Int then
                (id_type, SSliceExpr ((lt, lexpr'), SIndex (t, e')))
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
                ( id_type
                , SSliceExpr ((lt, lexpr'), SSlice ((t1, e1'), (t2, e2'))) )
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
  (* Checks if there are any statements after return *)
  let rec check_return sl typ =
    match sl with
    | [] -> if typ != Void then raise E.NoReturnInNonVoidFunction else ()
    | [Return _] -> ()
    | Return _ :: _ -> raise E.ReturnNotLast
    | _ :: ss -> check_return ss typ
  in
  (* Return a semantically-checked statement containing exprs *)
  let rec check_stmt scope stmt loop fdecl =
    match stmt with
    | Expr e -> SExpr (expr scope e)
    | Block sl ->
        let new_scope =
          { variables= StringMap.empty
          ; functions= !scope.functions
          ; list_variables= !scope.list_variables
          ; parent= Some !scope }
        in
        let new_scope_ref = ref new_scope in
        let rec check_stmt_list = function
          | Block sl :: ss -> check_stmt_list (sl @ ss)
          | s :: ss ->
              check_stmt new_scope_ref s loop fdecl :: check_stmt_list ss
          | [] -> []
        in
        SBlock (List.rev (check_stmt_list (List.rev sl)))
    | Return e as return -> (
      match fdecl.fname with
      | "toplevel" -> raise E.ReturnOutsideFunction
      | _ ->
          let t, e' = expr scope e in
          let is_return_list =
            match (t, fdecl.typ) with
            | List _, List Unknown -> true
            | _ -> false
          in
          if is_return_list then
            let look_up_func =
              (* Look up functions in the symbol table *)
              List.find_opt (fun f -> f.fname = fdecl.fname) !scope.functions
            in
            let func =
              match look_up_func with
              | Some f -> f
              | None -> raise (Failure "Could not find function")
            in
            let modified_func =
              {typ= t; fname= func.fname; formals= func.formals; body= func.body}
            in
            let _ =
              global_scope :=
                { variables= !global_scope.variables
                ; functions=
                    modified_func
                    ::
                    List.filter
                      (fun f -> f.fname != func.fname)
                      !global_scope.functions
                ; list_variables= !scope.list_variables
                ; parent= !global_scope.parent }
            in
            SReturn (t, e')
          else if t = fdecl.typ then SReturn (t, e')
          else raise (E.ReturnMismatchedTypes (fdecl.typ, t, return)) )
    | If (p, b1, b2) ->
        SIf
          ( check_bool_expr scope p
          , check_stmt scope b1 loop fdecl
          , check_stmt scope b2 loop fdecl )
    | For (s, e, st) ->
        let t, e' = expr scope e in
        let s_ty =
          match t with
          | List ty -> ty
          | String -> Char
          | _ -> raise (Failure "for loop can only take string or list")
        in
        let _ = add_var_to_scope scope s s_ty in
        let sexpr = SFor (s, (t, e'), check_stmt scope st (loop + 1) fdecl) in
        let _ =
          scope :=
            { variables= StringMap.remove s !scope.variables
            ; functions= !scope.functions
            ; list_variables= !scope.list_variables
            ; parent= !scope.parent }
        in
        sexpr
    | While (p, s) ->
        SWhile (check_bool_expr scope p, check_stmt scope s (loop + 1) fdecl)
    | Declaration (ty, s, e) as decl ->
        let expr_ty, e' = expr scope e in
        let _ = check_void_type ty s in
        let same = expr_ty = ty in
        let is_generic_list =
          match expr_ty with
          | List Unknown -> true
          | List _ -> false
          | _ -> false
        in
        if (same && not is_generic_list) || e' = SNoexpr then
          let _ = add_var_to_scope scope s ty in
          SDeclaration (ty, s, (expr_ty, e'))
        else if ty = List Unknown && not is_generic_list then
          let _ = add_var_to_scope scope s expr_ty in
          SDeclaration (expr_ty, s, (expr_ty, e'))
        else
          let _ =
            match (expr_ty, e') with
            | List Unknown, _ ->
                let _ = add_list_scope s scope in
                add_var_to_scope scope s ty
            | _ -> raise (E.IllegalDeclaration (ty, expr_ty, decl))
          in
          SDeclaration (ty, s, (expr_ty, e'))
    | Break -> if loop > 0 then SBreak else raise (E.NotInLoop "Break")
    | Continue -> if loop > 0 then SContinue else raise (E.NotInLoop "Continue")
  in
  let check_functions func =
    let formals' = check_binds func.formals in
    let add_formal map (ty, name) = StringMap.add name ty map in
    let func_variable_table =
      { variables= List.fold_left add_formal StringMap.empty formals'
      ; functions= !global_scope.functions
      ; list_variables= !global_scope.list_variables
      ; parent= Some !global_scope }
    in
    let func_scope = ref func_variable_table in
    let _ = check_return func.body func.typ in
    let body' = check_stmt func_scope (Block func.body) 0 func in
    if func.typ = List Unknown then
      let updated_func =
        List.filter (fun f -> f.fname = func.fname) !global_scope.functions
      in
      let _ =
        if length updated_func = 0 then raise (Failure "Function not found")
        else ()
      in
      (* func.typ should be updated styp *)
      { styp= (hd updated_func).typ
      ; sfname= func.fname
      ; sformals= formals'
      ; sbody= [body'] }
    else {styp= func.typ; sfname= func.fname; sformals= formals'; sbody= [body']}
  in
  let check_stmts stmt = check_stmt global_scope stmt 0 dummy in
  let statements' =
    try List.map check_stmts statements with e -> E.handle_error e
  in
  let functions' =
    try List.map check_functions !global_scope.functions
    with e -> E.handle_error e
  in
  (functions', statements')
