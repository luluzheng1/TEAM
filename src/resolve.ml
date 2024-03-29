(* Authors: Lulu Z. *)
open Ast
open Sast
module StringMap = Map.Make (String)
open List
module E = Exceptions

let resolve (functions, statements) =
  let variable_table : resolved_table =
    { rvariables= StringMap.empty
    ; rfunctions= functions
    ; rlist_variables= StringMap.empty
    ; rparent= None }
  in
  (* Create a reference to the global table. The scope will be passed through
     recurisve calls and be mutated when we need to add a new variable *)
  let global_scope : resolved_table ref = ref variable_table in
  (* Add a variable to the given scope *)
  let add_var (scope : resolved_table ref) id ty =
    scope :=
      { rvariables= StringMap.add id ty !scope.rvariables
      ; rfunctions= !scope.rfunctions
      ; rlist_variables= !scope.rlist_variables
      ; rparent= !scope.rparent }
  in
  (* Finding a variable, beginning in a given scope and searching upwards *)
  let rec type_of_identifier (scope : resolved_table ref) id =
    try StringMap.find id !scope.rvariables
    with Not_found -> (
      match !scope.rparent with
      | Some parent -> type_of_identifier (ref parent) id
      | _ -> raise (E.UndefinedId id) )
  in
  (* Add the scope of list variable with name id *)
  let add_list_scope id (scope : resolved_table ref) =
    scope :=
      { rvariables= !scope.rvariables
      ; rfunctions= !scope.rfunctions
      ; rlist_variables= StringMap.add id scope !scope.rlist_variables
      ; rparent= !scope.rparent }
  in
  (* For finding a list outside of current scope that hasn't been type inferred *)
  let get_list_scope id scope = StringMap.find_opt id !scope.rlist_variables in
  let look_up_func (funcs : sfunc_decl list) fname =
    List.find_opt (fun f -> f.sfname = fname) funcs
  in
  let func_ty fd =
    let param_types = List.map (fun (a, _) -> a) fd.sformals in
    Func (param_types, fd.styp)
  in
  let rec expr scope ((t, e) : sexpr) =
    match e with
    | SIntLit i -> (t, SIntLit i)
    | SFloatLit f -> (t, SFloatLit f)
    | SBoolLit b -> (t, SBoolLit b)
    | SCharLit c -> (t, SCharLit c)
    | SStringLit s -> (t, SStringLit s)
    | SId n -> (
      try
        if (type_of_identifier scope n, SId n) = (List Unknown, SId n) then
          (t, SId n)
        else (type_of_identifier scope n, SId n)
      with _ -> (t, SId n) )
    | SListLit l -> (t, SListLit l)
    | SSliceExpr (lexpr, slce) ->
        let lt, lexpr' = expr scope lexpr in
        let check_slice_expr =
          match slce with
          | SIndex e ->
              let t, e' = expr scope e in
              let id_type =
                match lt with
                | List ty -> ty
                | String -> Char
                | _ -> raise E.IllegalSSlice
              in
              (id_type, SSliceExpr ((lt, lexpr'), SIndex (t, e')))
          | SSlice _ -> (
              (* look up new inferred type from the symbol table *)
              let id_ty, id =
                match lexpr with
                | List Unknown, SId s -> (
                  try (type_of_identifier scope s, SId s)
                  with _ -> (List Unknown, SId s) )
                | ty, expr -> (ty, expr)
              in
              match id_ty with
              | List Unknown -> (t, SSliceExpr (lexpr, slce))
              | List _ -> (id_ty, SSliceExpr ((id_ty, id), slce))
              | _ -> (t, SSliceExpr (lexpr, slce)) )
        in
        check_slice_expr
    | SBinop (e1, op, e2) -> (t, SBinop (expr scope e1, op, expr scope e2))
    | SUnop (op, e) -> (t, SUnop (op, e))
    | SAssign (le, re) ->
        let lt, le' = expr scope le in
        let rt, re' = expr scope re in
        let s_name =
          match le' with
          | SId n -> n
          | SSliceExpr ((_, SId n), _) -> n
          | _ -> raise (Failure "Can't assign to a non variable")
        in
        let ret =
          match (lt, le') with
          | List _, SId s ->
              let _ = add_var scope s rt in
              let _ =
                (* update the list variable in the scope it is defined in *)
                match get_list_scope s_name scope with
                | Some sc -> add_var sc s_name rt
                | None -> ()
              in
              (rt, SAssign ((rt, le'), (rt, re')))
          | _ -> (t, SAssign (le, re))
        in
        ret
    | SCall (f, args) -> (
      match f with
      | _, SId "print" ->
          (* resolve type of list if arguments are list indexing exprs *)
          let resolved_args = List.map (expr scope) args in
          (t, SCall (f, resolved_args))
      | _, SId "append" -> (t, SCall (f, args))
      | _, SId "insert" -> (t, SCall (f, args))
      | _, SId "length" -> (t, SCall (f, args))
      | _, SId "find" -> (t, SCall (f, args))
      | _, SId "findall" -> (t, SCall (f, args))
      | _, SId "match" -> (t, SCall (f, args))
      | _, SId "replace" -> (t, SCall (f, args))
      | _, SId "replaceall" -> (t, SCall (f, args))
      | fty, SId fname ->
          let option_func = look_up_func functions fname in
          if Option.is_some option_func then
            let func = Option.get option_func in
            let args' = List.map (expr scope) args in
            let resolved_arg_tys = List.map fst args' in
            if resolved_arg_tys <> List.map fst args then
              let inner_tys =
                List.map
                  (fun t -> match t with List inner_ty -> inner_ty | ty -> ty)
                  resolved_arg_tys
              in
              let type_specific_name =
                String.concat "_"
                  (List.map (fun t -> string_of_typ t) inner_tys)
              in
              (* get new type resolved function name *)
              let new_fname = String.concat "_" [fname; type_specific_name] in
              (* if new function hasn't been created yet *)
              let new_func_created =
                List.find_opt (fun f -> f.sfname = new_fname) functions
              in
              let _, ret_type =
                match fty with
                | Func (f, r) -> (f, r)
                | _ -> raise E.UndefinedFunction
              in
              (* create new func with new types *)
              if Option.is_none new_func_created then
                let modified_func =
                  { styp= ret_type
                  ; sfname= new_fname
                  ; sformals=
                      List.combine resolved_arg_tys (List.map snd func.sformals)
                  ; sbody= func.sbody }
                in
                (* remove old function from the function list *)
                let _ =
                  global_scope :=
                    { rvariables= !global_scope.rvariables
                    ; rfunctions=
                        modified_func
                        ::
                        List.filter
                          (fun f -> f.sfname != fname)
                          !global_scope.rfunctions
                    ; rlist_variables= !scope.rlist_variables
                    ; rparent= !global_scope.rparent }
                in
                ( ret_type
                , SCall
                    ((Func (resolved_arg_tys, ret_type), SId new_fname), args')
                )
              else
                ( ret_type
                , SCall
                    ((Func (resolved_arg_tys, ret_type), SId new_fname), args')
                )
            else
              let ret =
                match func.styp with
                | List _ -> (func.styp, SCall ((func_ty func, SId fname), args'))
                | _ -> (t, SCall (f, args'))
              in
              ret
          else (t, SCall (f, args))
      | _, _ -> raise E.IllegalFname )
    | SEnd -> (t, SEnd)
    | SNoexpr -> (Void, SNoexpr)
  in
  let rec stmt scope st =
    match st with
    | SExpr e -> SExpr (expr scope e)
    | SBlock sl ->
        let new_scope : resolved_table =
          { rvariables= StringMap.empty
          ; rfunctions= !scope.rfunctions
          ; rlist_variables= !scope.rlist_variables
          ; rparent= Some !scope }
        in
        let new_scope_ref = ref new_scope in
        let rec stmt_list = function
          | SBlock sl :: ss -> stmt_list (sl @ ss)
          | s :: ss -> stmt new_scope_ref s :: stmt_list ss
          | [] -> []
        in
        SBlock (stmt_list sl)
    | SReturn e -> SReturn e
    | SIf (p, then_stmt, else_stmt) -> SIf (expr scope p, then_stmt, else_stmt)
    | SFor (s, e, sl) ->
        let t, e' = expr scope e in
        let list_name = match e' with SId s -> Some s | _ -> None in
        (* get resolved type if expr sl is a list *)
        let inner_ty =
          match t with
          | List ty -> ty
          | String -> Char
          | _ -> raise E.IllegalFor
        in
        let resolved_ty =
          if t = List Unknown && Option.is_some list_name then
            type_of_identifier scope (Option.get list_name)
          else t
        in
        let _ = add_var scope s inner_ty in
        let sexpr = SFor (s, (resolved_ty, e'), stmt scope sl) in
        let _ =
          scope :=
            { rvariables= StringMap.remove s !scope.rvariables
            ; rfunctions= !scope.rfunctions
            ; rlist_variables= !scope.rlist_variables
            ; rparent= !scope.rparent }
        in
        sexpr
    | SWhile (p, b) -> SWhile (p, stmt scope b)
    | SDeclaration (ty, s, e) ->
        let resolved_ty, e' = expr scope e in
        let is_generic_list =
          match resolved_ty with
          | List Unknown -> true
          | List _ -> false
          | _ -> false
        in
        if is_generic_list || e' = SNoexpr then
          let _ = add_list_scope s scope in
          let _ = add_var scope s ty in
          SDeclaration (ty, s, (resolved_ty, e'))
          (* update type of list on LHS of assignment to the type of the LHS *)
        else if resolved_ty = Unknown then
          let _ = add_var scope s ty in
          SDeclaration (ty, s, (resolved_ty, e'))
        else if ty = List Unknown && not is_generic_list then
          let _ = add_var scope s resolved_ty in
          SDeclaration (resolved_ty, s, (resolved_ty, e'))
        else if resolved_ty = List Unknown then
          let _ = add_list_scope s scope in
          let _ = add_var scope s ty in
          SDeclaration (ty, s, (resolved_ty, e'))
        else SDeclaration (ty, s, (resolved_ty, e'))
    | SBreak -> SBreak
    | SContinue -> SContinue
  in
  let check_functions func =
    let add_formal map (ty, name) = StringMap.add name ty map in
    let func_variable_table =
      { rvariables= List.fold_left add_formal StringMap.empty func.sformals
      ; rfunctions= !global_scope.rfunctions
      ; rlist_variables= !global_scope.rlist_variables
      ; rparent= Some !global_scope }
    in
    let func_scope = ref func_variable_table in
    let body' = stmt func_scope (SBlock func.sbody) in
    if func.styp = List Unknown then
      let updated_func =
        List.filter (fun f -> f.sfname = func.sfname) !global_scope.rfunctions
      in
      let _ =
        if length updated_func = 0 then raise E.UndefinedFunction else ()
      in
      (* func.typ should be updated to type specific *)
      { styp= (hd updated_func).styp
      ; sfname= func.sfname
      ; sformals= func.sformals
      ; sbody= [body'] }
    else
      { styp= func.styp
      ; sfname= func.sfname
      ; sformals= func.sformals
      ; sbody= [body'] }
  in
  let statements' =
    try List.map (stmt global_scope) statements with e -> E.handle_error e
  in
  let functions' =
    try List.map check_functions !global_scope.rfunctions
    with e -> E.handle_error e
  in
  (functions', statements')
