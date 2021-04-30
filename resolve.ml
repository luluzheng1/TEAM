open Ast
open Sast
module StringMap = Map.Make (String)
open List
module E = Exceptions

let resolve (functions, statements) =
  (* let _ = List.iter (fun f -> print_endline (string_of_sfdecl f)) functions
     in *)
  let variable_table =
    { variables= StringMap.empty
    ; functions= []
    ; list_variables= StringMap.empty
    ; parent= None }
  in
  let global_scope = ref variable_table in
  let add_var (scope : symbol_table ref) id ty =
    scope :=
      { variables= StringMap.add id ty !scope.variables
      ; functions= !scope.functions
      ; list_variables= !scope.list_variables
      ; parent= !scope.parent }
  in
  let rec type_of_identifier (scope : symbol_table ref) id =
    try StringMap.find id !scope.variables
    with Not_found -> (
      match !scope.parent with
      | Some parent -> type_of_identifier (ref parent) id
      | _ -> raise (E.UndefinedId id) )
  in
  let look_up_func (funcs : sfunc_decl list) fname =
    List.find_opt (fun f -> f.sfname = fname) funcs
  in
  let func_ty fd =
    let param_types = List.map (fun (a, _) -> a) fd.sformals in
    Func (param_types, fd.styp)
  in
  let rec innermost_ty ty =
    match ty with List t -> innermost_ty t | nonlist_ty -> nonlist_ty
  in
  let rec expr scope ((t, e) : sexpr) =
    match e with
    | SIntLit i -> (t, SIntLit i)
    | SFloatLit f -> (t, SFloatLit f)
    | SBoolLit b -> (t, SBoolLit b)
    | SCharLit c -> (t, SCharLit c)
    | SStringLit s -> (t, SStringLit s)
    | SId n -> (
      try (type_of_identifier scope n, SId n) with _ -> (t, SId n) )
    | SListLit l -> (t, SListLit l)
    | SSliceExpr (lexpr, slce) -> (
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
    (* (t, SSliceExpr (lexpr, slce)) *)
    | SBinop (e1, op, e2) -> (t, SBinop (e1, op, e2))
    | SUnop (op, e) -> (t, SUnop (op, e))
    | SAssign (le, re) ->
        let lt, le' = expr scope le in
        let rt, re' = expr scope re in
        let ret =
          match (lt, le') with
          | List Unknown, SId s ->
              let _ = add_var scope s rt in
              (rt, SAssign ((rt, le'), (rt, re')))
          | _ -> (t, SAssign (le, re))
        in
        ret
    | SCall (f, args) -> (
      match f with
      | _, SId "print" ->
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
      | _, SId fname ->
          let func =
            match look_up_func functions fname with
            | Some fn -> fn
            | None -> raise (Failure "Could not find function")
          in
          let ret =
            match func.styp with
            | List _ -> (func.styp, SCall ((func_ty func, SId fname), args))
            | _ -> (t, SCall (f, args))
          in
          ret
      | _, _ -> raise (Failure "Function does not have name") )
    | SEnd -> (t, SEnd)
    | SNoexpr -> (Void, SNoexpr)
  in
  let rec stmt scope st =
    match st with
    | SExpr e -> SExpr (expr scope e)
    | SBlock sl ->
        let new_scope =
          { variables= StringMap.empty
          ; functions= !scope.functions
          ; list_variables= !scope.list_variables
          ; parent= Some !scope }
        in
        let new_scope_ref = ref new_scope in
        let rec stmt_list = function
          | SBlock sl :: ss -> stmt_list (sl @ ss)
          | s :: ss -> stmt new_scope_ref s :: stmt_list ss
          | [] -> []
        in
        SBlock (List.rev (stmt_list (List.rev sl)))
    | SReturn e -> SReturn e
    | SIf (p, then_stmt, else_stmt) -> SIf (p, then_stmt, else_stmt)
    | SFor (s, e, sl) ->
        let t, e = e in
        let list_name = match e with SId s -> Some s | _ -> None in
        let resolved_ty =
          if t = List Unknown && Option.is_some list_name then
            type_of_identifier scope (Option.get list_name)
          else t
        in
        let _ = add_var scope s (innermost_ty resolved_ty) in
        let sexpr = SFor (s, (resolved_ty, e), stmt scope sl) in
        let _ =
          scope :=
            { variables= StringMap.remove s !scope.variables
            ; functions= !scope.functions
            ; list_variables= !scope.list_variables
            ; parent= !scope.parent }
        in
        sexpr
    | SWhile (p, b) -> SWhile (p, b)
    | SDeclaration (ty, s, e) ->
        let resolved_ty, e' = expr scope e in
        let ret =
          match (ty, resolved_ty) with
          | List Unknown, List _ ->
              let _ = add_var scope s resolved_ty in
              SDeclaration (resolved_ty, s, (resolved_ty, e'))
          | _ -> SDeclaration (ty, s, e)
        in
        ret
    | SBreak -> SBreak
    | SContinue -> SContinue
  in
  let statements' =
    try List.map (stmt global_scope) statements with e -> E.handle_error e
  in
  (functions, statements')
