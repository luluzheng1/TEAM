(* Semantic checking for the TEAM compiler *)

open Ast
open Sast
open Exceptions

module StringMap = Map.Make(String)


let check (functions, statements) =
  let variable_table = {
    variables = StringMap.empty;
    parent = None;
  }
  in
  (* Create a reference to the global table. 
  The scope will be passed through recurisve calls 
  and be mutated when we need to add a new variable  *)
  let global_scope = ref variable_table
  in
  (* Finding a variable, beginning in a given scope 
  and searching upwards *)
  let rec find_variable (scope : symbol_table ref) name = 
    try StringMap.find name !scope.variables
    with Not_found -> 
      match !scope.parent with
        Some(parent) -> find_variable (ref parent) name
      | _ -> raise (UndefinedId(name))
  in
  let rec expr scope exp = match exp with
      IntLit l -> (Int, SIntLit l)
    | FloatLit l -> (Float, SFloatLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | CharLit l -> (Char, SCharLit l)
    | StringLit l -> (String, SStringLit l)
    | ListLit es -> 
      let ts = List.map (fun x -> fst (expr scope x)) es in
      let ty = List.hd ts in
      let check_type e =
        let (ty', e') = expr scope e in
        if ty' = ty then (ty', e') else raise (NonUniformTypeContainer(ty, ty'))
      in
      (match ts with
      | [] -> (Unknown, SListLit [])
      | x::xs -> (ty, SListLit(List.map check_type es)))
    | Id s -> (find_variable scope s, SId s)
    | _ -> raise (Failure "Not Yet Implemented")
  in

  let check_bool_expr scope e = 
    let (t', e') = expr scope e
    in if t' != Bool then raise (MismatchedTypes(t', Bool)) else (t', e') 
  in

  let rec check_stmt scope stmt = match stmt with
      Expr e -> SExpr (expr scope e)
    | If(p, b1, b2, b3) -> SIf(check_bool_expr scope p, check_stmt scope b1, check_stmt scope b2, check_stmt scope b3)
    | For(e1, e2, st) ->
        SFor(expr scope e1, expr scope e2, check_stmt scope st)
    | While(p, s) -> SWhile(check_bool_expr scope p, check_stmt scope s)
    | Block sl -> 
        let rec check_stmt_list = function
            Block sl :: ss  -> check_stmt_list (sl @ ss)
          | s :: ss         -> check_stmt scope s :: check_stmt_list ss
          | []              -> []
        in SBlock(check_stmt_list sl)
    | _ -> SExpr((Void, SNoexpr))
  in 

  let check_stmts stmt = 
    check_stmt global_scope stmt
  in 
  let statements' = try List.map check_stmts statements with e -> handle_error e
in ([], statements')