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
    | Binop(e1, op, e2) as e -> 
        let (t1, e1') = expr scope e1
        and (t2, e2') = expr scope e2 in
        let same = t1 = t2 in
        let ty = match op with
          Add | Sub | Mult | Div | Mod when same && t1 = Int -> Int
        | Add | Sub | Mult | Div when same && t1 = Float     -> Float
        | Add | Sub | Mult | Div when t1 = Int && t2 = Float -> Float
        | Add | Sub | Mult | Div when t1 = Float && t2 = Int -> Float
        | Exp when same && t1 = Int -> Int
        | Exp when same && t1 = Float -> Float
        | Exp when t1 = Int && t2 = Float -> Float
        | Exp when t2 = Float && t2 = Int -> Float
        | Equal | Neq when same -> Bool
        | Less | Leq | Greater | Geq 
            when same && (t1 = Int || t1 = Float) -> Bool 
        | And | Or when same && t1 = Bool -> Bool
        | Range when same && t1 = Int -> List Int
        | _ -> raise (InvalidBinaryOperation(t1, op, t2, e))
        in (ty, SBinop((t1, e1'), op, (t2, e2')))

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