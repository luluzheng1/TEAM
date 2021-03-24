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
  let rec type_of_identifier (scope : symbol_table ref) name = 
    try StringMap.find name !scope.variables
    with Not_found -> 
      match !scope.parent with
        Some(parent) -> type_of_identifier (ref parent) name
      | _ -> raise (UndefinedId(name))
  in
  
  (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise err
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
    | Id s -> (type_of_identifier scope s, SId s)
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
        | Range when same && t1 = Int -> List(Int)
        | _ -> raise (InvalidBinaryOperation(t1, op, t2, e))
        in (ty, SBinop((t1, e1'), op, (t2, e2')))
    | Unop(op, e) as ex ->
        let (t, e') = expr scope e in
        let ty = match op with
          Neg when t = Int || t = Float -> t
        | Not when t = Bool -> Bool
        | _ -> raise (InvalidUnaryOperation(t, op, ex))
        in (ty, SUnop(op, (t, e')))
    | Assign(s, e) as ex -> 
        let lt = type_of_identifier scope s
        and (rt, e') = expr scope e in
        (check_assign lt rt (IllegalAssignment(lt, rt, ex)), SAssign(s, (rt, e')))
    | ListAssign(s, e1, e2) as ex -> 
        let lt = type_of_identifier scope s
        and (t1, e1') = expr scope e1
        and (t2, e2') = expr scope e2 in
        let inner_ty = match lt with 
          List(ty) -> ty
        | other -> raise (NonListAccess(t1, other, ex))
        in
        let is_index = match t1 with
          Int -> true
        | other -> raise (InvalidIndex(other, ex))
        in
        if is_index && inner_ty = t2 then 
        (List(inner_ty), SListAssign(s, (t1, e1'), (t2, e2'))) else 
        raise (MismatchedTypes(inner_ty, t2, ex))
    | _ -> raise (Failure "Not Yet Implemented")
  in

  let check_bool_expr scope e = 
    let (t', e') = expr scope e
    in if t' != Bool then raise (MismatchedTypes(t', Bool, e)) else (t', e') 
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