(* Semantic checking for the TEAM compiler *)

open Ast
open Sast
open Exceptions

module StringMap = Map.Make(String)


let check (functions, statements) =
  let rec expr = function
      IntLit l -> (Int, SIntLit l)
    | FloatLit l -> (Float, SFloatLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | CharLit l -> (Char, SCharLit l)
    | StringLit l -> (String, SStringLit l)
    | ListLit es -> 
      let ts = List.map (fun x -> fst (expr x)) es in
      let list_type x xs = List.fold_left 
      (fun acc t -> if acc = t then acc else 
      raise (NonUniformTypeContainer(acc, t)))
      x xs
      in
      (match ts with
      | [] -> (Unknown, SListLit [])
      | x::xs -> (list_type x xs, SListLit (List.map expr es)))
    | _ -> raise (Failure "Not Yet Implemented")
  in

  let check_bool_expr e = 
    let (t', e') = expr e
    and err = "expected Boolean expression in " ^ string_of_expr e
    in if t' != Bool then raise (Failure err) else (t', e') 
  in

  let rec check_stmt = function
      Expr e -> SExpr (expr e)
    | If(p, b1, b2, b3) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2, check_stmt b3)
    | For(e1, e2, st) ->
        SFor(expr e1, expr e2, check_stmt st)
    | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
    | Block sl -> 
        let rec check_stmt_list = function
            Block sl :: ss  -> check_stmt_list (sl @ ss)
          | s :: ss         -> check_stmt s :: check_stmt_list ss
          | []              -> []
        in SBlock(check_stmt_list sl)
    | _ -> SExpr((Void, SNoexpr))

in ([], try List.map check_stmt statements with e -> handle_error e)