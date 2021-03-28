(* team.ml: scan & parse & sematically analyze the input, pretty-print AST
   and SAST *)

type action = Ast | Sast

let () =
  let action = ref Sast in
  let set_action a () = action := a in
  let speclist =
    [ ("-a", Arg.Unit (set_action Ast), "Print the AST")
    ; ("-s", Arg.Unit (set_action Sast), "Print the SAST") ]
  in
  let usage_msg = "usage: ./team.native [-a|-s|-l|-c] [file.mc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg ;
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  match !action with
  | Ast -> print_string (Ast.string_of_program ast)
  | _ -> (
      let sast = Semant.check ast in
      match !action with
      | Ast -> ()
      | Sast -> print_string (Sast.string_of_sprogram sast) )

(* let () = let usage_msg = "usage ./team.native [files.tm]" in let channel =
   ref stdin in Arg.parse [] (fun file -> channel := open_in file) usage_msg;
   let lexbuf = Lexing.from_channel !channel in let ast = Parser.program
   Scanner.token lexbuf in let sast = Semant.check ast in print_string
   (Sast.string_of_sprogram sast) *)
