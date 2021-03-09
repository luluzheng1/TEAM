(* team.ml: scan & parse the input, pretty-print the AST *)

let () =
  let usage_msg = "usage ./team.native [files.tm]" in 
  let channel = ref stdin in 
  Arg.parse [] (fun file -> channel := open_in file) usage_msg;
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  print_string (Ast.string_of_program ast)