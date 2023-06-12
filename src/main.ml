
let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Needs a file in argument\n";
    exit 1
  end;
  let file = Sys.argv.(1) in
  let program = Frontend.ParserMain.parse_program file in
  Printf.printf "Parsing OK\n%!";
  let _prog = Frontend.Ast_to_ir.translate_program program in
  Printf.printf "First pass OK\n%!"
