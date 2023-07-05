
let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Needs a file in argument\n";
    exit 1
  end;
  let file = Sys.argv.(1) in
  let src_program = Frontend.ParserMain.parse_program file in
  Printf.printf "Parsing OK\n%!";
  Frontend.FormatAst.print_program (Format.formatter_of_out_channel stdout) src_program;
  let ctx_program = Frontend.Contextualize.program src_program in
  Printf.printf "Contextualization OK\n%!";
  Frontend.FormatAst.print_program (Format.formatter_of_out_channel stdout) ctx_program;
  let _prog = Frontend.Ast_to_ir.translate_program ctx_program in
  Printf.printf "First pass OK\n%!"
