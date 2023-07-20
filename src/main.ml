
let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Needs a file in argument\n";
    exit 1
  end;
  let file = Sys.argv.(1) in
  let src_program = Frontend.ParserMain.parse_program file in
  Frontend.FormatAst.print_program (Format.formatter_of_out_channel stdout) src_program;
  Printf.printf "Parsing OK\n%!";
  let ctx_program = Frontend.Contextualize.program src_program in
  Frontend.FormatAst.print_program (Format.formatter_of_out_channel stdout) ctx_program;
  Printf.printf "Contextualization OK\n%!";
  let prog = Frontend.Ast_to_ir.translate_program ctx_program in
  Frontend.FormatIr.print_program (Format.formatter_of_out_channel stdout) prog;
  Printf.printf "First pass OK\n%!";

