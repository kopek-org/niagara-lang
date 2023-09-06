

(** Compilation pipeline from a source AST. *)
let compile : Ast.source Ast.program -> unit = fun program ->
  Fmt.pr "%a@\nParsing OK@." FormatAst.print_program program;
  let ctx_program = Contextualize.program program in
  Fmt.pr "%a@\nContextualization OK@." FormatAst.print_program ctx_program;
  let prog = Ast_to_ir.translate_program ctx_program in
  Fmt.pr "%a@\nFirst pass OK@." FormatIr.print_program prog;
  let prog = ConditionLifting.compute_threshold_equations prog in
  Printf.printf "Events threshold OK@.";
  Test_interp.test prog
