open Surface
open Internal

(** Compilation pipeline from a source AST. *)
let compile : Ast.source Ast.program -> Ir.program = fun program ->
  let ctx_program = Contextualize.program program in
  let prog = Ast_to_ir.translate_program ctx_program in
  let prog = ConditionLifting.compute_threshold_equations prog in
  prog
