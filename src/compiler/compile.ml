open Surface
open Internal

open Equations

(** Compilation pipeline from a source AST. *)
let compile : Ast.source Ast.program -> Ir.program = fun program ->
  let ctx_program = Contextualize.program program in
  let () =
    let infos, aggr, evts = Equationalize.translate_program ctx_program in
    let equ_prog = Activation_propagation.compute infos aggr evts in
    FormatEqu.print_program (Format.formatter_of_out_channel stdout) equ_prog
  in
  let prog = Ast_to_ir.translate_program ctx_program in
  let prog = ConditionLifting.compute_threshold_equations prog in
  prog
