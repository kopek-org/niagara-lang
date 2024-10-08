open Surface
open Internal

open Equations

let time =
  let t = ref (Sys.time ()) in
  fun msg ->
  let t' = Sys.time () in
  Format.eprintf ".4%fs %s\n%!" (t' -. !t) msg;
  t := t'

(** Compilation pipeline from a source AST. *)
let compile : Ast.source Ast.program -> Ir.program = fun program ->
  time "compile start";
  let ctx_program = Contextualize.program program in
  time "contextualized";
  let () =
    let fmt = Format.formatter_of_out_channel stdout in
    let infos, aggr, evts = Equationalize.translate_program ctx_program in
    time "equationalized";
    let equ_prog = Activation_propagation.compute infos aggr evts in
    FormatEqu.print_program fmt equ_prog;
    time "activations propagated";
    let limits = Limits.compute equ_prog in
    FormatEqu.print_limits fmt limits;
    time "limits computed"
  in
  let prog = Ast_to_ir.translate_program ctx_program in
  let prog = ConditionLifting.compute_threshold_equations prog in
  prog
