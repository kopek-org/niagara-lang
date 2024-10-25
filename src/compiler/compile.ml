open Surface

open Equations

let time =
  let t = ref (Sys.time ()) in
  fun msg ->
  let t' = Sys.time () in
  Format.eprintf "%.3fs %s@." (t' -. !t) msg;
  t := t'

(** Compilation pipeline from a source AST. *)
let compile : Ast.source Ast.program -> Equ.program * Equ.limits = fun program ->
  time "compile start";
  let ctx_program = Contextualize.program program in
  time "contextualized";
  let fmt = Format.formatter_of_out_channel stderr in
  let infos, aggr, evts = Equationalize.translate_program ctx_program in
  Variable.Map.iter (fun v _ ->
      Format.fprintf fmt "%a@." (FormatEqu.print_var_with_info infos) v)
    infos.nvar_info;
  time "equationalized";
  let prog = Activation_propagation.compute infos aggr evts in
  FormatEqu.print_program fmt prog;
  time "activations propagated";
  let limits = Limits.compute prog in
  FormatEqu.print_limits fmt limits;
  time "limits computed";
  prog, limits
