open Surface

open Equations

let timing = false

let time =
  let t = ref (Sys.time ()) in
  fun msg ->
  let t' = Sys.time () in
  if timing then Format.eprintf "%.3fs %s@." (t' -. !t) msg;
  t := t'

(** Compilation pipeline from a source AST. *)
let compile : Ast.source Ast.program -> Equ.program * Equ.limits = fun program ->
  time "compile start";
  let ctx_program = Contextualize.program program in
  time "contextualized";
  (* let fmt = Format.formatter_of_out_channel stderr in *)
  let equ_res = Equationalize.translate_program ctx_program in
  (* Variable.Map.iter (fun v _ -> *)
  (*     Format.fprintf fmt "%a@." (FormatEqu.print_var_with_info equ_res.infos) v) *)
  (*   equ_res.infos.nvar_info; *)
  time "equationalized";
  (* let filter = *)
  (*   Dot.{ no_filtering with *)
  (*         variable_inclusion = Some (Variable.Set.singleton (Obj.magic 2), DepsOf); *)
  (*         event_knowledge = Variable.Map.singleton (Obj.magic 19) true; *)
  (*       } *)
  (* in *)
  (* Format.(fprintf (formatter_of_out_channel (open_out "graph.dot")) "%s" *)
  (*           (Dot.dot_string_of_program equ_res filter)); *)
  (* time "graph produced"; *)
  let prog = Activation_propagation.compute equ_res.infos equ_res.aggr_eqs equ_res.event_eqs in
  (* FormatEqu.print_program fmt prog; *)
  time "activations propagated";
  let limits = Limits.compute prog in
  (* FormatEqu.print_limits fmt limits; *)
  time "limits computed";
  prog, limits
