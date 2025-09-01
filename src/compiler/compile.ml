open Surface

open Dataflow

let timing = false

let time =
  let t = ref (Sys.time ()) in
  fun msg ->
  let t' = Sys.time () in
  if timing then Format.eprintf "%.3fs %s@." (t' -. !t) msg;
  t := t'

(** Compilation pipeline from a source AST. *)
let compile : Ast.source Ast.program -> Equ.program * Equ.limits = fun program ->
  (* let fmt = Format.formatter_of_out_channel stderr in *)
  time "compile start";
  let ctx_program = Contextualize.program program in
  (* FormatAst.print_program fmt ctx_program; *)
  time "contextualized";
  let equ_res = Equationalize.translate_program ctx_program in
  (* Variable.Map.iter (fun v _ -> *)
  (*     Format.fprintf fmt "%a@." (FormatEqu.print_var_with_info equ_res.infos) v) *)
  (*   equ_res.infos.var_info; *)
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
  (* List.iter (Format.fprintf fmt "%a@." (FormatEqu.print_var_with_info prog.infos)) *)
  (*   (Variable.Graph.topological_depth_ordering prog.infos.dep_graph); *)
  time "activations propagated";
  let limits = Limits.compute prog in
  (* FormatEqu.print_limits fmt limits; *)
  time "limits computed";
  prog, limits
