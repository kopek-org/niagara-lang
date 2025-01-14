type item_result_layout = {
  display_name : string;
  provider : bool;
  (* provided value item flag *)
  at_step : Variable.t;
  (* step value.
     canonical item variable, the one to look for in maps and for infos *)
  cumulated : Variable.t option;
  (* total value, if exists *)
  reps : Variable.Set.t Variable.Map.t;
  (* destination -> value mapping of repartition *)
  defaults : Variable.Set.t Variable.Map.t;
  (* destination -> value mapping of default repartition *)
}

type super_item_layout = {
  super_item : item_result_layout;
  (* aggregation values item *)
  super_detail_items : Variable.Set.t;
  (* detail lines items *)
}

type top_item =
  | Super of super_item_layout
  (* contexts/labels aggregation item *)
  | Top of item_result_layout
  (* non aggregate value items *)
  | Detail of item_result_layout
  (* aggregation details, not to be displayed at toplevel *)

type results_layout = top_item Variable.Map.t

type norm_mode =
  | Canonical
  | OpposedTo of Variable.t
(* Normalization form for computation valuations and result layout *)

val build_result_layout : ProgramInfo.t -> results_layout

val iter_layout :
  VarInfo.collection -> results_layout -> (top_item -> unit) -> unit
(* iter on result items in a predefined order *)

val normalize_valuations :
  ProgramInfo.t
  -> norm_mode
  -> Execution.computation_outputs
  -> Execution.computation_outputs
(* filter variables and squash steps following the given normalization mode *)

val normalize_layout :
  ProgramInfo.t -> norm_mode -> results_layout  -> results_layout
(* filter items and sub-items following the given normalization mode *)
