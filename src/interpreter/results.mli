type item_result_layout = {
  display_name : string;
  at_step : Variable.t;
  (* step value.
     canonical item variable, the one to look for in maps and for infos *)
  cumulated : Variable.t option;
  (* total value, if exists *)
  reps : Variable.t Variable.Map.t Variable.Map.t;
  (* destination -> staged value -> value mapping of repartition *)
  defaults : Variable.t Variable.Map.t Variable.Map.t;
  (* destination -> staged value -> value mapping of default repartition *)
  computed : Variable.Set.t
  (* computation of item value *)
}

type flat_item = {
  value : Variable.t;
  trigger : Variable.t option;
  target : Variable.t;
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
  | Flat of flat_item
  (* triggered value item *)

type results_layout = top_item Variable.Map.t

type line_squashing =
  | MeldInNext  (* Whole line is absorbed into the next displayed
                   line, defaulting to the very last one *)
  | SquashSteps (* Keep the line, but merge all steps *)
  | AllSteps (* Keep everything relevant *)
(* Define squashing behavior for an output line *)

type norm_mode =
  | Canonical
  | SquashAllButPartners
  | Explain of {
      for_partner : Variable.t;
      lines : line_squashing IntMap.t; (* Each line its own policy *)
      in_out_details : bool;
      partner_display : bool;
      relevancy_check : Variable.t -> bool;
    }
(* Normalization form for computation valuations and result layout *)

val build_result_layout : ProgramInfo.t -> results_layout

val sort_layout : graph:Variable.Graph.t -> results_layout -> top_item list

val iter_layout :
  graph:Variable.Graph.t -> results_layout -> (top_item -> unit) -> unit
(* iter on result items in a predefined order *)

val force_step_merge :
  VarInfo.collection ->
  filter:(Variable.t -> bool) ->
  Execution.output_step ->
  Execution.output_step ->
  Execution.output_step

val normalize_valuations :
  ProgramInfo.t
  -> norm_mode
  -> Execution.computation_outputs
  -> Execution.computation_outputs
(* filter variables and squash steps following the given normalization mode *)

val normalize_layout :
  ProgramInfo.t -> norm_mode -> results_layout  -> results_layout
(* filter items and sub-items following the given normalization mode *)

val diff_step_events :
  bool Variable.Map.t -> bool Variable.Map.t -> bool Variable.Map.t
(* [diff_step_events ev1 ev2] returns the map of events whose state
   has changed between [ev1] and [ev2]. The value is [true] if it
   occured and [false] is it was backtracked *)

type temporal_slice_value = {
  slice_event_state : bool Variable.Map.t;
  slice_value : Value.t
  (** Value cumulated between two event changes. Should not be zero,
      it would be absorbed by the next relevant slice *)
}

type var_cumulative = {
  from_start_total : Value.t;
  (** From the very beginning. Same value as `VarInfo.Cumulative` if any. *)
  slices_total : Value.t;
  (** Cumulative total of the record time frame. *)
  slices : temporal_slice_value list
  (** History of cumulation, fragmented by event changes.
      Sums up to `slices_total`. *)
}

type temporal_cumulatives = var_cumulative Variable.Map.t
(** Fragmented representation of value cumulation. Maps variables to
    their cumulatives through time. May represent an arbitrary time
    frame. *)

val compute_temporal_cumulatives :
  VarInfo.collection
  -> Execution.computation_outputs
  -> temporal_cumulatives Execution.InputLineMap.t
(** Builds history of cumulation. One time frame per input line. *)

val concat_temporal_cumulatives :
  temporal_cumulatives
  -> temporal_cumulatives
  -> temporal_cumulatives
(** Merges two consecutives (raises error otherwise) time frames.
    There may be some slices merging, in case their is no event
    changes at join point. *)

val query_cumulation :
  temporal_cumulatives
  -> Variable.t
  -> Condition.t
  -> Value.t
(** It's easy to get `from_start_total` and `slices_total`. This
    queries for a more specific cumulation of given variable during
    the given state condition. *)
