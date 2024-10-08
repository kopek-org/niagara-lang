type tally = { at_instant : Value.t; total : Value.t; }

type count = {
  tally : tally;
  repartition : Value.t Variable.Map.t;
}

type event_switch =
  | NoEvent
  | SwitchTo of { event : Variable.t; value : bool }

type count_changes = count Variable.Map.t

(* In reverse chronological order *)
type output_line = (event_switch * count_changes) list

type input_line = {
  input_variable : Variable.t;
  input_value : Literal.t;
  input_date : Date.Date.t;
}

module InputLineMap = IntMap

type computation_inputs = input_line InputLineMap.t

type computation_outputs = output_line InputLineMap.t

(* Assumes the program equations is populated. *)
val compute_input_lines : Internal.Ir.program -> computation_inputs -> computation_outputs
