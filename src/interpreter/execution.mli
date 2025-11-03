
type value_presence =
  | Absent
  | Present of Value.t

type output_step = {
  step_valuations : value_presence Variable.Map.t;
  step_events : bool Variable.Map.t;
}

type output_line = output_step list

module InputLineMap = IntMap

type computation_outputs = output_line InputLineMap.t

val compute_input_lines :
  Dataflow.Equ.program -> Dataflow.Equ.limits -> Initialization.t
  -> Input.t -> computation_outputs
