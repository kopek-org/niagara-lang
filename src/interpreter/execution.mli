type value_presence =
  | Absent
  | Present of Value.t

type output_step = {
  step_valuations : value_presence Variable.Map.t;
  step_events : bool Variable.Map.t;
}

type output_line = output_step list

type input_line = {
  input_variable : Variable.t;
  input_value : Literal.t;
}

val output_line_encoding : output_line Json_encoding.encoding
val input_line_encoding : input_line Json_encoding.encoding

module InputLineMap = IntMap

type computation_inputs = input_line InputLineMap.t

type computation_outputs = output_line InputLineMap.t

val compute_input_lines :
  Dataflow.Equ.program -> Dataflow.Equ.limits -> computation_inputs
  -> computation_outputs
