type raw = {
  id : int;
  name : string;
  context : string list;
  value : string;
}

val to_interpreter_inputs : ProgramInfo.t -> raw list -> Execution.computation_inputs
