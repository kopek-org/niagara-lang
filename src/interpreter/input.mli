type raw = {
  id : int;
  (** A unique id. *)
  name : string;
  (** The input name. *)
  context : string list;
  (** A convex characterization of the context.
      Can be one element, or can be a convex subset. *)
  value : string;
  (** The input value. *)
  }
val raw_encoding : raw list Json_encoding.encoding

val to_interpreter_inputs : ProgramInfo.t -> raw list -> Execution.computation_inputs

val of_interpreter_inputs : ProgramInfo.t -> Execution.computation_inputs -> raw list
