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

type line = {
  input_variable : Variable.t;
  input_value : Literal.t;
}

module InputLineMap = IntMap
(* any uid linking input and output lines would do *)

type t = line InputLineMap.t

val largest_domain :
  Context.world -> Context.CaseSet.t Context.DomainMap.t list -> string list

val of_raw : ProgramInfo.t -> raw list -> t

val to_raw : ProgramInfo.t -> t -> raw list
