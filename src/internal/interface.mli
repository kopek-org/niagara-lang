(* Module for more accessible informations on the program *)

type actor_label = string option

type variable_context = {
  var_context_group : Context.Group.t; (* can be used as uid *)
  var_context_desc : Context.group_desc; (* same information as above, but easier to lookup *)
}

type variable_kind =
  | ReceivingActor of actor_label      (* final cascade output *)
  | ProvidingActor of actor_label      (* for money injection *)
  | ParameterInput of variable_context (* any value not flowing through the cascade *)
  | PoolInput of variable_context      (* money to redistribute *)
  | Intermediary of variable_context   (* intermediary pool of money *)

type var_infos = {
  var_name : string;
  var_kind : variable_kind;
  var_type : ValueType.t;
}

type event_infos = { event_name : string; }

type program_desc = {
  variables : var_infos Variable.Map.t;
  events : event_infos Variable.Map.t;
  contexts : Context.world;
  dep_order : Variable.t list;
}

(* Partial order on kind according to the height the variables appears in the
   cascade. *)
val compare_kind : variable_kind -> variable_kind -> int

val context_of_variable : Ir.program -> Variable.t -> variable_context

val description_from_program : Ir.program -> program_desc

val print_program_desc : Format.formatter -> program_desc -> unit
