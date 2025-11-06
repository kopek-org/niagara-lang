type relevance_set = {
  endpoint : Variable.t;
  (* opposable variant of target *)
  relevant_vars : Variable.Set.t;
  (* minimal set of variable needed to explain opposable computation *)
}

type t = {
  var_info : VarInfo.collection;
  var_shapes : Context.shape Variable.Map.t;
  contexts : Context.world;
  compounds : Variable.Set.t Variable.Map.t;
  constants : Literal.t Variable.Map.t;
  relevance_sets : relevance_set Variable.Map.t;
  dep_graph : Variable.Graph.t;
}

val encoding : t Json_encoding.encoding
val dummy : t

val print_variable : t -> Format.formatter -> Variable.t -> unit

val print_ctx_variable : t -> Format.formatter -> Variable.t * Context.Group.t -> unit

val print_var_contexts : t -> Format.formatter -> unit -> unit

