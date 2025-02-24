type relevance_set = {
  endpoint : Variable.t;
  (* final receiving partner *)
  relevant_vars : Variable.Set.t;
  (* minimal set of variable needed to explain computation *)
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

let dummy = {
  var_info = Variable.Map.empty;
  var_shapes = Variable.Map.empty;
  contexts = Context.empty_world;
  compounds = Variable.Map.empty;
  constants = Variable.Map.empty;
  relevance_sets = Variable.Map.empty;
  dep_graph = Variable.Graph.empty;
}

let print_variable infos fmt (v : Variable.t) =
  match VarInfo.get_name infos.var_info v with
  | Some name ->
    Format.fprintf fmt "%s/%d" name (Variable.uid v)
  | None ->
    VarInfo.print fmt (Variable.Map.find v infos.var_info)

let print_ctx_variable infos fmt ((v, proj) : Variable.t * Context.Group.t) =
  Format.fprintf fmt "@[<hv 2>%a@,%a@]" (print_variable infos) v
    (Context.print_projection infos.contexts) proj

let print_var_contexts infos fmt () =
  Variable.Map.iter (fun v shape ->
      Format.fprintf fmt "@[<hv 2>var %a@ %a@]@;"
        (print_ctx_variable infos) (v, Context.any_projection (infos.contexts))
        (Context.print_shape infos.contexts) shape
    )
    infos.var_shapes
