type relevance_set = {
  endpoint : Variable.t;
  (* final receiving partner *)
  relevant_vars : Variable.Set.t;
  (* minimal set of variable needed to explain computation *)
}

let relevance_set_encoding =
  let open Json_encoding in
  conv
    (fun { endpoint; relevant_vars } -> (endpoint, relevant_vars))
    (fun (endpoint, relevant_vars) -> { endpoint; relevant_vars })
    (obj2 (req "endpoint" Variable.encoding) (req "vars" Variable.Set.encoding))

type t = {
  var_info : VarInfo.collection;
  var_shapes : Context.shape Variable.Map.t;
  contexts : Context.world;
  compounds : Variable.Set.t Variable.Map.t;
  constants : Literal.t Variable.Map.t;
  relevance_sets : relevance_set Variable.Map.t;
  dep_graph : Variable.Graph.t;
}

let encoding =
  let open Json_encoding in
  conv
    (fun {
           var_info;
           var_shapes;
           contexts;
           compounds;
           constants;
           relevance_sets;
           dep_graph;
         } ->
      ( var_info,
        var_shapes,
        contexts,
        compounds,
        constants,
        relevance_sets,
        dep_graph ))
    (fun ( var_info,
           var_shapes,
           contexts,
           compounds,
           constants,
           relevance_sets,
           dep_graph ) ->
      {
        var_info;
        var_shapes;
        contexts;
        compounds;
        constants;
        relevance_sets;
        dep_graph;
      })
    (obj7
       (req "var_info" (Variable.Map.array_encoding VarInfo.encoding))
       (req "var_shapes" (Variable.Map.array_encoding Context.shape_encoding))
       (req "contexts" Context.world_encoding)
       (req "compounds" (Variable.Map.array_encoding Variable.Set.encoding))
       (req "constants" (Variable.Map.array_encoding Literal.encoding))
       (req "relevance_sets"
          (Variable.Map.array_encoding relevance_set_encoding))
       (req "dep_graph" Variable.Graph.encoding))

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
