
type variable_context = {
  var_context_group : Context.Group.t;
  var_context_desc : Context.desc;
}

type variable_kind =
  | ReceivingActor of string option
  | ProvidingActor of string option
  | ParameterInput of variable_context
  | PoolInput of variable_context
  | Intermediary of variable_context

type var_infos = {
  var_name : string;
  var_kind : variable_kind;
  var_type : ValueType.t;
}

type event_infos = {
  event_name : string;
}

type program_desc = {
  variables : var_infos Variable.Map.t;
  events : event_infos Variable.Map.t;
  contexts : Context.world;
}

let context_of_variable (p : Ir.program) (v : Variable.t) =
  match Variable.Map.find_opt v p.infos.var_shapes with
  | None -> Errors.raise_error "(internal) Cannot find variable context group"
  | Some shape ->
    Context.shape_fold (fun res g ->
        match res with
        | Some _ -> Errors.raise_error "(internal) Unexpected compound shape"
        | None ->
          Some {
            var_context_group = g;
            var_context_desc = Context.group_desc p.infos.contexts g;
          })
      None shape
    |> Option.get

let description_from_ir (p : Ir.program) =
  let events =
    Variable.Map.mapi (fun evt _expr ->
        let event_name =
          match Variable.Map.find_opt evt p.infos.var_info with
          | None -> Errors.raise_error "(internal) Cannot find event infos"
          | Some info -> info.Variable.var_name
        in
        { event_name })
    p.events
  in
  let variables =
    Variable.Map.mapi (fun v Variable.{ var_name } ->
        let var_name, label_name =
          match String.split_on_char '$' var_name with
          | [] -> assert false
          | [vn] -> vn, None
          | vn::ln -> vn, Some (String.concat "$" ln)
        in
        let var_kind =
          match Variable.Map.find_opt v p.infos.inputs with
            | Some ReadOnly -> ParameterInput (context_of_variable p v)
            | Some Attributable -> PoolInput (context_of_variable p v)
            | None ->
          match Variable.Map.find_opt v p.infos.actors with
            | Some Upstream -> ProvidingActor label_name
            | Some Downstream -> ReceivingActor label_name
            | None -> Intermediary (context_of_variable p v)
        in
        let var_type =
            match Variable.Map.find_opt v p.infos.types with
            | None -> Errors.raise_error "(internal) Cannot find variable type"
            | Some t -> t
        in
        { var_name; var_kind; var_type; }
      )
      p.infos.var_info
  in
  {
    variables;
    events;
    contexts = p.infos.contexts;
  }
