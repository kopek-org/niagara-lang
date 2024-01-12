open Surface

type variable_context = {
  var_context_group : Context.Group.t;
  var_context_desc : Context.group_desc;
}

type actor_label = string option

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

type event_infos = {
  event_name : string;
}

type program_desc = {
  variables : var_infos Variable.Map.t;
  events : event_infos Variable.Map.t;
  contexts : Context.world;
  dep_order : Variable.t list;
}

let compare_kind (k1 : variable_kind) (k2 : variable_kind) =
  match k1, k2 with
  | ParameterInput _, _ -> -1
  | PoolInput _, ParameterInput _ -> 1
  | PoolInput _, _ -> -1
  | ProvidingActor _, (ParameterInput _ | PoolInput _) -> 1
  | ProvidingActor _, _ -> -1
  | Intermediary _, ReceivingActor _ -> -1
  | Intermediary _, _ -> 1
  | ReceivingActor _, _ -> 1

let context_of_variable (p : Ir.program) (v : Variable.t) =
  match Variable.Map.find_opt v p.infos.var_shapes with
  | None ->
    Errors.raise_error "(internal) Cannot find variable %d context group"
      (Variable.uid v)
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

let description_from_program (p : Ir.program) =
  let events =
    Variable.Map.mapi (fun evt _expr ->
        let event_name =
          match Variable.Map.find_opt evt p.infos.var_info with
          | None -> Errors.raise_error "(internal) Cannot find event infos"
          | Some info -> info.Variable.var_name
        in
        { event_name })
    p.equations
  in
  let variables =
    Variable.Map.filter_map (fun v Variable.{ var_name } ->
        if Variable.Map.mem v events then None else
          let var_name, label_name =
            if Variable.Map.mem v p.infos.actors then
              match String.split_on_char '$' var_name with
              | [] -> assert false
              | [vn] -> vn, None
              | vn::ln -> vn, Some (String.concat "$" ln)
            else var_name, None
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
          Some { var_name; var_kind; var_type; }
      )
      p.infos.var_info
  in
  {
    variables;
    events;
    contexts = p.infos.contexts;
    dep_order = p.eval_order;
  }

let print_event_desc fmt (evt, { event_name }) =
  Format.fprintf fmt "e%d: name: '%s'" (Variable.uid evt) event_name

let print_actor_label fmt (l : actor_label) =
  match l with
  | None -> Format.fprintf fmt "default label"
  | Some l -> Format.fprintf fmt "labeled '%s'" l

let print_var_context contexts fmt (ctx : variable_context) =
  Format.fprintf fmt "@[<v 2>Context:@ Group id: %a@ Group desc: @[<h>%a@]@]"
    Context.Group.print ctx.var_context_group
    (Context.print_group_desc contexts) ctx.var_context_desc

let print_var_kind contexts fmt (kind : variable_kind) =
  match kind with
  | ReceivingActor l ->
    Format.fprintf fmt "Receiving actor, %a" print_actor_label l
  | ProvidingActor l ->
    Format.fprintf fmt "Providing actor, %a" print_actor_label l
  | ParameterInput ctx ->
    Format.fprintf fmt "Parameter input@ %a" (print_var_context contexts) ctx
  | PoolInput ctx ->
    Format.fprintf fmt "Pool input@ %a" (print_var_context contexts) ctx
  | Intermediary ctx ->
    Format.fprintf fmt "Intermediate pool@ %a" (print_var_context contexts) ctx

let print_variable_desc contexts fmt (var, desc) =
  Format.fprintf fmt "@[<v 2>v%d:@," (Variable.uid var);
  Format.fprintf fmt "name: '%s'@," desc.var_name;
  Format.fprintf fmt "kind: @[<v>%a@]@," (print_var_kind contexts) desc.var_kind;
  Format.fprintf fmt "type: %a@]" FormatAst.print_type desc.var_type

let print_program_desc fmt (p : program_desc) =
  Format.fprintf fmt "@[<v 2>Program description:@ ";
  Format.fprintf fmt "@[<v 2>Contexts:@ %a@]@,"
    Context.print_world_desc p.contexts;
  Format.fprintf fmt "@[<v 2>Events:@ %a@]@,"
    (Format.pp_print_list print_event_desc) (Variable.Map.bindings p.events);
  Format.fprintf fmt "@[<v 2>Variables:@ %a@]@]"
    (Format.pp_print_list (print_variable_desc p.contexts))
       (Variable.Map.bindings p.variables)
