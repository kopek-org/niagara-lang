open VarInfo

type item_result_layout = {
  display_name : string;
  provider : bool;
  at_step : Variable.t;
  cumulated : Variable.t option;
  reps : Variable.t Variable.Map.t;
  defaults : Variable.t Variable.Map.t;
}

type results_layout = item_result_layout Variable.Map.t

let dummy_item v = {
  display_name = "%no_name%";
  provider = false;
  at_step = v;
  cumulated = None;
  reps = Variable.Map.empty;
  defaults = Variable.Map.empty;
}

let update_layout_of (v : Variable.t)
    (f : item_result_layout -> item_result_layout)
    (layout : results_layout) =
  Variable.Map.update v (function
      | None -> Some (f (dummy_item v))
      | Some item -> Some (f item))
    layout

let get_display_name (v : Variable.t) (layout : results_layout) =
  (Variable.Map.find v layout).display_name

let context_display (world : Context.world) (c : Context.Group.t) =
  Format.asprintf "%a"
    (Context.print_group_desc world)
    (Context.group_desc world c)

let build_result_layout (pinfos : Surface.Ast.program_infos) =
  Variable.Map.fold (fun v infos layout ->
      let update f = update_layout_of v f layout in
      match infos.origin with
      | Named name ->
        (match infos.kind with
         | Event | Constant -> layout
         | ProvidingPartner ->
           update (fun l -> { l with display_name = name; provider = true })
         | _ -> update (fun l -> { l with display_name = name }))
      | LabelOfPartner { partner; label } ->
        update (fun l ->
            { l with
              display_name =
                (get_display_name partner layout)
                ^ "[" ^ label ^ "]"
            })
      | Cumulative step ->
        update_layout_of step (fun l -> { l  with cumulated = Some v }) layout
      | ContextSpecialized { origin; context } ->
        update (fun l ->
            { l with
              display_name =
                get_display_name origin layout
                ^ "(" ^ (context_display pinfos.contexts context) ^ ")"
            })
      | OperationDetail { op_kind; source; target } ->
        (match op_kind with
         | Quotepart | Bonus ->
           update_layout_of source (fun l ->
               { l with reps = Variable.Map.update target (function
                     | None -> Some v
                     | o -> o)
                     l.reps
               })
             layout
         | Default _ ->
           update_layout_of source (fun l ->
               { l with defaults = Variable.Map.add target v l.defaults })
             layout
         | Deficit _ ->
           update_layout_of source (fun l ->
               { l with reps = Variable.Map.add target v l.reps })
             layout)
      | OperationSum { source; target } ->
        update_layout_of source (fun l ->
            { l with reps = Variable.Map.add target v l.reps })
          layout
      | RepartitionSum _ | DeficitSum _ | ConditionExistential
      | AnonEvent | Peeking _ | RisingEvent _ ->
        layout)
    pinfos.var_info Variable.Map.empty

let sort_layout (infos : collection) (layout : results_layout) =
  List.sort (fun (v1,_) (v2,_) ->
      let i1 = Variable.Map.find v1 infos in
      let i2 = Variable.Map.find v2 infos in
      match i1.kind, i2.kind with
      | (Event | Constant), _ | _, (Event | Constant) -> assert false
      | ParameterInput _, ParameterInput _
      | ProvidingPartner, ProvidingPartner
      | PoolInput _, PoolInput _
      | Intermediary, Intermediary
      | ReceivingPartner, ReceivingPartner -> Variable.compare v1 v2
      | ParameterInput _, _ -> -1
      | _, ParameterInput _ -> 1
      | ProvidingPartner, _ -> -1
      | _, ProvidingPartner -> 1
      | PoolInput _, _ -> -1
      | _, PoolInput _ -> 1
      | Intermediary, _ -> -1
      | _, Intermediary -> 1)
    (Variable.Map.bindings layout)
  |> List.map snd

let iter_layout (infos : collection) (layout : results_layout) f =
  sort_layout infos layout |> List.iter f
