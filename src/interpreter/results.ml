open VarInfo

type item_result_layout = {
  display_name : string;
  provider : bool;
  at_step : Variable.t;
  cumulated : Variable.t option;
  reps : Variable.t Variable.Map.t;
  defaults : Variable.t Variable.Map.t;
}

type super_item_layout = {
  super_item : item_result_layout;
  super_detail_items : Variable.Set.t;
}

type top_item =
  | Super of super_item_layout
  | Top of item_result_layout
  | Detail of item_result_layout

type results_layout = top_item Variable.Map.t

let dummy_detail v = {
  display_name = "%no_name%";
  provider = false;
  at_step = v;
  cumulated = None;
  reps = Variable.Map.empty;
  defaults = Variable.Map.empty;
}

let dummy_super v = {
  super_item = dummy_detail v;
  super_detail_items = Variable.Set.empty;
}

let update_detail_of ?(is_detail=false) (v : Variable.t)
    (f : item_result_layout -> item_result_layout)
    (layout : results_layout) =
  Variable.Map.update v (function
      | None ->
        let item = f (dummy_detail v) in
        if is_detail then Some (Detail item) else Some (Top item)
      | Some (Detail item) -> Some (Detail (f item))
      | Some (Top item) ->
        if is_detail then Some (Detail (f item)) else Some (Top (f item))
      | Some (Super item) ->
        Some (Super { item with super_item = f item.super_item }))
    layout

let update_super_detail_of (super : Variable.t) (detail : Variable.t)
    (f : item_result_layout -> item_result_layout)
    (layout : results_layout) =
  let layout = update_detail_of ~is_detail:true detail f layout in
  Variable.Map.update super (function
      | None -> assert false
      | Some (Super item) ->
        let super_item = {
          item with
          super_detail_items =
            Variable.Set.add detail item.super_detail_items
        } in
        Some (Super super_item)
      | Some (Top item) | Some (Detail item) ->
        let super_item = {
          super_item = item;
          super_detail_items =
            if Variable.equal super detail then Variable.Set.empty
            else Variable.Set.singleton detail
        } in
        Some (Super super_item))
    layout

let context_display (world : Context.world) (c : Context.Group.t) =
  Format.asprintf "%a"
    (Context.print_group_desc world)
    (Context.group_desc world c)

let build_result_layout (pinfos : Surface.Ast.program_infos) =
  Variable.Map.fold (fun v infos layout ->
      let update f = update_detail_of v f layout in
      let super_update s f = update_super_detail_of s v f layout in
      match infos.origin with
      | Named name ->
        (match infos.kind with
         | Event | Constant -> layout
         | ProvidingPartner ->
           update (fun l -> { l with display_name = name; provider = true })
         | _ -> update (fun l -> { l with display_name = name }))
      | LabelOfPartner { partner; label } ->
        super_update partner (fun l ->
            { l with
              display_name =
                (VarInfo.get_any_name pinfos.var_info partner)
                ^ "[" ^ label ^ "]"
            })
      | Cumulative step ->
        update_detail_of step (fun l -> { l  with cumulated = Some v }) layout
      | ContextSpecialized { origin; context } ->
        super_update origin (fun l ->
            { l with
              display_name =
                (VarInfo.get_any_name pinfos.var_info origin)
                ^ "(" ^ (context_display pinfos.contexts context) ^ ")"
            })
      | OperationDetail { op_kind; source; target } ->
        (match op_kind with
         | Quotepart | Bonus ->
           update_detail_of source (fun l ->
               { l with reps = Variable.Map.update target (function
                     | None -> Some v
                     | o -> o)
                     l.reps
               })
             layout
         | Default _ ->
           update_detail_of source (fun l ->
               { l with defaults = Variable.Map.add target v l.defaults })
             layout
         | Deficit _ ->
           update_detail_of source (fun l ->
               { l with reps = Variable.Map.add target v l.reps })
             layout)
      | OperationSum { source; target } ->
        update_detail_of source (fun l ->
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
  |> List.filter_map (fun (_,item) ->
      match item with Super _ | Top _ -> Some item | Detail _ -> None)

let iter_layout (infos : collection) (layout : results_layout) f =
  sort_layout infos layout |> List.iter f
