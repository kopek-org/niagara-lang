open VarInfo

type item_result_layout = {
  display_name : string;
  provider : bool;
  (* provided value item flag *)
  at_step : Variable.t;
  (* step value.
     canonical item variable, the one to look for in maps and for infos *)
  cumulated : Variable.t option;
  (* total value, if exists *)
  reps : Variable.Set.t Variable.Map.t;
  (* destination -> value mapping of repartition *)
  defaults : Variable.Set.t Variable.Map.t;
  (* destination -> value mapping of default repartition *)
}

type super_item_layout = {
  super_item : item_result_layout;
  (* aggregation values item *)
  super_detail_items : Variable.Set.t;
  (* detail lines items *)
}

type top_item =
  | Super of super_item_layout
  (* contexts/labels aggregation item *)
  | Top of item_result_layout
  (* non aggregate value items *)
  | Detail of item_result_layout
  (* aggregation details, not to be displayed at toplevel *)

type results_layout = top_item Variable.Map.t

let dummy_detail v = {
  display_name = "%no_name%";
  provider = false;
  at_step = v;
  cumulated = None;
  reps = Variable.Map.empty;
  defaults = Variable.Map.empty;
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

let variant_copy (pinfos : ProgramInfo.t) layout
    (targeted_variants : Variable.t Variable.Map.t Variable.Map.t) =
  Variable.Map.fold (fun target variants (layout : results_layout) ->
      let target_name = VarInfo.get_any_name pinfos.var_info target in
      let variant_opt v = Variable.Map.find_opt v variants in
      let variant_if_exists v = Option.value ~default:v (variant_opt v) in
      let reps_update reps base =
        Variable.Map.fold (fun dest values reps ->
              let dest = variant_if_exists dest in
              Variable.Set.fold (fun value reps ->
                  match variant_opt value with
                  | None -> reps
                  | Some value ->
                    Variable.Map.update dest (function
                        | None -> Some (Variable.Set.singleton value)
                        | Some values -> Some (Variable.Set.add value values))
                      reps)
                values reps)
          reps base
      in
      let copy_item item v = {
        display_name = Printf.sprintf "%s @%s" item.display_name target_name;
        provider = item.provider;
        at_step = v;
        cumulated = Option.bind item.cumulated variant_opt;
        reps = reps_update item.reps Variable.Map.empty;
        defaults = reps_update item.defaults Variable.Map.empty;
      }
      in
      let update_canon_item item = {
        item with
        reps = reps_update item.reps item.reps;
        defaults = reps_update item.defaults item.defaults;
      }
      in
      let layout, variant_detail_items =
        Variable.Map.fold (fun org item (layout, details) ->
            match item with
            | Top item ->
              let canon_item = update_canon_item item in
              let layout =
                Variable.Map.add org (Top canon_item) layout
              in
              layout, details
            | Detail item ->
              let canon_item = update_canon_item item in
              let layout =
                Variable.Map.add org (Detail canon_item) layout
              in
              layout, details
            | Super { super_item; super_detail_items } ->
              let canon_super_item = update_canon_item super_item in
              let layout =
                Variable.Map.add org (Super { super_item = canon_super_item; super_detail_items }) layout
              in
              match Variable.Map.find_opt org variants with
              | None -> layout, details
              | Some variant ->
                let super_item = copy_item super_item variant in
                let super_detail_items =
                  Variable.Set.filter_map variant_opt super_detail_items
                in
                Variable.Map.add variant (Super { super_item; super_detail_items }) layout,
                Variable.Set.union details super_detail_items)
          layout (layout, Variable.Set.empty)
      in
      Variable.Map.fold (fun org item layout ->
          match Variable.Map.find_opt org variants with
          | None -> layout
          | Some variant ->
            match item with
            | Super _ -> layout
            | Top item ->
              Variable.Map.add variant (Top (copy_item item variant)) layout
            | Detail item ->
              let item =
                if Variable.Set.mem variant variant_detail_items
                then Detail (copy_item item variant)
                else Top (copy_item item variant)
              in
              Variable.Map.add variant item layout)
        layout layout)
    targeted_variants layout

let build_result_layout (pinfos : ProgramInfo.t) =
  let layout, variants =
    Variable.Map.fold (fun v infos (layout, variants) ->
        let update f = update_detail_of v f layout in
        let super_update s f = update_super_detail_of s v f layout in
        match infos.origin with
        | Named name ->
          (match infos.kind with
           | Event | Constant -> layout, variants
           | ProvidingPartner ->
             update (fun l -> { l with display_name = name; provider = true }), variants
           | _ -> update (fun l -> { l with display_name = name }), variants)
        | LabelOfPartner { partner; label } ->
          super_update partner (fun l ->
              { l with
                display_name =
                  (VarInfo.get_any_name pinfos.var_info partner)
                  ^ "[" ^ label ^ "]"
              }), variants
        | Cumulative step ->
          update_detail_of step (fun l -> { l with cumulated = Some v }) layout, variants
        | ContextSpecialized { origin; context } ->
          super_update origin (fun l ->
              { l with
                display_name =
                  (VarInfo.get_any_name pinfos.var_info origin)
                  ^ "(" ^ (context_display pinfos.contexts context) ^ ")"
              }), variants
        | OperationDetail { op_kind; source; target } ->
          (match op_kind with
           | Quotepart | Bonus ->
             update_detail_of source (fun l ->
                 { l with reps = Variable.Map.update target (function
                       | None -> Some (Variable.Set.singleton v)
                       | Some vs -> Some (Variable.Set.add v vs))
                       l.reps
                 })
               layout
           | Default _ ->
             update_detail_of source (fun l ->
                 { l with defaults = Variable.Map.update target (function
                       | None -> Some (Variable.Set.singleton v)
                       | Some vs -> Some (Variable.Set.add v vs))
                       l.defaults })
               layout
           | Deficit _ ->
             update_detail_of source (fun l ->
                 { l with reps = Variable.Map.update target (function
                       | None -> Some (Variable.Set.singleton v)
                       | Some vs -> Some (Variable.Set.add v vs))
                       l.reps })
               layout), variants
        | OperationSum _ ->
            (* No need, we already register the details, which always exists *)
            layout, variants
        | OpposingVariant { target; origin; variant = _ } ->
          layout,
          Variable.Map.update target (function
              | None -> Some (Variable.Map.singleton origin v)
              | Some vrt -> Some (Variable.Map.add origin v vrt))
            variants
        | RepartitionSum _ | DeficitSum _ | ConditionExistential
        | AnonEvent | Peeking _ | RisingEvent _ ->
          layout, variants)
      pinfos.var_info (Variable.Map.empty, Variable.Map.empty)
  in
  variant_copy pinfos layout variants

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


open Execution

type norm_mode =
  | Canonical
  | OpposedTo of Variable.t

let filter_of_norm_mode (info : ProgramInfo.t) (mode : norm_mode) =
  let canonical_filter v =
    match (Variable.Map.find v info.var_info).origin with
    | OpposingVariant _ -> false
    | _ -> true
  in
  match mode with
  | Canonical -> canonical_filter
  | OpposedTo target ->
    match Variable.Map.find_opt target info.relevance_sets with
    | None -> canonical_filter
    | Some ps ->
      fun (v : Variable.t) -> Variable.Set.mem v ps.relevant_vars

let merge_valuations (info : ProgramInfo.t) ~(filter : Variable.t -> bool)
    (val1 : value_presence Variable.Map.t) (val2 : value_presence Variable.Map.t) =
  Variable.Map.merge (fun v p1 p2 ->
      if filter v then
        match p1, p2 with
        | None, None -> None
        | Some p, None | None, Some p -> Some p
        | Some p1, Some p2 ->
          let rec merge_on_org vorigin =
            match vorigin with
            | Cumulative _ -> p2
            | Named _
            | LabelOfPartner _
            | Peeking _
            | ContextSpecialized _
            | OperationDetail _
            | OperationSum _
            | RepartitionSum _
            | DeficitSum _
            | ConditionExistential ->
              (match p1, p2 with
               | Present v1, Present v2 -> Present (Value.add v1 v2)
               | Absent, p | p, Absent -> p)
            | OpposingVariant { origin = _; target = _; variant } ->
              merge_on_org variant
            | AnonEvent | RisingEvent _ -> assert false
          in
          Some (merge_on_org (Variable.Map.find v info.var_info).origin)
      else None)
      val1 val2

let try_step_merge (info : ProgramInfo.t) ~(filter : Variable.t -> bool)
    (step1 : output_step) (step2 : output_step) =
  let ev1 = Variable.Map.filter (fun v _ -> filter v) step1.step_events in
  let ev2 = Variable.Map.filter (fun v _ -> filter v) step2.step_events in
  if Variable.Map.equal (=) ev1 ev2 then
    let step_valuations =
      merge_valuations info ~filter step1.step_valuations step2.step_valuations
    in
    Some { step_valuations; step_events = ev2 }
  else
    None

let normalize_valuations (info : ProgramInfo.t) (mode : norm_mode)
    (valuations : computation_outputs) : computation_outputs =
  let filter = filter_of_norm_mode info mode in
  InputLineMap.map (function
      | ([] | [_]) as line -> line
      | fstep::steps ->
        let lstep, nsteps =
          List.fold_left (fun (acc_step, nsteps) step ->
              match try_step_merge info ~filter acc_step step with
              | None -> step, acc_step :: nsteps
              | Some merged -> merged, nsteps)
            (fstep, []) steps
        in
        let lstep =
          { step_events = Variable.Map.filter (fun v _ -> filter v) lstep.step_events;
            step_valuations = Variable.Map.filter (fun v _ -> filter v)
                lstep.step_valuations;
          }
        in
        List.rev_append nsteps [lstep])
    valuations

let normalize_layout (info : ProgramInfo.t) (mode : norm_mode) (layout : results_layout)
  : results_layout =
  let filter = filter_of_norm_mode info mode in
  let filter_res_item item =
    let filter_map map =
      Variable.Map.filter_map (fun _d vs ->
          let vs = Variable.Set.filter filter vs in
          if Variable.Set.is_empty vs
          then None else Some vs)
        map
    in
    let reps = filter_map item.reps in
    let defaults = filter_map item.defaults in
    { item with
      reps;
      defaults
    }
  in
  let layout, promotions =
    Variable.Map.fold (fun v item (nlayout, promotions) ->
        match item with
        | Super { super_item; super_detail_items } ->
          if filter v then
            let super_item = filter_res_item super_item in
            let item = Super {
                super_item;
                super_detail_items = Variable.Set.filter filter super_detail_items;
              }
            in
            (Variable.Map.add v item nlayout, promotions)
          else
            let promotions =
              Variable.Set.union promotions
                (Variable.Set.filter filter super_detail_items)
            in
            nlayout, promotions
        | Top item ->
          if filter v then
            let item = filter_res_item item in
            Variable.Map.add v (Top item) nlayout, promotions
          else nlayout, promotions
        | Detail item ->
          if filter v then
            let item = filter_res_item item in
            Variable.Map.add v (Detail item) nlayout, promotions
          else nlayout, promotions)
      layout (Variable.Map.empty, Variable.Set.empty)
  in
  Variable.Set.fold (fun v layout ->
      Variable.Map.update v (function
          | Some (Detail item) -> Some (Top item)
          | item -> item)
        layout)
    promotions layout
