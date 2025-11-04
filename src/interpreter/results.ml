open VarInfo

type item_result_layout = {
  display_name : string;
  at_step : Variable.t;
  (* step value.
     canonical item variable, the one to look for in maps and for infos *)
  cumulated : Variable.t option;
  (* total value, if exists *)
  reps : Variable.t Variable.Map.t Variable.Map.t;
  (* destination -> staged value -> value mapping of repartition *)
  defaults : Variable.t Variable.Map.t Variable.Map.t;
  (* destination -> staged value -> value mapping of default repartition *)
  computed : Variable.Set.t
  (* computation of item value *)
}

type flat_item = {
  flat_name : string;
  value : Variable.t;
  trigger : Variable.t option;
  target : Variable.t;
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
  | Flat of flat_item
  (* triggered value item *)

type results_layout = top_item Variable.Map.t

let dummy_detail v = {
  display_name = "%no_name%";
  at_step = v;
  cumulated = None;
  reps = Variable.Map.empty;
  defaults = Variable.Map.empty;
  computed = Variable.Set.empty;
}

let dummy_trigger v tr ta = {
  flat_name = "%no_name%";
  value = v;
  trigger = tr;
  target = ta;
}

let update_flat_detail (v : Variable.t) (trigger : Variable.t option)
    (target : Variable.t) (f : flat_item -> flat_item)
    (layout : results_layout) =
  Variable.Map.update v (function
      | None ->
        let item = f (dummy_trigger v trigger target) in
        Some (Flat item)
      | Some (Flat item) -> Some (Flat (f item))
      | Some _ -> assert false)
    layout

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
        Some (Super { item with super_item = f item.super_item })
      | Some (Flat _) -> assert false)
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
        Some (Super super_item)
      | Some (Flat _) -> assert false)
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
              Variable.Map.fold (fun value staged_v reps ->
                  let staged_v = variant_if_exists staged_v in
                  match variant_opt value with
                  | None -> reps
                  | Some value ->
                    Variable.Map.update dest (function
                        | None -> Some (Variable.Map.singleton value staged_v)
                        | Some values -> Some (Variable.Map.add value staged_v values))
                      reps)
                values reps)
          reps base
      in
      let copy_item item v = {
        display_name = Printf.sprintf "%s @%s" item.display_name target_name;
        at_step = v;
        cumulated = Option.bind item.cumulated variant_opt;
        reps = reps_update item.reps Variable.Map.empty;
        defaults = reps_update item.defaults Variable.Map.empty;
        computed = Variable.Set.map variant_if_exists item.computed;
      }
      in
      let copy_flat_item item v = {
        flat_name = Printf.sprintf "%s @%s" item.flat_name target_name;
        value = v;
        trigger = Option.map variant_if_exists item.trigger;
        target = variant_if_exists item.target;
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
            | Flat item ->
              let layout =
                Variable.Map.add org (Flat item) layout
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
                  Variable.Set.map variant_if_exists super_detail_items
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
            | Flat item ->
              Variable.Map.add variant (Flat (copy_flat_item item variant)) layout
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
           | Event | Constant | Value { observable = false; _ } -> layout, variants
           | Value { cumulative; _ } ->
             update (fun l ->
                 { l with
                   display_name = name;
                   cumulated = if cumulative then Some v else l.cumulated;
                   computed = Variable.Set.add v l.computed;
                 }),
             variants
           | _ -> update (fun l -> { l with display_name = name }), variants)
        | LabelOfPartner { partner; label } ->
          super_update partner (fun l ->
              { l with
                display_name =
                  (VarInfo.get_any_name pinfos.var_info partner)
                  ^ "[" ^ label ^ "]";
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
        | StagedRepartition { rep; stage = _ } ->
          (match (Variable.Map.find rep pinfos.var_info).origin with
           | OperationDetail { op_kind; source; target; _ } ->
             (match op_kind with
              | Quotepart  _ ->
                update_detail_of source (fun l ->
                    { l with reps = Variable.Map.update target (function
                          | None -> Some (Variable.Map.singleton v rep)
                          | Some vs -> Some (Variable.Map.add v rep vs))
                          l.reps
                    })
                  layout
              | Default _ ->
                update_detail_of source (fun l ->
                    { l with defaults = Variable.Map.update target (function
                          | None -> Some (Variable.Map.singleton v rep)
                          | Some vs -> Some (Variable.Map.add v rep vs))
                          l.defaults })
                  layout
              | _ -> assert false), variants
           | _ -> assert false)
        | OperationDetail { label; op_kind; source; target; condition } ->
          (match op_kind with
           | Quotepart _ ->
             update_detail_of source (fun l ->
                 { l with reps = Variable.Map.update target (function
                       | None -> Some (Variable.Map.singleton v v)
                       | Some vs -> Some (Variable.Map.add v v vs))
                       l.reps
                 })
               layout
           | Bonus _ ->
             let layout =
               let trigger =
                 match condition with When c -> Some c | _ -> None
               in
               update_flat_detail v trigger target (fun l ->
                   { l with
                     flat_name =
                       Option.value label ~default:("flat bonus");
                   })
                 layout
             in
             let layout =
               update_detail_of source (fun l ->
                   { l with
                     reps = Variable.Map.update target (function
                         | None -> Some (Variable.Map.singleton v v)
                         | Some vs -> Some (Variable.Map.add v v vs))
                         l.reps;
                   })
                 layout
             in
             update_detail_of target (fun l ->
                { l with
                  computed = Variable.Set.add v l.computed;
                })
              layout
           | Default _ ->
             update_detail_of source (fun l ->
                 { l with defaults = Variable.Map.update target (function
                       | None -> Some (Variable.Map.singleton v v)
                       | Some vs -> Some (Variable.Map.add v v vs))
                       l.defaults })
               layout
           | Deficit _ ->
             update_detail_of source (fun l ->
                 { l with reps = Variable.Map.update target (function
                       | None -> Some (Variable.Map.singleton v v)
                       | Some vs -> Some (Variable.Map.add v v vs))
                       l.reps })
               layout), variants
        | LocalValuation { target; deps = _ } ->
          update_detail_of target (fun l ->
              { l with
                computed = Variable.Set.add v l.computed;
              })
            layout, variants
        | OperationSum _ ->
            (* No need, we already register the details, which always exists *)
            layout, variants
        | OpposingVariant { target; origin; variant = _ } ->
          layout,
          Variable.Map.update target (function
              | None -> Some (Variable.Map.singleton origin v)
              | Some vrt -> Some (Variable.Map.add origin v vrt))
            variants
        | OppositionDelta { target } ->
          update (fun l ->
              { l with
                display_name = (VarInfo.get_any_name pinfos.var_info target)^" delta";
              }),
          variants
        | RepartitionSum _ | DeficitSum _ | PoolStage _
        | ConditionExistential | AnonEvent | Peeking _ | RisingEvent _ ->
          layout, variants)
      pinfos.var_info (Variable.Map.empty, Variable.Map.empty)
  in
  variant_copy pinfos layout variants

let sort_layout ~(graph : Variable.Graph.t) (layout : results_layout) =
  let order = Variable.Graph.DAG.topological_depth_ordering graph in
  List.filter_map (fun v ->
      let item = Variable.Map.find_opt v layout in
      Option.bind item (function
          | Detail _ -> None
          | i -> Some i))
    order

let iter_layout ~(graph : Variable.Graph.t) (layout : results_layout) =
  let layout = sort_layout ~graph layout in
  fun f -> List.iter f layout


open Execution

type line_squashing =
  | MeldInNext
  | SquashSteps
  | AllSteps

type norm_mode =
  | Canonical
  | SquashAllButPartners
  | Explain of {
      for_partner : Variable.t;
      lines : line_squashing IntMap.t;
      in_out_details : bool;
      partner_display : bool;
      relevancy_check : Variable.t -> bool;
    }

let filter_of_norm_mode (info : ProgramInfo.t) (mode : norm_mode) =
  let canonical_filter v =
    let vinfo = Variable.Map.find v info.var_info in
    match vinfo.origin with
    | OpposingVariant _ | Peeking  _ | RisingEvent _ -> false
    | _ -> true
  in
  let only_partners_filter () =
    let partners =
      Variable.Map.fold (fun v vinfos partners ->
          match vinfos.origin, vinfos.kind with
          | OpposingVariant { origin; target; _ }, _ ->
            let org_info = Variable.Map.find origin info.var_info in
            let rec own_partner org vinfo =
              match vinfo.origin with
              | LabelOfPartner { partner; label = _ } ->
                Variable.equal target partner
              | Cumulative s -> own_partner s (Variable.Map.find s info.var_info)
              | _ -> Variable.equal org target
            in
            if own_partner origin org_info then
              Variable.Set.add v (Variable.Set.remove origin partners)
            else partners
          | _, (Partner _ | Value { observable = true; _}) ->
            Variable.Set.add v partners
          | Cumulative s, _ ->
            (match (Variable.Map.find s info.var_info).origin with
             | OppositionDelta _ -> Variable.Set.add v partners
             | _ -> partners)
          | _ -> partners)
        info.var_info Variable.Set.empty
    in
    fun v -> Variable.Set.mem v partners
  in
  let all_lines _ = AllSteps in
  let no_lines _ = MeldInNext in
  match mode with
  | Canonical -> canonical_filter, all_lines
  | SquashAllButPartners -> only_partners_filter (), no_lines
  | Explain { for_partner = _; lines; in_out_details;
              partner_display; relevancy_check } ->
    let line_filter i =
      match IntMap.find_opt i lines with
      | Some s -> s
      | None -> MeldInNext
    in
    let rec org_check v =
      let vinfo = Variable.Map.find v info.var_info in
      let rec variant_check variant =
        match variant with
        | Peeking  _ | RisingEvent _ -> false
        | OpposingVariant { variant; _ } -> variant_check variant
        | RepartitionSum _ | DeficitSum _
        | OperationDetail _ | OperationSum _ ->
          in_out_details && relevancy_check v
        | Cumulative v -> org_check v
        | _ -> relevancy_check v
      in
      if not partner_display && VarInfo.is_partner vinfo then false
      else match vinfo.kind with
        | Value { observable = true; _ } -> true
        | _ -> variant_check vinfo.origin
    in
    org_check, line_filter

(* We don't filter valuations because it causes more problems than it
   solves. Only events may be filtered out for now. The actual filter
   test function should mainly be used to construct the layout, which
   ultimatly decides what is displayed or not.
*)

let merge_valuations (info : ProgramInfo.t)
    (val1 : value_presence Variable.Map.t) (val2 : value_presence Variable.Map.t) =
  Variable.Map.merge (fun v p1 p2 ->
      let vinfo = Variable.Map.find v info.var_info in
      match vinfo.kind with
      | Constant | Value { cumulative = true; _ } -> p2
      | _ ->
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
            | LocalValuation _
            | OperationSum _
            | RepartitionSum _
            | DeficitSum _
            | StagedRepartition _
            | PoolStage _
            | ConditionExistential
            | OppositionDelta _ ->
              (match p1, p2 with
               | Present v1, Present v2 -> Present (Value.add v1 v2)
               | Absent, p | p, Absent -> p)
            | OpposingVariant { origin = _; target = _; variant } ->
              merge_on_org variant
            | AnonEvent | RisingEvent _ -> assert false
          in
          Some (merge_on_org vinfo.origin))
    val1 val2

let try_step_merge (info : ProgramInfo.t) ~(filter : Variable.t -> bool)
    (step1 : output_step) (step2 : output_step) =
  let ev1 = Variable.Map.filter (fun v _ -> filter v) step1.step_events in
  let ev2 = Variable.Map.filter (fun v _ -> filter v) step2.step_events in
  if Variable.Map.equal (=) ev1 ev2 then
    let step_valuations =
      merge_valuations info step1.step_valuations step2.step_valuations
    in
    Some { step_valuations; step_events = ev2 }
  else
    None

let force_step_merge (info : ProgramInfo.t) ~(filter : Variable.t -> bool)
    (step1 : output_step) (step2 : output_step) =
  let ev2 = Variable.Map.filter (fun v _ -> filter v) step2.step_events in
  let step_valuations =
    merge_valuations info step1.step_valuations step2.step_valuations
  in
  { step_valuations; step_events = ev2 }

let normalize_valuations (info : ProgramInfo.t) (mode : norm_mode)
    (valuations : computation_outputs) : computation_outputs =
  let var_filter, line_filter = filter_of_norm_mode info mode in
  let filter_step step =
    { step with
      step_events = Variable.Map.filter (fun v _ -> var_filter v)
          step.step_events;
    }
  in
  let add_to_pending i pending step =
    match pending with
    | None -> Some (i, filter_step step)
    | Some (_, pending) ->
      Some (i, force_step_merge info ~filter:var_filter pending step)
  in
  let push_pending pending vals =
    match pending with
    | None -> vals
    | Some (pi, pending) ->
      InputLineMap.update pi (function
          | Some _ ->
            Report.raise_internal_error
              "Cannot add squashed lines, line already present"
          | None -> Some [pending])
        vals
  in
  let push_line i pending line vals =
    let vals = push_pending pending vals in
    if line = [] then vals else
      InputLineMap.add i line vals
  in
  let try_merge_rev_step step steps =
    match steps with
    | [] -> [filter_step step]
    | lstep::steps ->
      match try_step_merge info ~filter:var_filter lstep step with
      | None -> (filter_step step)::lstep::steps
      | Some mstep -> mstep::steps
  in
  let line_steps ~i ~squashing pending line =
    match squashing with
    | MeldInNext ->
      let pending = List.fold_left (add_to_pending i) pending line in
      [], pending
    | SquashSteps ->
      let lsteps =
        match line with
        | [] -> []
        | fstep::steps ->
          [ List.fold_left (fun mstep step ->
                force_step_merge info ~filter:var_filter mstep step)
                (filter_step fstep) steps ]
      in
      lsteps, pending
    | AllSteps ->
      let rsteps =
        List.fold_left (fun rsteps step -> try_merge_rev_step step rsteps) [] line
      in
      List.rev rsteps, pending
  in
  let pending, vals =
    InputLineMap.fold (fun i line (pending, vals) ->
        let squashing = line_filter i in
        let line_steps, pending = line_steps ~i ~squashing pending line in
        if squashing = MeldInNext then pending, vals else
          None, push_line i pending line_steps vals)
      valuations (None, InputLineMap.empty)
  in
  push_pending pending vals

let normalize_layout (info : ProgramInfo.t) (mode : norm_mode) (layout : results_layout)
  : results_layout =
  let filter, _ = filter_of_norm_mode info mode in
  let filter_res_item item =
    let filter_map map =
      Variable.Map.filter_map (fun _d vs ->
          let vs = Variable.Map.filter (fun rv v -> filter rv || filter v) vs in
          if Variable.Map.is_empty vs
          then None else Some vs)
        map
    in
    let reps = filter_map item.reps in
    let defaults = filter_map item.defaults in
    let computed =
      match mode with
      | Canonical -> item.computed
      | SquashAllButPartners -> Variable.Set.empty
      | Explain { in_out_details; _ } ->
        if in_out_details then item.computed else Variable.Set.empty
    in
    { item with reps; defaults; computed }
  in
  let layout, as_details =
    Variable.Map.fold (fun v item (nlayout, details) ->
        let show = filter v in
        let nlayout, details =
          match item with
          | Super { super_item; super_detail_items } ->
            let super_item = filter_res_item super_item in
            let item =
              Super {
                super_item;
                super_detail_items = Variable.Set.filter filter super_detail_items;
              }
            in
            let details =
              if show then
              Variable.Set.union details
                (Variable.Set.filter filter super_detail_items)
              else details
            in
            (Variable.Map.add v item nlayout, details)
          | Top item ->
            let item = filter_res_item item in
            Variable.Map.add v (Top item) nlayout, details
          | Detail item ->
            let item = filter_res_item item in
            Variable.Map.add v (Detail item) nlayout, details
          | Flat item ->
            let nlayout =
              if show then
                Variable.Map.add v (Flat item) nlayout
              else nlayout
            in
            nlayout, details
        in
        if show then nlayout, details else
          nlayout, Variable.Set.add v details)
      layout (Variable.Map.empty, Variable.Set.empty)
  in
  Variable.Map.fold (fun v item layout ->
      match item with
      | Detail i when not (Variable.Set.mem v as_details) ->
        Variable.Map.add v (Top i) layout
      | Top i | Super { super_item = i; _ }
        when Variable.Set.mem v as_details ->
        Variable.Map.add v (Detail i) layout
      | _ -> layout)
    layout layout
