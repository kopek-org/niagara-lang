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
        | OperationDetail { label = _; op_kind; source; target } ->
          (match op_kind with
           | Quotepart  _ | Bonus ->
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

let sort_layout ~(graph : Variable.Graph.t) (layout : results_layout) =
  let scc_index =
    let _, scc = Variable.Graph.Topology.scc graph in
    fun v -> try scc v with Not_found -> min_int
  in
  let max_index item =
    match item with
    | Top i | Detail i -> scc_index i.at_step
    | Super { super_detail_items; super_item = _ } ->
      Variable.Set.fold (fun v m -> max m (scc_index v))
        super_detail_items 0
  in
  Variable.Map.fold (fun _ item l ->
      match item with
      | Detail _ -> l
      | _ -> item::l)
    layout []
  |> List.sort (fun item1 item2 ->
      let m1 = max_index item1 in
      let m2 = max_index item2 in
      compare m1 m2)

let iter_layout ~(graph : Variable.Graph.t) (layout : results_layout) =
  let layout = sort_layout ~graph layout in
  fun f -> List.iter f layout


open Execution

type norm_mode =
  | Canonical
  | SquashAllButPartners
  | PartnerView of Variable.t * IntSet.t

let filter_of_norm_mode (info : ProgramInfo.t) (mode : norm_mode) =
  let canonical_filter v =
    match (Variable.Map.find v info.var_info).origin with
    | OpposingVariant _ -> false
    | _ -> true
  in
  let only_partners_filter =
    let partners =
      Variable.Map.fold (fun v vinfos partners ->
          match vinfos.origin, vinfos.kind with
          | OpposingVariant { origin; target; variant; _ }, _ ->
            let own_partner =
              match variant with
              | LabelOfPartner { partner; label = _ } ->
                Variable.equal target partner
              | Cumulative s ->
                (match (Variable.Map.find s info.var_info).origin with
                 | Named _ -> Variable.equal s target
                 | LabelOfPartner { partner; label = _ } ->
                   Variable.equal partner target
                 | _ -> false
                )
              | _ -> Variable.equal origin target
            in
            if own_partner then
              Variable.Set.add v (Variable.Set.remove origin partners)
            else partners
          | _, (ProvidingPartner | ReceivingPartner) ->
            Variable.Set.add v partners
          | _ -> partners)
        info.var_info Variable.Set.empty
    in
    fun v -> Variable.Set.mem v partners
  in
  let all_lines _ = true in
  let no_lines _ = false in
  match mode with
  | Canonical -> canonical_filter, all_lines
  | SquashAllButPartners -> only_partners_filter, no_lines
  | PartnerView (target, keep_lines) ->
    let line_filter i = IntSet.mem i keep_lines in
    match Variable.Map.find_opt target info.relevance_sets with
    | None -> canonical_filter, line_filter
    | Some ps ->
      (fun (v : Variable.t) -> Variable.Set.mem v ps.relevant_vars),
      line_filter

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

let force_step_merge (info : ProgramInfo.t) ~(filter : Variable.t -> bool)
    (step1 : output_step) (step2 : output_step) =
  let ev2 = Variable.Map.filter (fun v _ -> filter v) step2.step_events in
  let step_valuations =
    merge_valuations info ~filter step1.step_valuations step2.step_valuations
  in
  { step_valuations; step_events = ev2 }

let normalize_valuations (info : ProgramInfo.t) (mode : norm_mode)
    (valuations : computation_outputs) : computation_outputs =
  let var_filter, line_filter = filter_of_norm_mode info mode in
  let filter_step step =
    { step_events = Variable.Map.filter (fun v _ -> var_filter v)
          step.step_events;
      step_valuations = Variable.Map.filter (fun v _ -> var_filter v)
          step.step_valuations;
    }
  in
  let add_to_pending i pending step =
    match pending with
    | None -> Some (i, step)
    | Some (_, pending) ->
      Some (i, force_step_merge info ~filter:var_filter pending step)
  in
  let push_pending pending vals =
    match pending with
    | None -> vals
    | Some (pi, pending) ->
      InputLineMap.update pi (function
          | Some _ ->
            Errors.raise_error "(internal) cannot add squashed lines, \
                                line already present"
          | None -> Some [pending])
        vals
  in
  let push_line i pending line vals =
    let vals = push_pending pending vals in
    InputLineMap.add i line vals
  in
  let pending, vals =
    InputLineMap.fold (fun i line (pending, vals) ->
        let squash = not (line_filter i) in
        match line with
        | [] -> pending, vals
        | [step] ->
          let step = filter_step step in
          if squash then
            add_to_pending i pending step, vals
          else
            None, push_line i pending [step] vals
        | fstep::steps ->
          if squash then
            let pending =
              List.fold_left (add_to_pending i) pending (fstep::steps)
            in
            pending, vals
          else
            let lstep, nsteps =
              List.fold_left (fun (acc_step, nsteps) step ->
                  match try_step_merge info ~filter:var_filter acc_step step with
                  | None -> step, acc_step :: nsteps
                  | Some merged -> merged, nsteps)
                (fstep, []) steps
            in
            let lstep = filter_step lstep in
            let line = List.rev_append nsteps [lstep] in
            None, push_line i pending line vals)
      valuations (None, InputLineMap.empty)
  in
  push_pending pending vals

let normalize_layout (info : ProgramInfo.t) (mode : norm_mode) (layout : results_layout)
  : results_layout =
  let filter, _ = filter_of_norm_mode info mode in
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
    { item with reps; defaults }
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
