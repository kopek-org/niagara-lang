open Equ

type subst_kind =
  | QuotePart of { source : Variable.t; delta : R.t }
  | Flat of { source : Variable.t }
  | Other

type user_substitution = {
  expr : expr;
  condition : Condition.t;
  kind : subst_kind;
}

type user_substitutions = user_substitution Variable.Map.t

type result = {
  opp_var_info : VarInfo.collection;
  opp_value_eqs : aggregate_eqs;
  opp_event_eqs : expr Variable.Map.t;
  opp_relevance_sets : ProgramInfo.relevance_set Variable.Map.t;
}

type copies = Variable.t option Variable.Map.t

type acc = {
  var_info : VarInfo.collection;
  value_eqs : aggregate_eqs;
  event_eqs : expr Variable.Map.t;
  copies : copies;
  relevance_sets : ProgramInfo.relevance_set Variable.Map.t
}

type env = {
  target : Variable.t;
  provider : Variable.t;
  user_substs : user_substitutions;
  cumulatives : Variable.t Variable.Map.t;
}

let add_copy acc { cumulatives; _ } (var : Variable.t) =
  let one_copy acc var =
    let copies =
      Variable.Map.update var (function
          | None | Some None -> Some (Some (Variable.create ()))
          | c -> c)
        acc.copies in
    { acc with copies }
  in
  let acc = one_copy acc var in
  match Variable.Map.find_opt var cumulatives with
  | None -> acc
  | Some c -> one_copy acc c

let not_consequent acc env (var : Variable.t) =
  { acc with
    copies =
      Variable.Map.add var None
        (match Variable.Map.find_opt var env.cumulatives with
         | None -> acc.copies
         | Some c -> Variable.Map.add c None acc.copies)
  }

let register_consequence acc env (var : Variable.t) (is_consequent : bool) =
  if is_consequent then
    add_copy acc env var
  else
    not_consequent acc env var

let rec graph_edge acc g (from : Variable.t) (to_ : Variable.t) =
  let from_present = Variable.Graph.mem_vertex g from in
  let g = Variable.Graph.add_edge g from to_ in
  if from_present then g else
    graph_var acc g from

and graph_expr acc g (succ : Variable.t) (expr : expr) =
  match expr with
  | EConst _ -> g
  | EVar var | EPre var ->
    graph_edge acc g var succ
  | ENeg e | EInv e | ENot e ->
    graph_expr acc g succ e
  | EAdd (e1, e2) | EMult (e1, e2)
  | EAnd (e1, e2) | EGe (e1, e2) ->
    let g = graph_expr acc g succ e1 in
    graph_expr acc g succ e2
  | EMerge _ -> assert false

and graph_eq acc g (succ : Variable.t) (eq : aggregation) =
  match eq with
  | One { eq_act; eq_expr } ->
    let g = graph_expr acc g succ eq_expr in
    graph_condition acc g succ eq_act
  | More vars ->
    List.fold_left (fun g (var, _cond) ->
        graph_edge acc g var succ)
      g vars

and graph_condition acc g (succ : Variable.t) (cond : Condition.t) =
  Variable.Set.fold (fun ev g ->
      let g = Variable.Graph.add_edge g ev succ in
      graph_event acc g ev)
    (Condition.events_of cond)
    g

and graph_event acc g (ev : Variable.t) =
  let expr = Variable.Map.find ev acc.event_eqs in
  graph_expr acc g ev expr

and graph_var acc g (var : Variable.t) =
  match Variable.Map.find_opt var acc.value_eqs with
  | None -> Variable.Graph.add_vertex g var
  | Some eq -> graph_eq acc g var eq

let populate_copies acc env g =
  let oppositions_reach =
    Variable.Map.fold (fun v _ reach_set ->
        let r = Variable.Graph.reachables g v in
        Variable.Set.union r reach_set)
      env.user_substs
      Variable.Set.empty
  in
  let acc =
    Variable.Graph.fold_vertex (fun v acc ->
        register_consequence acc env v
          (Variable.Set.mem v oppositions_reach))
      g
      acc
  in
  acc,
  (not @@ Variable.Set.is_empty oppositions_reach)

let var_subst csq (var : Variable.t) =
  match Variable.Map.find_opt var csq.copies with
  | None | Some None -> var
  | Some (Some dup_var) -> dup_var

let rec expr_subst csq (expr : expr) =
  match expr with
  | EConst _ -> expr
  | EVar v -> EVar (var_subst csq v)
  | EPre v -> EPre (var_subst csq v)
  | ENot e -> ENot (expr_subst csq e)
  | ENeg e -> ENeg (expr_subst csq e)
  | EInv e -> EInv (expr_subst csq e)
  | EAnd (e1, e2) ->
    let e1 = expr_subst csq e1 in
    let e2 = expr_subst csq e2 in
    EAnd (e1, e2)
  | EGe (e1, e2) ->
    let e1 = expr_subst csq e1 in
    let e2 = expr_subst csq e2 in
    EGe (e1, e2)
  | EAdd (e1, e2) ->
    let e1 = expr_subst csq e1 in
    let e2 = expr_subst csq e2 in
    EAdd (e1, e2)
  | EMult (e1, e2) ->
    let e1 = expr_subst csq e1 in
    let e2 = expr_subst csq e2 in
    EMult (e1, e2)
  | EMerge _ -> assert false

let condition_subst csq (cond : Condition.t) =
  let open Condition in
  let rec aux (tree : tree) =
    match tree with
    | True -> always
    | False -> never
    | Branch { var = Input _; _ } -> assert false
    | Branch { var = Event ev; yes; no } ->
      let yes = aux yes in
      let no = aux no in
      let ev = var_subst csq ev in
      disj
       (conj (of_event ev true) yes)
       (conj (of_event ev false) no)
  in
  aux (tree cond)

let duplicate_event acc env ~(org : Variable.t) ~(dup : Variable.t) =
  let expr =
    match Variable.Map.find_opt org env.user_substs with
    | Some { expr; _ } -> expr
    | None -> Variable.Map.find org acc.event_eqs
  in
  let dup_expr = expr_subst acc expr in
  let event_eqs = Variable.Map.add dup dup_expr acc.event_eqs in
  { acc with event_eqs }

let duplicate_value acc env ~(org : Variable.t) ~(dup : Variable.t) =
  let dup_one cond expr =
    let cond = condition_subst acc cond in
    let expr =
      match Variable.Map.find_opt org env.user_substs with
      | None -> expr_subst acc expr
      | Some { expr; _ } -> expr_subst acc expr
    in
    cond, expr
  in
  let dup_aggr =
    match Variable.Map.find org acc.value_eqs with
    | One { eq_act; eq_expr } ->
      let eq_act, eq_expr = dup_one eq_act eq_expr in
      One { eq_act; eq_expr }
    | More vars ->
      let dup_vars =
        List.map (fun (var, cond) ->
            let cond = condition_subst acc cond in
            let var = var_subst acc var in
            var, cond)
          vars
      in
      More dup_vars
  in
  let value_eqs = Variable.Map.add dup dup_aggr acc.value_eqs in
  { acc with value_eqs }

let origin_variant acc env (var : Variable.t) (vorigin : VarInfo.origin) =
  let convert_part p =
    match Variable.Map.find_opt var env.user_substs with
    | None -> p
    | Some subst ->
      match subst.kind with
      | QuotePart { delta; _ } -> R.(p + delta)
      | _ -> assert false
  in
  let convert_reps (src : Variable.t) (rep : Condition.t R.Map.t) =
    let relevant_substs =
      Variable.Map.filter (fun _ subst ->
          match subst.kind with
          | QuotePart { source; _ } | Flat { source } ->
            Variable.equal source src
          | Other -> false)
        env.user_substs
    in
    let add_rep c r rep =
      if R.(r < zero || r > one) then rep else
        R.Map.add r (condition_subst acc c) rep
    in
    R.Map.fold (fun r c rep ->
        if Variable.Map.is_empty relevant_substs then
          add_rep c r rep
        else
          Variable.Map.fold (fun _ subst rep ->
              let delta =
                match subst.kind with
                | QuotePart { delta; _ } -> delta
                | _ -> assert false
              in
              let ccond = Condition.conj c subst.condition in
              if Condition.is_never ccond then
                add_rep c r rep
              else
                let xcond = Condition.excluded c subst.condition in
                let cr = R.(r - delta) in
                let rep = add_rep ccond cr rep in
                if Condition.is_never xcond then rep else
                  add_rep xcond r rep)
            relevant_substs rep)
      rep R.Map.empty
  in
  let variant_if_exists v =
    match Variable.Map.find_opt v acc.copies with
    | None | Some None -> v
    | Some (Some var) -> var
  in
  match vorigin with
  | Named _ | AnonEvent | Peeking _
  | RisingEvent _ | ContextSpecialized _
  | ConditionExistential | OppositionDelta _ -> vorigin
  | LabelOfPartner { partner; label } ->
    LabelOfPartner { partner = variant_if_exists partner; label }
  | Cumulative v -> Cumulative (variant_if_exists v)
  | OpposingVariant { variant; _ } -> variant
  | OperationDetail { label; op_kind; source; target; condition } ->
    let op_kind =
      match op_kind with
      | Quotepart p -> VarInfo.Quotepart (convert_part p)
      | Bonus vs -> Bonus (Variable.Set.map variant_if_exists vs)
      | Default rep -> Default (convert_reps source rep)
      | Deficit rep -> Deficit (convert_reps source rep)
    in
    let source = variant_if_exists source in
    let target = variant_if_exists target in
    let condition = match condition with
      | NoEvent -> condition
      | Before e -> Before (variant_if_exists e)
      | After e -> After (variant_if_exists e)
      | When e -> When (variant_if_exists e)
    in
    OperationDetail { label; op_kind; source; target; condition }
  | LocalValuation { target; deps } ->
    let target = variant_if_exists target in
    let deps = Variable.Set.map variant_if_exists deps in
    LocalValuation { target; deps }
  | OperationSum { source; target } ->
    let source = variant_if_exists source in
    let target = variant_if_exists target in
    OperationSum { source; target }
  | RepartitionSum s ->
    let s = variant_if_exists s in
    RepartitionSum s
  | DeficitSum s ->
    let s = variant_if_exists s in
    DeficitSum s
  | StagedRepartition { rep; stage } ->
    let rep = variant_if_exists rep in
    StagedRepartition { rep; stage }
  | PoolStage s ->
    let s = variant_if_exists s in
    PoolStage s

let duplication acc env =
  Variable.Map.fold (fun org dup acc ->
      match dup with
      | None -> acc
      | Some dup ->
        let vinfos = Variable.Map.find org acc.var_info in
        let variant = origin_variant acc env org vinfos.origin in
        let origin =
          VarInfo.OpposingVariant { target = env.target; origin = org; variant }
        in
        let acc =
          { acc with
            var_info =
              Variable.Map.add dup { vinfos with origin } acc.var_info;
          }
        in
        let is_event = VarInfo.is_event vinfos in
        if is_event
        then duplicate_event acc env ~org ~dup
        else duplicate_value acc env ~org ~dup)
    acc.copies acc

let add_delta acc env =
  match Variable.Map.find_opt env.target acc.copies with
  | None | Some None -> acc
  | Some (Some opp) ->
    let dv = Variable.create () in
    let dvinfo = VarInfo.{
      kind = Intermediary;
      typ = TMoney;
      origin = OppositionDelta { target = env.target };
    }
    in
    let cdv = Variable.create () in
    let cdvinfo = VarInfo.{
      kind = Intermediary;
      typ = TMoney;
      origin = Cumulative dv;
    }
    in
    let dve = EAdd (EVar opp, ENeg (EVar env.target)) in
    let cdve = EAdd (EPre cdv, EVar dv) in
    { acc with
      copies =
        (* hack to includes delta in relevant sets *)
        Variable.Map.add dv None acc.copies
        |> Variable.Map.add cdv None;
      var_info =
        Variable.Map.add dv dvinfo acc.var_info
        |> Variable.Map.add cdv cdvinfo;
      value_eqs =
        Variable.Map.add dv (One { eq_act = Condition.always; eq_expr = dve }) acc.value_eqs
        |> Variable.Map.add cdv (One { eq_act = Condition.always; eq_expr = cdve })
        |> Variable.Map.update env.provider (function
            | None -> Some (More [dv, Condition.always])
            | Some (More vars) -> Some (More ((dv, Condition.always)::vars))
            | Some (One _) ->
              Report.raise_internal_error "Cannot aggregate on valuation expression")
    }

let save_relevant_set ~opposable acc ~(target : Variable.t) =
  (* acc.copies should contain every explored variables during
     consequent discoveries. We just go and use that. *)
  let pset = ProgramInfo.{
    endpoint =
      (match Variable.Map.find_opt target acc.copies with
       | None | Some None ->
         if opposable then
           Report.raise_internal_error "No opposing variant for opposability target"
         else
           target
      | Some (Some var) -> var);
    relevant_vars =
      Variable.Map.fold (fun org dup set ->
          let pv =
            match dup with
            | None -> org
            | Some dup -> dup
          in
          Variable.Set.add pv set)
        acc.copies Variable.Set.empty
      (* ensure upstream labels are included *)
      |> Variable.Map.fold (fun v (info : VarInfo.t) pset ->
          match info.origin with
          | LabelOfPartner { partner; _ } ->
            if Variable.equal target partner then
              Variable.Set.add v pset
            else pset
          | _ -> pset)
        acc.var_info
  }
  in
  { acc with
    copies = Variable.Map.empty;
    relevance_sets =
      Variable.Map.add target pset acc.relevance_sets
  }

let resolve_one_target ~opposable pinfos acc ~(target : Variable.t) ~(provider : Variable.t)
    (user_substs : user_substitutions) (cumulatives : Variable.t Variable.Map.t) =
  let env = { target; provider; user_substs; cumulatives } in
  let graph = graph_var acc Variable.Graph.empty target in
  let acc, has_consequent = populate_copies acc env graph in
  if not has_consequent && opposable then
    Report.raise_useless_opposition_error { pinfos with var_info = acc.var_info } target;
  let acc = duplication acc env in
  let acc = add_delta acc env in
  save_relevant_set ~opposable acc ~target

let resolve (pinfo : ProgramInfo.t) (value_eqs : aggregate_eqs)
    (event_eqs : expr Variable.Map.t) (oppositions : user_substitutions Variable.Map.t)
    ~(cumulatives : Variable.t Variable.Map.t) ~(providers : Variable.t Variable.Map.t) =
  let acc = {
    var_info = pinfo.var_info; value_eqs; event_eqs;
    copies = Variable.Map.empty;
    relevance_sets = Variable.Map.empty;
  }
  in
  let acc =
    Variable.Map.fold (fun target substs acc ->
        let provider =
          match Variable.Map.find_opt target providers with
          | None ->
            Report.raise_internal_error "no provider for opposition target '%s'"
          (VarInfo.get_any_name acc.var_info target)
          | Some provider -> provider
        in
        resolve_one_target ~opposable:true pinfo acc ~target ~provider substs cumulatives)
      oppositions acc
  in
  (* Now adding non opposable varinfos *)
  let acc =
    Variable.Map.fold (fun v t acc ->
        match VarInfo.is_original_partner t with
        | None -> acc
        | Some _ ->
          if Variable.Map.mem v oppositions
          then acc
          else
            resolve_one_target
              ~opposable:false
              pinfo
              acc
              ~target:v
              ~provider:v (* should be useless *)
              Variable.Map.empty
              cumulatives
      )
      pinfo.var_info acc
  in
  { opp_var_info = acc.var_info;
    opp_value_eqs = acc.value_eqs;
    opp_event_eqs = acc.event_eqs;
    opp_relevance_sets = acc.relevance_sets;
  }
