open Equ

type user_substitution = {
  expr : expr;
  source : Variable.t;
  condition : Condition.t;
  delta : R.t;
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
  events : Variable.Set.t;
  relevance_sets : ProgramInfo.relevance_set Variable.Map.t
}

type env = {
  target : Variable.t;
  user_substs : user_substitutions;
  cumulatives : Variable.t Variable.Map.t;
  maybes : Condition.t Variable.Map.t;
}

let is_user_subst env var = Variable.Map.mem var env.user_substs

let add_copy acc { cumulatives; _ } (var : Variable.t) =
  let one_copy acc var =
    let copy = Variable.create () in
    let copies = Variable.Map.add var (Some copy) acc.copies in
    { acc with copies }
  in
  let acc = one_copy acc var in
  match Variable.Map.find_opt var cumulatives with
  | None -> acc
  | Some c -> one_copy acc c

let add_condition_events acc (cond : Condition.t) =
  let rec aux acc (tree : Condition.tree) =
    match tree with
    | True | False -> acc
    | Branch { var = Input _; _ } -> assert false
    | Branch { var = Event v; yes; no } ->
      let acc =
        { acc with events = Variable.Set.add v acc.events }
      in
      aux (aux acc yes) no
  in
  aux acc (Condition.tree cond)

let maybes_are_certain acc env =
  let acc =
    Variable.Map.fold (fun var cond acc ->
        let acc = add_copy acc env var in
        add_condition_events acc cond)
      env.maybes acc
  in
  let env = { env with maybes = Variable.Map.empty } in
  acc, env

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

let rec expr_consequents acc env (expr : expr) =
  let rec aux acc env delays expr =
    match expr with
    | EConst _ -> acc, false, delays
    | EVar var ->
      let acc, is_consequent = compute_consequents acc env var in
      acc, is_consequent, delays
    | EPre var ->
      acc, false, Variable.Set.add var delays
    | ENeg e | EInv e | ENot e ->
      aux acc env delays e
    | EAdd (e1, e2) | EMult (e1, e2)
    | EAnd (e1, e2) | EGe (e1, e2) ->
      let acc, is_csq1, delays =
        aux acc env delays e1
      in
      let acc, is_csq2, delays  =
        aux acc env delays e2
      in
      acc, is_csq1 || is_csq2, delays
    | EMerge _ -> assert false
  in
  aux acc env Variable.Set.empty expr

and eq_consequents acc env (var : Variable.t) (eq : aggregation) =
  match eq with
  | One { eq_act; eq_expr } ->
    let env = { env with maybes = Variable.Map.add var eq_act env.maybes } in
    let acc, is_consequent, delays = expr_consequents acc env eq_expr in
    let acc, delay_consequent = compute_consequent_delays acc env delays in
    acc, is_consequent || delay_consequent
  | More vars ->
    let env = { env with maybes = Variable.Map.add var Condition.always env.maybes } in
    List.fold_left (fun (acc, aggr_is_consequent) (var, _) ->
        let acc, is_consequent = compute_consequents acc env var in
        acc, (aggr_is_consequent || is_consequent))
      (acc, false) vars

and compute_consequent_delays acc env (delays : Variable.Set.t) =
  Variable.Set.fold (fun var (acc, one_consequent) ->
      let acc, is_consequent = compute_consequents acc env var in
      acc, is_consequent || one_consequent)
    delays (acc, false)

and compute_consequents acc env (var : Variable.t) =
  match Variable.Map.find_opt var acc.copies with
  | Some c -> acc, Option.is_some c
  | None ->
    if Variable.Map.mem var env.maybes then
      acc, false
    else
      match Variable.Map.find_opt var acc.value_eqs with
      | None -> not_consequent acc env var, false
      | Some eq ->
        let is_user_subst = is_user_subst env var in
        let acc, env =
          if is_user_subst
          then maybes_are_certain acc env
          else acc, env
        in
        let acc, is_consequent = eq_consequents acc env var eq in
        let is_consequent = is_consequent || is_user_subst in
        let acc = register_consequence acc env var is_consequent in
        acc, is_consequent

let rec events_consequents acc env =
  let new_events =
    Variable.Set.filter (fun ev -> not @@ Variable.Map.mem ev acc.copies)
      acc.events
  in
  let acc = { acc with events = Variable.Set.empty } in
  let fixpoint_reached = Variable.Set.is_empty new_events in
  if fixpoint_reached then acc else
    let acc =
      Variable.Set.fold (fun ev acc ->
          let ev_expr = Variable.Map.find ev acc.event_eqs in
          let acc, is_consequent, delays = expr_consequents acc env ev_expr in
          let acc, delay_consequent = compute_consequent_delays acc env delays in
          let is_consequent = is_consequent || delay_consequent in
          let acc = register_consequence acc env ev is_consequent in
          acc)
        new_events acc
    in
    events_consequents acc env

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
    | Some subst -> R.(p + subst.delta)
  in
  let convert_reps (source : Variable.t) (rep : Condition.t R.Map.t) =
    let relevant_substs =
      Variable.Map.filter (fun _ subst -> Variable.equal source subst.source)
        env.user_substs
    in
    R.Map.fold (fun r c rep ->
        let r =
          Variable.Map.fold (fun _ subst r ->
              if not Condition.(is_never (conj c subst.condition))
              then R.(r - subst.delta)
              else r)
          relevant_substs r
        in
        if R.(r < zero || r > one) then rep else
          R.Map.add r (condition_subst acc c) rep)
      rep R.Map.empty
  in
  let variant_if_exists v =
    match Variable.Map.find_opt v acc.copies with
    | None | Some None -> v
    | Some (Some var) -> var
  in
  match vorigin with
  | Named _ | LabelOfPartner _ | Cumulative _ | AnonEvent
  | Peeking _ | RisingEvent _ | ContextSpecialized _
  | ConditionExistential -> vorigin
  | OpposingVariant { variant; _ } -> variant
  | OperationDetail { op_kind; source; target } ->
    let op_kind =
      match op_kind with
      | Quotepart p -> VarInfo.Quotepart (convert_part p)
      | Bonus -> Bonus
      | Default rep -> Default (convert_reps source rep)
      | Deficit rep -> Deficit (convert_reps source rep)
    in
    let source = variant_if_exists source in
    let target = variant_if_exists target in
    OperationDetail { op_kind; source; target }
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

let save_relevant_set ~opposable acc ~(target : Variable.t) =
  (* acc.copies should contain every explored variables during
     consequent discoveries. We just go and use that. *)
  let pset = ProgramInfo.{
    endpoint =
      (match Variable.Map.find_opt target acc.copies with
       | None | Some None ->
         if opposable then
           Errors.raise_error "(internal) No opposing variant for opposability target"
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
  }
  in
  { acc with
    copies = Variable.Map.empty;
    relevance_sets = Variable.Map.add target pset acc.relevance_sets
  }

let resolve_one_target ~opposable acc ~(target : Variable.t) (user_substs : user_substitutions)
  (cumulatives : Variable.t Variable.Map.t)  =
  let env = { target; user_substs; cumulatives; maybes = Variable.Map.empty } in
  let acc, is_consequent = compute_consequents acc env target in
  if not is_consequent && opposable then Errors.raise_error "Useless opposition";
  let acc = events_consequents acc env in
  let acc = duplication acc env in
  save_relevant_set ~opposable acc ~target

let resolve (var_info : VarInfo.collection) (value_eqs : aggregate_eqs)
    (event_eqs : expr Variable.Map.t) (oppositions : user_substitutions Variable.Map.t)
    (cumulatives : Variable.t Variable.Map.t) =
  let acc = {
    var_info; value_eqs; event_eqs;
    copies = Variable.Map.empty;
    events = Variable.Set.empty;
    relevance_sets = Variable.Map.empty;
  }
  in
  let acc =
    Variable.Map.fold (fun target substs acc ->
        resolve_one_target ~opposable:true acc ~target substs cumulatives)
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
              acc
              ~target:v
              Variable.Map.empty
              cumulatives
      )
      var_info acc
  in
  { opp_var_info = acc.var_info;
    opp_value_eqs = acc.value_eqs;
    opp_event_eqs = acc.event_eqs;
    opp_relevance_sets = acc.relevance_sets;
  }
