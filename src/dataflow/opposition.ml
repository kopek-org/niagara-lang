open Equ

type user_substitutions = expr Variable.Map.t

type result = {
  var_info : VarInfo.collection;
  value_eqs : aggregate_eqs;
  event_eqs : expr Variable.Map.t;
}

type copies = Variable.t option Variable.Map.t

type concequents = {
  user_substs : user_substitutions;
  events : Variable.Set.t;
  copies : copies;
}

let is_user_subst csq var = Variable.Map.mem var csq.user_substs

let add_copy acc csq ~(target : Variable.t) (var : Variable.t) =
  let copy = Variable.create () in
  let infos = Variable.Map.find var acc.var_info in
  let origin = VarInfo.OpposingVariant { target; origin = var} in
  let acc =
    { acc with
      var_info =
        Variable.Map.add copy { infos with origin } acc.var_info;
    }
  in
  let copies = Variable.Map.add var (Some copy) csq.copies in
  let csq = { csq with copies } in
  acc, csq

let not_consequent csq (var : Variable.t) =
  { csq with
    copies = Variable.Map.add var None csq.copies
  }

let register_consequence acc csq ~(target : Variable.t)
    (var : Variable.t) (is_consequent : bool) =
  let is_consequent = is_consequent || is_user_subst csq var in
  let acc, csq =
    if is_consequent then
      add_copy acc csq ~target var
    else
      acc, not_consequent csq var
  in
  acc, csq, is_consequent

let add_condition_events csq (cond : Condition.t) =
  let rec aux csq (tree : Condition.tree) =
    match tree with
    | True | False -> csq
    | Branch { var = Input _; _ } -> assert false
    | Branch { var = Event v; yes; no } ->
      let csq =
        { csq with events = Variable.Set.add v csq.events }
      in
      aux (aux csq yes) no
  in
  aux csq (Condition.tree cond)

let rec expr_consequents acc csq ~(target : Variable.t) (expr : expr) =
  match expr with
  | EConst _ -> acc, csq, false
  | EVar var | EPre var ->
    let acc, csq, is_consequent =
      compute_consequents acc csq ~target var
    in
    acc, csq, is_consequent
  | ENeg e | EInv e | ENot e ->
    expr_consequents acc csq ~target e
  | EAdd (e1, e2) | EMult (e1, e2)
  | EAnd (e1, e2) | EGe (e1, e2) ->
    let acc, csq, is_csq1 =
      expr_consequents acc csq ~target e1
    in
    let acc, csq, is_csq2 =
      expr_consequents acc csq ~target e2
    in
    acc, csq, is_csq1 || is_csq2
  | EMerge _ -> assert false

and eq_consequents acc csq ~(target : Variable.t) (eq : aggregation) =
  match eq with
  | One { eq_act; eq_expr } ->
    let acc, csq, is_consequent =
      expr_consequents acc csq ~target eq_expr
    in
    let csq =
      if is_consequent then add_condition_events csq eq_act else csq
    in
    acc, csq, is_consequent
  | More vars ->
    List.fold_left (fun (acc, csq, aggr_is_consequent) (var, cond) ->
        let acc, csq, is_consequent =
          compute_consequents acc csq ~target var
        in
        let csq =
          if is_consequent then add_condition_events csq cond else csq
        in
        acc, csq, (aggr_is_consequent || is_consequent))
      (acc, csq, false) vars

and compute_consequents acc csq ~(target : Variable.t) (var : Variable.t) =
  match Variable.Map.find_opt var csq.copies with
  | Some c -> acc, csq, Option.is_some c
  | None ->
    match Variable.Map.find_opt var acc.value_eqs with
    | None -> acc, csq, false
    | Some eq ->
      let acc, csq, is_consequent = eq_consequents acc csq ~target eq in
      register_consequence acc csq ~target var is_consequent

let rec events_consequents acc csq ~(target : Variable.t) =
  let new_events =
    Variable.Set.filter (fun ev -> not @@ Variable.Map.mem ev csq.copies)
      csq.events
  in
  let csq = { csq with events = Variable.Set.empty } in
  let fixpoint_reached = Variable.Set.is_empty new_events in
  if fixpoint_reached then acc, csq else
    let acc, csq =
      Variable.Set.fold (fun ev (acc, csq) ->
          let ev_expr = Variable.Map.find ev acc.event_eqs in
          let acc, csq, is_consequent =
            expr_consequents acc csq ~target ev_expr
          in
          let acc, csq, _ =
            register_consequence acc csq ~target ev is_consequent
          in
          acc, csq)
        new_events (acc, csq)
    in
    events_consequents acc csq ~target

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

let duplicate_event acc csq ~(org : Variable.t) ~(dup : Variable.t) =
  let expr =
    match Variable.Map.find_opt org csq.user_substs with
    | Some expr -> expr
    | None -> Variable.Map.find org acc.event_eqs
  in
  let dup_expr = expr_subst csq expr in
  let event_eqs = Variable.Map.add dup dup_expr acc.event_eqs in
  { acc with event_eqs }

let duplicate_value acc csq ~(org : Variable.t) ~(dup : Variable.t) =
  let dup_one cond expr =
    let cond = condition_subst csq cond in
    let expr =
      match Variable.Map.find_opt org csq.user_substs with
      | None -> expr_subst csq expr
      | Some uexpr -> expr_subst csq uexpr
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
          let cond = condition_subst csq cond in
          let var = var_subst csq var in
          var, cond)
        vars
    in
    More dup_vars
  in
  let value_eqs = Variable.Map.add dup dup_aggr acc.value_eqs in
  { acc with value_eqs }

let duplication acc csq =
  Variable.Map.fold (fun org dup acc ->
      match dup with
      | None -> acc
      | Some dup ->
        let is_event = VarInfo.is_event (Variable.Map.find org acc.var_info) in
        if is_event
        then duplicate_event acc csq ~org ~dup
        else duplicate_value acc csq ~org ~dup)
    csq.copies acc

let resolve_one_target acc ~(target : Variable.t) (user_substs : user_substitutions) =
  let csq = {
    user_substs;
    events = Variable.Set.empty;
    copies = Variable.Map.empty;
  }
  in
  let acc, csq, is_consequent = compute_consequents acc csq ~target target in
  if not is_consequent then Errors.raise_error "Useless opposition";
  let acc, csq = events_consequents acc csq ~target in
  duplication acc csq

let resolve (var_info : VarInfo.collection) (value_eqs : aggregate_eqs)
    (event_eqs : expr Variable.Map.t) (oppositions : user_substitutions Variable.Map.t) =
  let acc = { var_info; value_eqs; event_eqs } in
  Variable.Map.fold (fun target substs acc ->
      resolve_one_target acc ~target substs)
    oppositions acc
