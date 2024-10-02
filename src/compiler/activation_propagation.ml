open Surface
open Equations
open Equ

let no_existence = {
  eq_act = Condition.never;
  eq_expr = EConst (LRational R.zero)
}

let rec add_to_groups (groups : ('a list * Condition.t) list)
    (obj : 'a) (cond : Condition.t) =
  match groups with
  | [] ->
    if Condition.is_never cond then [] else
      [[obj], cond]
  | (gobjs, gcond)::groups ->
    (* needs to preserve list order *)
    let com = Condition.conj cond gcond in
    if Condition.is_never com then
      let groups = add_to_groups groups obj cond in
      (gobjs, gcond)::groups
    else
      let cond = Condition.excluded cond com in
      let groups = add_to_groups groups obj cond in
      let ex_gcond = Condition.excluded gcond com in
      let groups =
        if Condition.is_never ex_gcond then groups else
          (gobjs, ex_gcond)::groups
      in
      (obj::gobjs, com)::groups

let generate_addition (exprs : expr list) =
  match exprs with
  | [] -> Errors.raise_error "(internal) Cannot generate addition on empty set"
  | h::t ->
    List.fold_left (fun a e -> Equ.(EAdd (a, e)))
      h t

type acc = {
  pinfos : Ast.program_infos;
  aggr : aggregate_eqs;
  events : expr Variable.Map.t;
  memo : guarded_eq Variable.Map.t;
  dep_graph : Variable.Graph.t;
 }

let make_acc (pinfos : Ast.program_infos) (aggr : aggregate_eqs)
    (events : expr Variable.Map.t) = {
  pinfos;
  aggr;
  events;
  memo = Variable.Map.empty;
  dep_graph = Variable.Graph.empty;
}

let find_vinfo t (v : Variable.t) =
  match Variable.Map.find_opt v t.pinfos.nvar_info with
  | None -> Errors.raise_error "(internal) variable %d info not found" (Variable.uid v)
  | Some i -> i

let bind_vinfo t (v : Variable.t) (info : Variable.Info.t) =
  { t with
    pinfos = {
      t.pinfos with
      nvar_info = Variable.Map.add v info t.pinfos.nvar_info
    }
  }

let create_var_from t (ov : Variable.t) (build : Variable.Info.t -> Variable.Info.t) =
  let v = Variable.create () in
  let oi = find_vinfo t ov in
  let i = build oi in
  let t = bind_vinfo t v i in
  t, v

let add_dep acc from to_ =
  { acc with dep_graph = Variable.Graph.add_edge acc.dep_graph from to_ }

let add_deps acc from to_ =
  List.fold_left (fun acc from ->
      List.fold_left (fun acc to_ ->
          add_dep acc from to_)
        acc to_)
    acc from

let add_eq acc dest eq =
  { acc with memo = Variable.Map.add dest eq acc.memo }

let create_existential acc =
  let v = Variable.create () in
  let acc =
    bind_vinfo acc v
      {
        origin = ConditionExistential;
        typ = ValueType.TRational;
        (* At this point the only type that makes sense *)
        kind = Intermediary;
      }
  in
  acc, v

type expr_form =
  | Direct of expr
  | Merge of Variable.t list

type expr_result = {
  acc : acc;
  cond : Condition.t;
  deps : Variable.Set.t;
  rev_deps : Variable.Set.t;
  form : expr_form;
}

let result_to_var { acc; form; cond; deps; rev_deps } (target : Variable.t) =
  let eq =
    {
      eq_act = cond;
      eq_expr =
        match form with
        | Direct e -> e
        | Merge vars -> EMerge vars
    }
  in
  let acc = add_eq acc target eq in
  let acc = add_deps acc (Variable.Set.elements deps) [target] in
  let acc = add_deps acc [target] (Variable.Set.elements rev_deps) in
  acc, eq

let aggregate_exprs acc (exprs : (expr * Condition.t * Variable.Set.t * Variable.Set.t) list) =
  let ex_groups, target_cond =
    List.fold_left (fun (groups, glob_cond) (e, cond, deps, rdeps) ->
        let glob_cond = Condition.disj glob_cond cond in
        let groups = add_to_groups groups (e, deps, rdeps) cond in
        groups, glob_cond)
      ([], Condition.never) exprs
  in
  let ex_groups =
    List.map (fun (gexprs, gcond) ->
        let gexprs, depsl, rdepsl =
          List.fold_left (fun (es,ds,rs) (e,d,r) -> e::es, d::ds, r::rs)
            ([],[],[]) gexprs
        in
        let gdeps =
          List.fold_left (fun s vs -> Variable.Set.union vs s)
            Variable.Set.empty depsl
        in
        let grdeps =
          List.fold_left (fun s vs -> Variable.Set.union vs s)
            Variable.Set.empty rdepsl
        in
        gexprs, gdeps, grdeps, gcond)
      ex_groups
  in
  match ex_groups with
  | [] -> {
      acc;
      form = Direct (EConst (LRational R.zero));
      cond = Condition.never;
      deps = Variable.Set.empty;
      rev_deps = Variable.Set.empty;
    }
  | [es, deps, rev_deps, cond] -> {
      acc; cond; deps; rev_deps;
      form = Direct (generate_addition es);
    }
  | _ ->
    let acc, fresh_vars =
      List.fold_left (fun (acc, fvars) (gexprs, gdeps, grdeps, gcond) ->
          let e = generate_addition gexprs in
          let acc, fv =
            match e with
            | EVar v -> acc, v
            | _ ->
              let acc, fv = create_existential acc in
              let ge = { eq_expr = e; eq_act = gcond } in
              let acc = add_eq acc fv ge in
              let acc = add_deps acc (Variable.Set.elements gdeps) [fv] in
              let acc = add_deps acc [fv] (Variable.Set.elements grdeps) in
              acc, fv
          in
          acc, Variable.Set.add fv fvars)
        (acc, Variable.Set.empty) ex_groups
    in
    {
      acc;
      cond = target_cond;
      deps = fresh_vars;
      rev_deps = Variable.Set.empty;
      form = Merge (Variable.Set.elements fresh_vars);
    }

let aggregate_vars acc (target : Variable.t)
    (vars : (Variable.t * Condition.t) list) =
  let res =
    aggregate_exprs acc
      (List.map (fun (v,c) ->
           (EVar v, c, Variable.Set.singleton v, Variable.Set.empty))
          vars)
  in
  result_to_var res target

let result_to_expr ({ acc; form; cond; deps; rev_deps } : expr_result) =
  match form with
  | Direct e -> acc, e, cond, deps, rev_deps
  | Merge vars ->
    let eq = { eq_expr = EMerge vars; eq_act = cond } in
    let acc, v = create_existential acc in
    let acc = add_eq acc v eq in
    let acc = add_deps acc (Variable.Set.elements deps) [v] in
    let acc = add_deps acc [v] (Variable.Set.elements rev_deps) in
    acc, EVar v, cond,Variable.Set.singleton v, Variable.Set.empty

let rec propagate_expr acc (dest : Variable.t) (act : Condition.t) (expr : expr) =
  match expr with
  | EVar v ->
    let acc, { eq_act = vact; _ } = compute_one acc v in
    { acc; form = Direct expr;
      cond = Condition.conj act vact;
      deps = Variable.Set.singleton v;
      rev_deps = Variable.Set.empty;
    }
  | EConst _ ->
    { acc; form = Direct expr;
      cond = act;
      deps = Variable.Set.empty;
      rev_deps = Variable.Set.empty;
    }
  | EPre v ->
    { acc; form = Direct expr;
      cond = act;
      deps = Variable.Set.empty;
      rev_deps = Variable.Set.singleton v;
    }
  | ENeg e ->
    let acc, e, act, deps, rev_deps =
      result_to_expr @@ propagate_expr acc dest act e
    in
    { acc; deps; rev_deps;
      form = Direct (ENeg e);
      cond = act;
    }
  | ENot e ->
    let acc, e, act, deps, rev_deps =
      result_to_expr @@ propagate_expr acc dest act e
    in
    { acc; deps; rev_deps;
      form = Direct (ENot e);
      cond = act;
    }
  | EInv e ->
    let acc, e, act, deps, rev_deps =
      result_to_expr @@ propagate_expr acc dest act e
    in
    { acc; deps; rev_deps;
      form = Direct (EInv e);
      cond = act;
    }
  | EAdd (e1, e2) ->
    let acc, e1, act1, deps1, rev_deps1 =
      result_to_expr @@ propagate_expr acc dest act e1
    in
    let acc, e2, act2, deps2, rev_deps2 =
      result_to_expr @@ propagate_expr acc dest act e2
    in
    aggregate_exprs acc [e1, act1, deps1, rev_deps1; e2, act2, deps2, rev_deps2]
  | EMult (e1, e2) ->
    let acc, e1, act1, deps1, rev_deps1 =
      result_to_expr @@ propagate_expr acc dest act e1
    in
    let acc, e2, act2, deps2, rev_deps2 =
      result_to_expr @@ propagate_expr acc dest act e2
    in
    { acc;
      form = Direct (EMult (e1, e2));
      cond = Condition.conj act1 act2;
      deps = Variable.Set.union deps1 deps2;
      rev_deps = Variable.Set.union rev_deps1 rev_deps2;
    }
  | EAnd (e1, e2) ->
    let acc, e1, act1, deps1, rev_deps1 =
      result_to_expr @@ propagate_expr acc dest act e1
    in
    let acc, e2, act2, deps2, rev_deps2 =
      result_to_expr @@ propagate_expr acc dest act e2
    in
    { acc;
      form = Direct (EAnd (e1, e2));
      cond = Condition.conj act1 act2;
      deps = Variable.Set.union deps1 deps2;
      rev_deps = Variable.Set.union rev_deps1 rev_deps2;
    }
  | EGe (e1, e2) ->
    let acc, e1, act1, deps1, rev_deps1 =
      result_to_expr @@ propagate_expr acc dest act e1
    in
    let acc, e2, act2, deps2, rev_deps2 =
      result_to_expr @@ propagate_expr acc dest act e2
    in
    { acc;
      form = Direct (EGe (e1, e2));
      cond = Condition.conj act1 act2;
      deps = Variable.Set.union deps1 deps2;
      rev_deps = Variable.Set.union rev_deps1 rev_deps2;
    }
  | EMerge _ ->
    Errors.raise_error "(internal) Merge expression produced before it should"

and affect_eq acc (dest : Variable.t) (eq : guarded_eq) =
  let res = propagate_expr acc dest eq.eq_act eq.eq_expr in
  result_to_var res dest

and dispatch_eqs acc (dest : Variable.t) (aggr : aggregation) =
  match aggr with
  | One eq ->
    let acc, eq = affect_eq acc dest eq in
    if Condition.is_never eq.eq_act then acc, no_existence else
      add_eq acc dest eq, eq
  | More vars ->
    let acc, vars =
      List.fold_left_map (fun acc (v, cond) ->
          let acc, ge = compute_one acc v in
          acc, (v, Condition.conj ge.eq_act cond))
        acc vars
    in
    aggregate_vars acc dest vars

and compute_one acc (v : Variable.t) =
  match Variable.Map.find_opt v acc.memo with
  | Some eq -> acc, eq
  | None ->
    match Variable.Map.find_opt v acc.events with
    | Some eq_expr ->
      affect_eq acc v { eq_expr; eq_act = Condition.always }
    | None ->
      match Variable.Map.find_opt v acc.aggr with
      | None ->
        let eq_act =
          if Variable.Map.mem v acc.pinfos.Ast.inputs
          then Condition.of_input v else Condition.never
        in
        acc, { eq_act; eq_expr = EVar v }
      | Some eqs -> dispatch_eqs acc v eqs

let order_eqs ~filter acc =
  let scc = Variable.Graph.Topology.scc_list acc.dep_graph in
  List.rev @@ List.filter_map (fun vs ->
      match List.filter filter vs with
      | [] -> None
      | [v] -> Some v
      | _scc -> Errors.raise_error "(internal) Cyclic equations")
    scc

let compute (pinfos : Ast.program_infos) (ag_eqs : aggregate_eqs)
    (act_eqs : expr Variable.Map.t) =
  let acc = make_acc pinfos ag_eqs act_eqs in
  let acc =
    Variable.Map.fold (fun dest _eqs acc ->
        fst @@ compute_one acc dest)
      ag_eqs acc
  in
  let acc =
    Variable.Map.fold (fun dest _eqs acc ->
        fst @@ compute_one acc dest)
      act_eqs acc
  in
  let val_order =
    order_eqs acc
      ~filter:(fun v ->
          match (Variable.Map.find v acc.pinfos.nvar_info).kind with
          | ParameterInput { shadow = false }
          | PoolInput { shadow = false }
          | Event -> false
          | _ -> true)
  in
  let is_event v =
    match (Variable.Map.find v acc.pinfos.nvar_info).kind with
    | Event -> true
    | _ -> false
  in
  let act_order = order_eqs acc ~filter:is_event in
  let act_eqs, val_eqs =
    Variable.Map.partition (fun v _ -> is_event v)
      acc.memo
  in
  { infos = acc.pinfos;
    act_eqs; val_eqs;
    val_order; act_order;
  }
