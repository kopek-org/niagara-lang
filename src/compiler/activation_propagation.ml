open Surface
open Equations
open Equ

let no_existence v = { eq_act = Condition.never; eq_aff = EExpr (EVar v) }

let rec add_var_to_groups (groups : (Variable.t list * Condition.t) list)
    (var : Variable.t) (cond : Condition.t) =
  match groups with
  | [] ->
    if Condition.is_never cond then [] else
      [[var], cond]
  | (gvars, gcond)::groups ->
    let com = Condition.conj cond gcond in
    if Condition.is_never com then
      let groups = add_var_to_groups groups var cond in
      (gvars, gcond)::groups
    else
      let cond = Condition.excluded cond com in
      let groups = add_var_to_groups groups var cond in
      let ex_gcond = Condition.excluded gcond com in
      let groups =
        if Condition.is_never ex_gcond then groups else
          (gvars, ex_gcond)::groups
      in
      (var::gvars, com)::groups

let generate_addition (vars : Variable.t list) =
  match vars with
  | [] -> Errors.raise_error "(internal) Cannot generate addition on empty set"
  | h::t ->
    List.fold_left (fun e v -> Equ.(EAdd (e, EVar v)))
      (Equ.EVar h) t

type acc = {
  pinfos : Ast.program_infos;
  aggr : aggregate_eqs;
  memo : guarded_eq Variable.Map.t;
  dep_graph : Variable.Graph.t;
 }

let make_acc (pinfos : Ast.program_infos) (aggr : aggregate_eqs) = {
  pinfos;
  aggr;
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

let add_eq acc dest eq =
  { acc with memo = Variable.Map.add dest eq acc.memo }

let aggregate_vars acc (target : Variable.t)
    (vars : (Variable.t * Condition.t) list) =
  let ex_groups, target_cond =
    List.fold_left (fun (groups, glob_cond) (v, cond) ->
        let glob_cond = Condition.disj glob_cond cond in
        let groups = add_var_to_groups groups v cond in
        groups, glob_cond)
      ([], Condition.never) vars
  in
  match ex_groups with
  | [] -> acc, no_existence target
  | [vs, target_cond] ->
    acc, { eq_act = target_cond; eq_aff = EExpr (generate_addition vs) }
  | _ ->
    let acc, fresh_vars =
      List.fold_left (fun (acc, fvars) (gvars, gcond) ->
          let acc, fv =
            create_var_from acc target (fun i ->
                { i with
                  origin = ExistentialAggreg gvars
                })
          in
          let eq_aff = EExpr (generate_addition gvars) in
          let acc = add_eq acc fv { eq_aff; eq_act = gcond } in
          let acc =
            List.fold_left (fun acc v -> add_dep acc v fv)
              acc gvars
          in
          acc, fv::fvars)
        (acc, []) ex_groups
    in
    let eq = { eq_act = target_cond; eq_aff = EMerge fresh_vars } in
    let acc =
      List.fold_left (fun acc v -> add_dep acc v target)
        acc fresh_vars
    in
    add_eq acc target eq, eq

let rec propagate_expr acc (dest : Variable.t) (act : Condition.t)
    (expr : value expr) =
  match expr with
  | EVar v ->
    let acc, { eq_act = vact; _ } = compute_one acc v in
    let acc = add_dep acc v dest in
    acc, expr, Condition.conj act vact
  | ESelf | EConst _-> acc, expr, act
  | ENeg e ->
    let acc, e, act = propagate_expr acc dest act e in
    acc, ENeg e, act
  | EInv e ->
    let acc, e, act = propagate_expr acc dest act e in
    acc, EInv e, act
  | EAdd (e1, e2) ->
    let acc, e1, act1 = propagate_expr acc dest act e1 in
    let acc, e2, act2 = propagate_expr acc dest act e2 in
    acc, EAdd (e1, e2), Condition.conj act1 act2
  | EMult (e1, e2) ->
    let acc, e1, act1 = propagate_expr acc dest act e1 in
    let acc, e2, act2 = propagate_expr acc dest act e2 in
    acc, EMult (e1, e2), Condition.conj act1 act2

and affect_eq acc (dest : Variable.t) (eq : guarded_eq) =
  match eq.eq_aff with
  | ELast v ->
    let acc = add_dep acc dest v in
    acc, eq
  | EMerge _ ->
    Errors.raise_error "(internal) Merge affectation produced before it should"
  | EExpr e ->
    let acc, e, eq_act = propagate_expr acc dest eq.eq_act e in
    acc, { eq_act; eq_aff = EExpr e }

and dispatch_eqs acc (dest : Variable.t) (aggr : aggregation) =
  match aggr with
  | One eq ->
    let acc, eq = affect_eq acc dest eq in
    if Condition.is_never eq.eq_act then acc, no_existence dest else
      add_eq acc dest eq, eq
  | More vars -> aggregate_vars acc dest vars

and compute_one acc (v : Variable.t) =
  match Variable.Map.find_opt v acc.memo with
  | Some eq -> acc, eq
  | None ->
    match Variable.Map.find_opt v acc.aggr with
    | None ->
      let eq_act =
        if Variable.Map.mem v acc.pinfos.Ast.inputs
        then Condition.of_input v else Condition.never
      in
      acc, { eq_act; eq_aff = EExpr (EVar v) }
    | Some eqs -> dispatch_eqs acc v eqs

let order_eqs acc =
  let scc = Variable.Graph.Topology.scc_list acc.dep_graph in
  List.rev @@ List.filter_map (function
      | [] -> assert false
      | [v] ->
        (match (Variable.Map.find v acc.pinfos.nvar_info).kind with
         | ParameterInput | PoolInput -> None
         | _ -> Some v)
      | _scc -> Errors.raise_error "(internal) Cyclic equations")
    scc

let compute (pinfos : Ast.program_infos) (ag_eqs : aggregate_eqs) (act_eqs : event_eqs) =
  let acc = make_acc pinfos ag_eqs in
  let acc =
    Variable.Map.fold (fun dest _eqs acc ->
        fst @@ compute_one acc dest)
      ag_eqs acc
  in
  let order = order_eqs acc in
  { infos = acc.pinfos;
    val_eqs = acc.memo;
    eqs_order = order;
    act_eqs;
  }
