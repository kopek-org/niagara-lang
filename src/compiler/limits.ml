open Equations
open Equ

type val_exprs =
  | Zero
  | Exprs of guarded_eq list

type acc = {
  info : Variable.Info.collection;
  act_eqs : guarded_eq Variable.Map.t;
  val_eqs : guarded_eq Variable.Map.t;
  lim_memo : threshold list Variable.Map.t;
  val_memo : val_exprs Variable.Map.t;
}

let is_input acc (v : Variable.t) =
  match Variable.Map.find_opt v acc.info with
  | None -> Errors.raise_error "(internal) No info for var %d" (Variable.uid v)
  | Some i ->
    match i.Variable.Info.kind with
    | ParameterInput { shadow  = false }
    | PoolInput { shadow  = false } -> true
    | _ -> false

let add_limits acc (v : Variable.t) (l : threshold list) =
  { acc with
    lim_memo = Variable.Map.add v l acc.lim_memo;
  }

let add_val acc (v : Variable.t) (va : val_exprs) =
  { acc with
    val_memo = Variable.Map.add v va acc.val_memo;
  }

let filter_val (v : val_exprs) (act : Condition.t) =
  match v with
  | Zero -> Zero
  | Exprs es ->
    let exprs =
      List.filter_map (fun ge ->
          let eq_act = Condition.conj ge.eq_act act in
          if Condition.is_never eq_act
          then None
          else Some { ge with eq_act })
        es
    in
    match exprs with [] -> Zero | _ -> Exprs exprs

let map_val f (v : val_exprs) =
  match v with
  | Zero -> Zero
  | Exprs es ->
    Exprs
      (List.map (fun ge ->
           { ge with eq_expr = f ge.eq_expr })
          es)

let rec expr_val acc (e : guarded_eq) =
  match e.eq_expr with
  | EConst _ | EPre _ -> acc, Exprs [e]
  | EVar v ->
    let acc, vals = var_val acc v in
    acc, filter_val vals e.eq_act
  | ENeg eq_expr ->
    let acc, vals = expr_val acc { e with eq_expr } in
    acc, map_val (fun e -> ENeg e) vals
  | EInv eq_expr ->
    let acc, vals = expr_val acc { e with eq_expr } in
    acc, map_val (fun e -> EInv e) vals
  | EAdd (e1, e2) ->
    let acc, vals1 = expr_val acc { e with eq_expr = e1 } in
    let acc, vals2 = expr_val acc { e with eq_expr = e2 } in
    let vals =
      match vals1, vals2 with
      | Zero, v | v, Zero -> v
      | Exprs ge1, Exprs ge2 ->
        let exprs =
          List.flatten @@
          List.map (fun ge1 ->
              List.filter_map (fun ge2 ->
                  let eq_act = Condition.conj ge1.eq_act ge2.eq_act in
                  if Condition.is_never eq_act then None else
                    Some {
                      eq_expr = EAdd (ge1.eq_expr, ge2.eq_expr);
                      eq_act;
                    })
                ge2)
            ge1
        in
        Exprs exprs
    in
    acc, vals
  | EMult (e1, e2) ->
    let acc, vals1 = expr_val acc { e with eq_expr = e1 } in
    let acc, vals2 = expr_val acc { e with eq_expr = e2 } in
    let vals =
      match vals1, vals2 with
      | Zero, _ | _, Zero -> Zero
      | Exprs ge1, Exprs ge2 ->
        let exprs =
          List.flatten @@
          List.map (fun ge1 ->
              List.filter_map (fun ge2 ->
                  let eq_act = Condition.conj ge1.eq_act ge2.eq_act in
                  if Condition.is_never eq_act then None else
                    Some {
                      eq_expr = EMult (ge1.eq_expr, ge2.eq_expr);
                      eq_act;
                    })
                ge2)
            ge1
        in
        Exprs exprs
    in
    acc, vals
  | EMerge [] -> assert false
  | EMerge (v::vars) ->
    let acc, vl =
      List.fold_left (fun (acc, vl) v ->
          let acc, vals = var_val acc v in
          match vl, vals with
          | Zero, vl | vl, Zero -> acc, vl
          | Exprs ge1, Exprs ge2 ->
            acc, Exprs (ge1@ge2))
        (var_val acc v) vars
    in
    acc, filter_val vl e.eq_act
  | ENot _ | EAnd _ | EGe _ -> assert false

and var_val acc (v : Variable.t) =
  match Variable.Map.find_opt v acc.val_memo with
  | Some l -> acc, l
  | None ->
    if is_input acc v then
      let ge = { eq_act = Condition.of_input v; eq_expr = EVar v } in
      let va = Exprs [ge] in
      add_val acc v va, va
    else
      match Variable.Map.find_opt v acc.val_eqs with
      | Some ge ->
        let acc, va = expr_val acc ge in
        add_val acc v va, va
      | None ->
        Errors.raise_error
          "Unable to compute value for variable %d, equation not \
           found"
          (Variable.uid v)

type linear = {
  factor : (Variable.t * expr) option;
  const : expr option;
}

let rec linear_of_expr (e : expr) =
  match e with
  | EVar v ->
    { factor = Some (v, EConst (LRational R.one));
      const = None }
  | EPre _ | EConst _ ->
    { factor = None;
      const = Some e }
  | ENeg e ->
    let { factor; const } = linear_of_expr e in
    { factor = Option.map (fun (v,e) -> (v, ENeg e)) factor;
      const = Option.map (fun e -> ENeg e) const; }
  | EInv e ->
    let { factor; const } = linear_of_expr e in
    { factor = Option.map (fun (v,e) -> (v, EInv e)) factor;
      const = Option.map (fun e -> EInv e) const; }
  | EAdd (e1, e2) ->
    let l1 = linear_of_expr e1 in
    let l2 = linear_of_expr e2 in
    let factor =
      match l1.factor, l2.factor with
      | f, None | None, f -> f
      | Some (v1,f1), Some (v2,f2) ->
        assert (Variable.equal v1 v2);
        Some (v1, EAdd (f1, f2))
    in
    let const =
      match l1.const, l2.const with
      | c, None | None, c -> c
      | Some c1, Some c2 -> Some (EAdd (c1,c2))
    in
    { factor; const }
  | EMult (e1, e2) ->
    let l1 = linear_of_expr e1 in
    let l2 = linear_of_expr e2 in
    let factor =
      match l1.factor, l2.factor with
      | Some _, Some _ ->
        Errors.raise_error "Non-linear values are not allowed"
      | None, None -> None
      | Some (v,f), None ->
        (match l2.const with None -> Some (v,f) | Some c -> Some (v, EMult(c,f)))
      | None, Some (v,f) ->
        (match l1.const with None -> Some (v,f) | Some c -> Some (v, EMult(c,f)))
    in
    let const =
      match l1.const, l2.const with
      | _, None | None, _ -> None
      | Some c1, Some c2 -> Some (EMult (c1,c2))
    in
    { factor; const }
  | EMerge _ | ENot _ | EAnd (_, _) | EGe (_, _) -> assert false

let threshold_of_linears (cond : Condition.t) (l1 : linear) (l2 : linear) =
  let const fside oside =
    match fside, oside with
    | None, None -> None
    | Some e, None -> Some (ENeg e)
    | None, e -> e
    | Some e1, Some e2 -> Some (EAdd (e2, ENeg e1))
  in
  let with_const var f edge c =
    Option.map (fun c ->
        {
          var; edge;
          value = { eq_act = cond; eq_expr = EMult (c, EInv f) };
        })
      c
  in
  match l1.factor, l2.factor with
  | None, None -> None
  | Some (v1,e1), Some (v2,e2) ->
    Format.eprintf "{v%d}%a = {v%d}%a@."
      (Variable.uid v1) FormatEqu.print_expr e1
      (Variable.uid v2) FormatEqu.print_expr e2;
    Errors.raise_error "Unable to properly solve limits with factors on both sides"
  | Some (var, f), None -> with_const var f Raising (const l1.const l2.const)
  | None, Some (var, f) -> with_const var f Falling (const l2.const l1.const)

let thresholds_of_vals (v1 : val_exprs) (v2 : val_exprs) =
  let no_val = { factor = None; const = None } in
  match v1, v2 with
  | Zero, Zero -> []
  | Exprs vs, Zero ->
    List.filter_map (fun { eq_expr; eq_act } ->
        let l = linear_of_expr eq_expr in
        threshold_of_linears eq_act l no_val)
      vs
  | Zero, Exprs vs ->
    List.filter_map (fun { eq_expr; eq_act } ->
        let l = linear_of_expr eq_expr in
        threshold_of_linears eq_act no_val l)
      vs
  | Exprs vs1, Exprs vs2 ->
    List.flatten @@
    List.map (fun v1 ->
        List.filter_map (fun v2 ->
            let cond = Condition.conj v1.eq_act v2.eq_act in
            if Condition.is_never cond then None else
              let l1 = linear_of_expr v1.eq_expr in
              let l2 = linear_of_expr v2.eq_expr in
              threshold_of_linears cond l1 l2)
          vs2)
      vs1

let rec expr_limits acc (evt : Variable.t) (e : guarded_eq) =
  match e.eq_expr with
  | EConst _ | ENeg _ | EInv _ | EAdd _ | EMult _ | EMerge [] -> assert false
  | EVar _ | EPre _ | EMerge _ -> acc
  | ENot eq_expr -> expr_limits acc evt { e with eq_expr }
  | EAnd (e1, e2) ->
    let acc = expr_limits acc evt { e with eq_expr = e1 } in
    expr_limits acc evt { e with eq_expr = e2 }
  | EGe (e1, e2) ->
    let acc, val1 = expr_val acc { e with eq_expr = e1 } in
    let acc, val2 = expr_val acc { e with eq_expr = e2 } in
    add_limits acc evt (thresholds_of_vals val1 val2)

let compute (p : program) =
  let acc = {
    info = Surface.Ast.(p.infos.nvar_info);
    act_eqs = p.act_eqs;
    val_eqs = p.val_eqs;
    val_memo = Variable.Map.empty;
    lim_memo = Variable.Map.empty;
  }
  in
  let acc =
    Variable.Map.fold (fun evt eq acc -> expr_limits acc evt eq)
      p.act_eqs acc
  in
  acc.lim_memo
