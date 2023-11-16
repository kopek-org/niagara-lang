open Internal
open Ir

(* Very similar type from Ir.eqex, maybe merge them. Although the difference
   between [Switch] constructor and BDT might make things tedious in some
   places. *)
type expr =
  | Zero
  | Const of literal
  | Mult of expr * expr
  | Switch of {
      evt : Variable.t;
      before : expr;
      after : expr;
    }
  | Add of expr * expr
  | Pre of Variable.t
  | Current of Variable.t

module BDT = Variable.BDT

type prov_exprs = expr Variable.Map.t

let rec print_expr fmt (expr : expr) =
  match expr with
  | Zero -> Format.fprintf fmt "0"
  | Const l -> FormatIr.print_literal fmt l
  | Mult (e1, e2) -> Format.fprintf fmt "%a*%a" print_expr e1 print_expr e2
  | Switch { evt; before; after } ->
    Format.fprintf fmt "@[<hv 2>(v%d ?@ %a@ : %a)@]"
      (Variable.uid evt)
      print_expr before
      print_expr after
  | Add (expr1, expr2) ->
    Format.fprintf fmt "@[<hv>(%a@ + %a)@]"
      print_expr expr1
      print_expr expr2
  | Pre v -> Format.fprintf fmt "v%d" (Variable.uid v)
  | Current v -> Format.fprintf fmt "v%d'" (Variable.uid v)

let _print_provenances fmt (provs : prov_exprs) =
  Format.pp_open_vbox fmt 0;
  Variable.Map.iter (fun dest prov ->
      Format.fprintf fmt "@[<hv 2>provs %d:@ %a@]@,"
        (Variable.uid dest)
        print_expr prov)
    provs;
  Format.fprintf fmt "@]@."

let add_expr (expr1 : expr) (expr2 : expr) =
  match expr1, expr2 with
  | Zero , expr
  | expr, Zero -> expr
  | _ -> Add (expr1, expr2)

let merge_prov_exprs (f : expr option -> expr option -> expr option)
    (p1 : prov_exprs) (p2 : prov_exprs) =
  Variable.Map.merge (fun _dest -> f) p1 p2

let prov_exprs_union (p1 : prov_exprs) (p2 : prov_exprs) =
  Variable.Map.union (fun _dest expr1 expr2 ->
      Some (add_expr expr1 expr2))
    p1 p2

(* The following transforms trees to provenance expressions, the source being
   the one of the given tree. *)

let rec formula_prov (provs : prov_exprs) (cf : formula) : expr =
  match cf with
  | Literal l -> Const l
  | Variable (v, view) ->
    let wrap_view =
      match view with
      | AtInstant -> Fun.id
      | Cumulated -> function
        | Zero -> Pre v
        | e -> Add (e, Pre v)
    in
    begin match Variable.Map.find_opt v provs with
      | None -> wrap_view (Current v)
      | Some osrc -> wrap_view (Add (Current v, osrc))
    end
  | Binop (op, f1, f2) ->
    let e1 = formula_prov provs f1 in
    let e2 = formula_prov provs f2 in
    begin match op with
      | Add -> Add (e1, e2)
      | Mult -> Mult (e1, e2)
      | _ -> assert false
    end

let redist_prov (type a) (src : Variable.t) (r : a RedistTree.redist) : prov_exprs =
  match r with
  | RedistTree.NoInfo -> Variable.Map.empty
  | RedistTree.Shares shares ->
    Variable.Map.map (fun f -> Mult (Const (LRational f), Current src)) shares
  | RedistTree.Flats fs ->
    Variable.Map.map (formula_prov Variable.Map.empty) fs.transfers
    (* We can ignore deficit balancing, as they have no effect on equations *)

let tree_prov (type a) (src : Variable.t) (tree : a RedistTree.tree) =
  Variable.BDT.fold tree
    ~noaction:(fun _ -> Variable.Map.empty)
    ~action:(fun _ r -> redist_prov src r)
    ~decision:(fun _ evt afters befores ->
        merge_prov_exprs (fun e1 e2 ->
            let before = Option.value ~default:Zero e1 in
            let after = Option.value ~default:Zero e2 in
            if before = after then Some before else
              Some (Switch { evt; before; after }))
          befores afters)

let trees_prov (src : Variable.t) (t : RedistTree.t) : prov_exprs =
  match t with
  | Flat fs ->
    List.fold_left (fun acc tree ->
        let provs = tree_prov src tree in
        prov_exprs_union acc provs)
      Variable.Map.empty fs
  | Fractions { base_shares; balance; branches } ->
    let branches =
      match balance with
      | BalanceTree bt -> bt::branches
      | BalanceVars _ ->
        Errors.raise_error "(internal) Balance attributions should have been computed away"
    in
    List.fold_left (fun acc tree ->
        let provs = tree_prov src tree in
        prov_exprs_union acc provs)
      (redist_prov src base_shares) branches

let provenance_expressions (program : program) : prov_exprs =
  Variable.Map.fold (fun source ts provs ->
      let tprovs = trees_prov source ts in
      prov_exprs_union tprovs provs)
    program.trees Variable.Map.empty

(* Propagate provenance until we reach actual inputs. We might want to keep
   every intermediate points in the future, for testing and simulations, this is
   a discussion relevant to plausible features. *)
let prov_transitivity (provs : prov_exprs) : prov_exprs =
  let rec subst tprovs e =
    match e with
    | Zero | Const _ | Pre _ -> tprovs, e
    | Current v ->
      if Variable.Map.mem v provs then memoize tprovs v else tprovs, e
    | Mult (e1, e2) ->
      let tprovs, e1 = subst tprovs e1 in
      let tprovs, e2 = subst tprovs e2 in
      tprovs, Mult (e1, e2)
    | Add (e1, e2) ->
      let tprovs, e1 = subst tprovs e1 in
      let tprovs, e2 = subst tprovs e2 in
      tprovs,  Add (e1, e2)
    | Switch { evt; before; after } ->
      let tprovs, before = subst tprovs before in
      let tprovs, after = subst tprovs after in
      tprovs, Switch { evt; before; after }
  and memoize tprovs dest =
    match Variable.Map.find_opt dest tprovs with
    | Some se -> tprovs, se
    | None ->
      let e = Variable.Map.find dest provs in
      let tprovs, e' = subst tprovs e in
      Variable.Map.add dest e' tprovs, e'
  in
  Variable.Map.fold (fun dest _se tprovs -> fst @@ memoize tprovs dest)
    provs Variable.Map.empty

(* The following transforms event expression into equations using previously
   definied provenance.

   An important note: there is an hidden hypothesis on the way equations are
   specified by the user. [v = v'] as an event really means "when v reach v'",
   implying that [v] is assumed to be initially lower than [v']. This is
   important because of the semantics of threshold passing: the event is deemed
   reached when [v >= v'], and not reach yet (or reverted!) when [v < v']. *)

let lift_expr (expr : expr) : eqex BDT.t =
  let rec aux decided expr =
    match expr with
    | Zero -> BDT.NoAction
    | Const l -> BDT.Action (EConst l)
    | Mult (e1, e2) ->
      let d1 = aux decided e1 in
      let d2 = aux decided e2 in
      Variable.BDT.merge
        (fun _k e1 e2 ->
           match e1, e2 with
           | None, _ | _, None -> None
           | Some e1, Some e2 -> Some (EMult (e1, e2))
        )
        d1 d2
    | Pre v -> Action (EVar v)
    | Current v -> Action (ECurrVar v)
    | Add (e1, e2) ->
      let d1 = aux decided e1 in
      let d2 = aux decided e2 in
      Variable.BDT.merge
        (fun _k e1 e2 ->
           match e1, e2 with
           | None, None -> None
           | Some e, None
           | None, Some e -> Some e
           | Some e1, Some e2 -> Some (EAdd (e1, e2))
        )
        d1 d2
    | Switch { evt; before; after } ->
      match Variable.Map.find_opt evt decided with
      | None ->
        let b = aux (Variable.Map.add evt true decided) before in
        let a = aux (Variable.Map.add evt false decided) after in
        Decision (evt, a, b)
      | Some decision ->
        let e = if decision then before else after in
        aux decided e
  in
  aux Variable.Map.empty expr

let rec sourced_nf_eqex (e : eqex) =
  match e with
  | EZero | EConst _ | EVar _ ->
    { pinned_src = Variable.Map.empty; other_src = { src_factor = EZero; const = e } }
  | ECurrVar src ->
    { pinned_src =
        Variable.Map.singleton src
          { src_factor = EConst (LRational R.one); const = EZero };
      other_src = { src_factor = EZero; const = EZero } }
  | EMult (e1, e2) ->
    let se1 = sourced_nf_eqex e1 in
    let se2 = sourced_nf_eqex e2 in
    merge_sources (fun e1 e2 ->
        let src_factor =
          match e1.src_factor, e2.src_factor with
          | EZero, sf2 -> EMult(sf2, e1.const)
          | sf1, EZero -> EMult(sf1, e2.const)
          | _ -> Errors.raise_error "Polynomial expression"
        in
        let const =
          match e1.const, e2.const with
          | EZero, _ | _, EZero -> EZero
          | c1, c2 -> EMult (c1, c2)
        in
        { src_factor; const })
      se1 se2
  | EAdd (e1, e2) ->
    let se1 = sourced_nf_eqex e1 in
    let se2 = sourced_nf_eqex e2 in
    merge_sources (fun e1 e2 ->
        { src_factor = EAdd (e1.src_factor, e2.src_factor);
          const = EAdd (e1.const, e2.const) })
      se1 se2
  | EMinus e ->
    let se = sourced_nf_eqex e in
    map_source (fun e ->
        { src_factor = EMinus e.src_factor; const = EMinus e.const })
      se

let eq_normal_form (e1 : eqex) (e2 : eqex) =
  let se1 = sourced_nf_eqex e1 in
  let se2 = sourced_nf_eqex e2 in
  merge_sources (fun e1 e2 ->
      let src_factor =
        EAdd (e1.src_factor, EMinus e2.src_factor)
      in
      let const = EAdd (e2.const, EMinus e1.const) in
      { src_factor; const })
    se1 se2

let equality_to_equation (provs : prov_exprs) (f1 : formula) (f2 : formula) =
  let e1 = lift_expr (formula_prov provs f1) in
  let e2 = lift_expr (formula_prov provs f2) in
  BDT.merge (fun _k e1 e2 ->
      match e1, e2 with
      | None, _ | _, None -> None
      | Some e1, Some e2 -> Some (eq_normal_form e1 e2))
    e1 e2

let condition_equations (prog : program) (provs : prov_exprs) =
  let rec conv eqs expr =
    match expr with
    | EvtComp (Eq, f1, f2) -> eqs, equality_to_equation provs f1 f2
    | EvtVar evt -> memoize eqs evt
    | EvtOnRaise evt ->
      let eqs, eq = memoize eqs evt in
      (* [when e] events are transformed to [not (already_reached e) && (test e)].
         Mainly to avoid equation dependencies. *)
      if eq = BDT.NoAction then eqs, eq else
        let eq =
          BDT.only_when (Variable.Map.singleton evt false) eq
        in
        eqs, eq
    | EvtAnd _ | EvtOr _ | EvtDate _ -> assert false
  and memoize eqs evt =
    match Variable.Map.find_opt evt eqs with
    | Some eq -> eqs, eq
    | None ->
      let expr = Variable.Map.find evt prog.events in
      let eqs, eq = conv eqs expr in
      Variable.Map.add evt eq eqs, eq
  in
  Variable.Map.fold (fun evt _ eqs ->
      fst @@ memoize eqs evt)
    prog.events Variable.Map.empty

let compute_threshold_equations (prog : program) =
  let provs = provenance_expressions prog in
  let tprovs = prov_transitivity provs in
  let eqs = condition_equations prog tprovs in
  {
    infos = prog.infos;
    trees = prog.trees;
    events = prog.events;
    equations = eqs;
    eval_order = prog.eval_order;
  }
