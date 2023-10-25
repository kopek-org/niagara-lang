open Internal
open Ir

(* Very similar type from Ir.eqex, maybe merge them. Although the difference
   between [Switch] constructor and BDT might make things tedious in some
   places. *)
type expr =
  | Zero
  | Src
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

type prov_exprs = expr sourced Variable.Map.t

let rec print_expr fmt (expr : expr) =
  match expr with
  | Src -> Format.fprintf fmt "[src]"
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
      Format.fprintf fmt "@[<hv 2>provs %d:@ " (Variable.uid dest);
      Variable.Map.iter (fun src expr ->
          Format.fprintf fmt "@[<hv 2>from %d:@ %a@],@ "
            (Variable.uid src)
            print_expr expr)
        prov.pinned_src;
      Format.fprintf fmt "from another:@ %a" print_expr prov.other_src;
      Format.fprintf fmt "@]@,")
    provs;
  Format.fprintf fmt "@]@."

let add_expr (expr1 : expr) (expr2 : expr) =
  match expr1, expr2 with
  | Zero , expr
  | expr, Zero -> expr
  | Mult (e1, Src), Mult (e2, Src) -> Mult (Add (e1, e2), Src)
  | _ -> Add (expr1, expr2)

let rec apply_expr (lexpr : expr) (xexpr : expr) =
  match lexpr with
  | Zero | Const _ | Pre _ | Current _ -> lexpr
  | Src -> xexpr
  | Mult (le1, le2) -> Mult (apply_expr le1 xexpr, apply_expr le2 xexpr)
  | Add (le1, le2) -> Add (apply_expr le1 xexpr, apply_expr le2 xexpr)
  | Switch { evt; before; after } ->
    Switch { evt; before = apply_expr before xexpr; after = apply_expr after xexpr }

let merge_prov_exprs (f : expr option -> expr option -> expr option)
    (p1 : prov_exprs) (p2 : prov_exprs) =
  let opt_pinned = function
    | None -> Variable.Map.empty
    | Some s -> s.pinned_src
  in
  let opt_other = Option.map (fun s -> s.other_src) in
  Variable.Map.merge (fun _dest expr1 expr2 ->
      Some {
        pinned_src = Variable.Map.merge (fun _src -> f)
            (opt_pinned expr1) (opt_pinned expr2);
        other_src =
            match f (opt_other expr1) (opt_other expr2) with
            | None -> Zero
            | Some e -> e;
      }
    )
    p1 p2

let prov_exprs_union (p1 : prov_exprs) (p2 : prov_exprs) =
  let merge_sexpr se1 se2 =
    { pinned_src =
        Variable.Map.union (fun _dest e1 e2 -> Some (add_expr e1 e2))
          se1.pinned_src se2.pinned_src;
      other_src = add_expr se1.other_src se2.other_src;
    }
  in
  Variable.Map.union (fun _dest expr1 expr2 ->
      Some (merge_sexpr expr1 expr2))
    p1 p2

(* The following transforms trees to provenance expressions, the source being
   the one of the given tree. *)

let rec formula_prov (provs : prov_exprs) (cf : formula) : expr sourced =
  match cf with
  | Literal l ->
    { pinned_src = Variable.Map.empty; other_src = Const l }
  | Variable (v, view) ->
    let wrap_view =
      match view with
      | AtInstant -> Fun.id
      | Cumulated -> function
        | Zero -> Pre v
        | e -> Add (e, Pre v)
    in
    begin match Variable.Map.find_opt v provs with
      | None ->
        {
          pinned_src =
            Variable.Map.singleton v (wrap_view Src);
          other_src = wrap_view (Current v);
        }
      | Some srcmap ->
        {
          pinned_src = Variable.Map.map wrap_view srcmap.pinned_src;
          other_src = wrap_view (Add (Current v, srcmap.other_src));
        }
    end
  | Binop (op, f1, f2) ->
    let se1 = formula_prov provs f1 in
    let se2 = formula_prov provs f2 in
    let op e1 e2 =
      match op with
      | Add -> Add (e1, e2)
      | Mult -> Mult (e1, e2)
      | _ -> assert false
    in
    { pinned_src =
        Variable.Map.merge (fun _src e1 e2 ->
            match e1, e2 with
            | None, None -> None
            | Some e1, None -> Some (op e1 se2.other_src)
            | None, Some e2 -> Some (op se1.other_src e2)
            | Some e1, Some e2 -> Some (op e1 e2)
          )
          se1.pinned_src se2.pinned_src;
      other_src = op se1.other_src se2.other_src;
    }

let redist_prov (type a) (src : Variable.t) (r : a RedistTree.redist) : prov_exprs =
  match r with
  | RedistTree.NoInfo -> Variable.Map.empty
  | RedistTree.Shares shares ->
    Variable.Map.map (fun f ->
        { pinned_src =
            Variable.Map.singleton src (Mult (Const (LRational f), Src));
          other_src = Zero;
        })
      shares
  | RedistTree.Flats fs ->
    Variable.Map.map (formula_prov Variable.Map.empty) fs.transfers
    (* We can ignore deficit balancing, as they have no effect on equations *)

let rec tree_prov : type a. Variable.t -> a RedistTree.tree -> prov_exprs =
  fun src tree ->
  match tree with
  | Nothing -> Variable.Map.empty
  | Redist r -> redist_prov src r
  | When wr ->
    List.fold_left (fun provs (evt, r) ->
        let prov = tree_prov src r in
        merge_prov_exprs (fun p w ->
            match p, w with
            | None, None -> assert false
            | Some e, None -> Some e
            | None, Some on_when ->
              Some (Switch { evt; before = Zero; after = on_when })
            | Some e, Some on_when ->
              Some (Switch { evt; before = e; after = add_expr e on_when}))
          provs prov)
      Variable.Map.empty wr
  | Branch { evt; before; after } ->
    let befores = tree_prov src before in
    let afters = tree_prov src after in
    merge_prov_exprs (fun e1 e2 ->
        let before = Option.value ~default:Zero e1 in
        let after = Option.value ~default:Zero e2 in
        if before = after then Some before else
          Some (Switch { evt; before; after }))
      befores afters

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
  let rec memoize tprovs dest =
    match Variable.Map.find_opt dest tprovs with
    | Some se -> tprovs, se
    | None ->
      let se = Variable.Map.find dest provs in
      let tprovs, pinned_src =
        Variable.Map.fold (fun src e (tprovs, ps) ->
            let tprovs, sps =
              if Variable.Map.mem src provs then
                let tprovs, sse = memoize tprovs src in
                tprovs, Variable.Map.map (fun e' ->
                    apply_expr e e')
                  sse.pinned_src
              else tprovs, Variable.Map.singleton src e
            in
            let ps =
              Variable.Map.union (fun _src e e' ->
                  Some (add_expr e e'))
                ps sps
            in
            tprovs, ps)
          se.pinned_src (tprovs, Variable.Map.empty)
      in
      let se = { se with pinned_src } in
      Variable.Map.add dest se tprovs, se
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
    | Src -> BDT.Action ESrc
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

let event_condition (provs : prov_exprs) (event : event) : cond BDT.t sourced =
  let merge_eq d1 d2 =
    Variable.BDT.merge (fun _k e1 e2 ->
        match e1, e2 with
        | None, None
        | Some _ , None
        | None, Some _ -> None
        | Some e1, Some e2 ->
          Some (CEq (e1, e2)))
      d1 d2
  in
  match event with
  | EvtVar evt ->
    { pinned_src = Variable.Map.empty; other_src = Action (CRef evt) }
  | EvtOnRaise evt ->
    { pinned_src = Variable.Map.empty; other_src = Action (CRaising evt)}
  | EvtComp (Eq, f1, f2) ->
    let se1 = formula_prov provs f1 in
    let se2 = formula_prov provs f2 in
    let pinned_src =
      Variable.Map.merge (fun _src e1 e2 ->
          let e1, e2 =
            match e1 with
            | Some e1 -> begin
                match e2 with
                | Some e2 -> e1, e2
                (* Errors.raise_error "source term present in both side of equation"; *)
                | None -> e1, se2.other_src
              end
            | None ->
              match e2 with
              | None -> se1.other_src, se2.other_src
              | Some e2 -> se1.other_src, e2
          in
          let d1 = lift_expr e1 in
          let d2 = lift_expr e2 in
          let eq = merge_eq d1 d2 in
          Some eq)
        se1.pinned_src se2.pinned_src
    in
    let other_src = merge_eq (lift_expr se1.other_src) (lift_expr se2.other_src) in
    { pinned_src; other_src }
  | EvtDate _ -> assert false
  | EvtAnd (_, _) -> assert false
  | EvtOr (_, _) -> assert false

let condition_equations (program : program) (provs : prov_exprs) : event_eq Variable.Map.t =
  Variable.Map.map (event_condition provs) program.events

(* [when e] events are transformed to [not (already_reached e) && (test e)].
   Mainly to avoid equation dependencies. *)
let transform_raising_cond (eqs : event_eq Variable.Map.t) =
  let reduce_decide_on evt d bdd =
    if bdd = BDT.NoAction then bdd else
      let bdd = BDT.add_decision evt bdd in
      BDT.map_action (Variable.Map.singleton evt (not d))
        (fun _k _c -> NoAction)
        bdd
  in
  let trans_raising cond =
    match cond with
    | CEq _ | CNorm _ ->
      { pinned_src = Variable.Map.empty;
        other_src = BDT.Action cond;
      }
    | CRef evt -> Variable.Map.find evt eqs
    | CRaising evt ->
      let srcd = Variable.Map.find evt eqs in
      { pinned_src =
          Variable.Map.map (reduce_decide_on evt false) srcd.pinned_src;
        other_src = reduce_decide_on evt false srcd.other_src;
      }
  in
  Variable.Map.map (fun sourced ->
      let pinned_src =
        Variable.Map.mapi (fun src bdd ->
            BDT.map_action Variable.Map.empty (fun k act ->
                match act with
                | None -> NoAction
                | Some cond ->
                  BDT.cut k (get_source src (trans_raising cond)))
              bdd)
          sourced.pinned_src
      in
      let any_source a = { pinned_src = Variable.Map.empty; other_src = a } in
      let merge_bdd d1 d2 =
        BDT.merge (fun _k c1 c2 ->
            match c1, c2 with
            | None, None -> None
            | None, c | c, None -> c
            | Some c1, Some c2 ->
              Format.eprintf "merging %a@.%a@."
              FormatIr.print_cond c1 FormatIr.print_cond c2;
              assert false)
          d1 d2
      in
      let default_src =
        BDT.fold ~noaction:(fun _k -> any_source BDT.NoAction)
          ~action:(fun k cond ->
              let srcd = trans_raising cond in
              { pinned_src = Variable.Map.map (BDT.cut k) srcd.pinned_src;
                other_src = BDT.cut k srcd.other_src;
              })
          ~decision:(fun _k evt s1 s2 ->
              { pinned_src =
                  Variable.Map.union (fun _src c1 c2 ->
                      Some (merge_bdd c1 c2))
                    (Variable.Map.map (reduce_decide_on evt true) s1.pinned_src)
                    (Variable.Map.map (reduce_decide_on evt false) s2.pinned_src);
                other_src =
                  merge_bdd
                    (reduce_decide_on evt true s1.other_src)
                    (reduce_decide_on evt false s2.other_src)
              }
            )
          sourced.other_src
      in
      { pinned_src =
          Variable.Map.union (fun _src bdd _ -> Some bdd)
            pinned_src default_src.pinned_src;
        other_src = default_src.other_src;
      })
    eqs

(* Equations are assumed linear, normalization to dynamically compute limits is
   trival. This hypothesis could be lifted as we could express polynomial
   equations. Solving them would require more work, but I have the intuition it
   could be not that bad in practice, since we only need to compute one limit at
   a time (the closest). *)
let rec extract_src_factor (e : eqex) =
  match e with
  | ESrc -> EConst (LRational R.one), EZero
  | EZero | EConst _ | EVar _ | ECurrVar _ -> EZero, e
  | EMult (e1, e2) ->
    let sf1, c1 = extract_src_factor e1 in
    let sf2, c2 = extract_src_factor e2 in
    (* sf1*sf2X^2 + sf1X*c2 + sf2X*c1 + c1*c2 *)
    let src_factor =
      match sf1, sf2 with
      | EZero, _ -> EMult(sf2, c1)
      | _, EZero -> EMult(sf1, c2)
      | _ -> Errors.raise_error "Polynomial expression found in equation"
    in
    let const_part =
      match c1, c2 with
      | EZero, _ | _, EZero -> EZero
      | _ -> EMult (c1, c2)
    in
    src_factor, const_part
  | EAdd (e1, e2) ->
    let sf1, e1 = extract_src_factor e1 in
    let sf2, e2 = extract_src_factor e2 in
    (if sf1 = EZero then sf2
    else if sf2 = EZero then sf1
    else EAdd (sf1, sf2)),
    if e1 = EZero then e2
    else if e2 = EZero then e1
    else EAdd (e1, e2)
  | EMinus e ->
    let sf, e = extract_src_factor e in
    (if sf = EZero then EZero else EMinus sf),
    if e = EZero then e else EMinus e

let normalize_condition (e : cond) =
  match e with
  | CRef _ | CRaising _ | CNorm _ -> e
  | CEq (e1, e2) ->
    let src_factor1, const_part1 = extract_src_factor e1 in
    let src_factor2, const_part2 = extract_src_factor e2 in
    let src_factor =
      match src_factor1, src_factor2 with
      | EZero, EZero -> EZero
      | EZero, e -> EMinus e
      | e, EZero -> e
      | e1, e2 ->
        EAdd (e1, EMinus e2)
    in
    let const =
      match const_part1, const_part2 with
      | EZero, EZero -> EZero
      | EZero, e -> e
      | e, EZero -> EMinus e
      | e1, e2 ->
        EAdd (e2, EMinus e1)
    in
    CNorm { src_factor; const }

let normalize_equations (eqs : event_eq Variable.Map.t) : event_eq Variable.Map.t =
  let eqs = transform_raising_cond eqs in
  let norm_bdd =
    Variable.BDT.map_action Variable.Map.empty (fun _k -> function
        | None -> NoAction
        | Some c -> Action (normalize_condition c))
  in
  Variable.Map.map (fun sourced ->
      { pinned_src =
          Variable.Map.map norm_bdd sourced.pinned_src;
        other_src = norm_bdd sourced.other_src;
      })
    eqs

let compute_threshold_equations (prog : program) =
  let provs = provenance_expressions prog in
  let tprovs = prov_transitivity provs in
  let eqs = condition_equations prog tprovs in
  let eqs = normalize_equations eqs in
  Format.eprintf "%a@." FormatIr.print_conditions eqs;
  {
    infos = prog.infos;
    trees = prog.trees;
    events = prog.events;
    equations = eqs;
    eval_order = prog.eval_order;
  }
