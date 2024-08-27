open Internal
open Ir

(* Very similar type from Ir.eqex, maybe merge them. Although the difference
   between [Switch] constructor and BDT might make things tedious in some
   places. *)
type expr =
  | Zero
  | Const of Literal.t
  | Mult of expr * expr
  | Switch of {
      evt : Variable.t;
      before : expr;
      after : expr;
    }
  | Add of expr * expr
  | Pre of Variable.t
  | Current of Variable.t

type affine_expr = { factor : expr; const : expr }

type nf_expr = affine_expr sourced

module BDT = Variable.BDT

type prov_exprs = nf_expr Variable.Map.t

let rec print_expr fmt (expr : expr) =
  match expr with
  | Zero -> Format.fprintf fmt "0"
  | Const l -> Literal.print fmt l
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

let print_affine_expr fmt (e : affine_expr) =
  Format.fprintf fmt "@[<hv 2>(%a*[src] + %a@])"
    print_expr e.factor
    print_expr e.const

let print_nf_expr fmt (e : nf_expr) =
  Variable.Map.iter (fun src ae ->
      Format.fprintf fmt "@[<hv 2>from %d:@ %a@],@ "
        (Variable.uid src)
        print_affine_expr ae)
    e.pinned_src;
  Format.fprintf fmt "from another:@ %a" print_affine_expr e.other_src

let _print_provenances fmt (provs : prov_exprs) =
  Format.pp_open_vbox fmt 0;
  Variable.Map.iter (fun dest prov ->
      Format.fprintf fmt "@[<hv 2>provs %d:@ %a@]@,"
        (Variable.uid dest)
        print_nf_expr prov)
    provs;
  Format.fprintf fmt "@]@."

let add_expr (expr1 : expr) (expr2 : expr) : expr =
  match expr1, expr2 with
  | Zero , expr
  | expr, Zero -> expr
  | e1, e2 -> Add (e1, e2)

let mult_expr (e1 : expr) (e2 : expr) : expr =
  match e1, e2 with
  | Zero, _ | _, Zero -> Zero
  | _ -> Mult (e1, e2)

let add_affine_expr (e1 : affine_expr) (e2 : affine_expr) : affine_expr =
  { factor = add_expr e1.factor e2.factor;
    const = add_expr e1.const e2.const;
  }

let add_nf_expr (e1 : nf_expr) (e2 : nf_expr) : nf_expr =
  merge_sources add_affine_expr e1 e2

let mult_nf_expr (expr1 : nf_expr) (expr2 : nf_expr) : nf_expr =
  merge_sources (fun e1 e2 ->
      if e1.factor <> Zero && e2.factor <> Zero then
        Errors.raise_error "Polynomial expression found";
      { factor = add_expr (mult_expr e1.factor e2.const)
                          (mult_expr e2.factor e1.const);
        const = mult_expr e1.const e2.const;
      })
    expr1 expr2

let merge_prov_exprs (f : nf_expr option -> nf_expr option -> nf_expr option)
    (p1 : prov_exprs) (p2 : prov_exprs) =
  Variable.Map.merge (fun _dest -> f) p1 p2

let prov_exprs_union (p1 : prov_exprs) (p2 : prov_exprs) =
  Variable.Map.union (fun _dest expr1 expr2 ->
      Some (add_nf_expr expr1 expr2))
    p1 p2

(* The following transforms trees to provenance expressions, the source being
   the one of the given tree. *)

let rec formula_prov (provs : prov_exprs) (cf : formula) : nf_expr =
  match cf with
  | Literal l ->
    { pinned_src = Variable.Map.empty;
      other_src = { factor = Zero; const = Const l }
    }
  | Variable (v, view) ->
    let wrap_view ae =
      match view with
      | AtInstant -> ae
      | Cumulated ->
        { factor = ae.factor; const = add_expr ae.const (Pre v) }
    in
    begin match Variable.Map.find_opt v provs with
      | Some osrc ->
        { pinned_src = Variable.Map.map wrap_view osrc.pinned_src;
          other_src = wrap_view osrc.other_src }
      | None ->
        { pinned_src = Variable.Map.singleton v
              (wrap_view { factor = Const (LRational R.one); const = Zero });
          other_src = wrap_view { factor = Zero; const = Current v }
        }
    end
  | Binop (op, f1, f2) ->
    let e1 = formula_prov provs f1 in
    let e2 = formula_prov provs f2 in
    begin match op with
      | Add -> add_nf_expr e1 e2
      | Mult -> mult_nf_expr e1 e2
      | _ -> assert false
    end

let redist_prov (type a) (src : Variable.t) (r : a RedistTree.redist) : prov_exprs =
  match r with
  | RedistTree.NoInfo -> Variable.Map.empty
  | RedistTree.Shares shares ->
    Variable.Map.map (function
        | RedistTree.Remain -> (* handled elsewhere *)
          { pinned_src = Variable.Map.empty;
            other_src =
              { factor = Zero;
                const = Zero }
          }
        | Part f ->
        { pinned_src =
            Variable.Map.singleton src
              { factor = Const (LRational f);
                const = Zero };
          other_src =
            { factor = Zero;
              const = Zero }
        })
      shares
  | RedistTree.Flats fs ->
    Variable.Map.map (formula_prov Variable.Map.empty) fs.transfers
    (* We can ignore deficit balancing, as they have no effect on equations *)

let tree_prov (type a) (src : Variable.t) (tree : a RedistTree.tree) =
  Variable.BDT.fold tree
    ~noaction:(fun _ -> Variable.Map.empty)
    ~action:(fun _ r -> redist_prov src r)
    ~decision:(fun _ evt afters befores ->
        merge_prov_exprs (fun e1 e2 ->
            let sourced_zero =
              { pinned_src = Variable.Map.empty; other_src = { factor = Zero; const = Zero }}
            in
            let before = Option.value ~default:sourced_zero e1 in
            let after = Option.value ~default:sourced_zero e2 in
            let smart_branch evt before after =
              match before, after with
              | Zero, Zero -> Zero
              | _ -> Switch { evt; before; after }
            in
            Some (merge_sources (fun b a ->
                { factor = smart_branch evt b.factor a.factor;
                  const = smart_branch evt b.const a.const
                })
                before after))
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
  let rec subst tprovs (e : expr) : prov_exprs * expr =
    match e with
    | Zero | Const _ | Pre _ -> tprovs, e
    | Current v ->
      if Variable.Map.mem v provs then
        let tprovs, nfe = memoize tprovs v in
        tprovs, nfe.other_src.const
      else tprovs, e
    | Mult (e1, e2) ->
      let tprovs, e1 = subst tprovs e1 in
      let tprovs, e2 = subst tprovs e2 in
      tprovs, Mult (e1, e2)
    | Add (e1, e2) ->
      let tprovs, e1 = subst tprovs e1 in
      let tprovs, e2 = subst tprovs e2 in
      tprovs, add_expr e1 e2
    | Switch { evt; before; after } ->
      let tprovs, before = subst tprovs before in
      let tprovs, after = subst tprovs after in
      tprovs, Switch { evt; before; after }
  and untangle_sources tprovs sourced =
    let tprovs, pinned_src =
      Variable.Map.fold (fun src e (tprovs, np) ->
          let tprovs, snp =
            if Variable.Map.mem src provs then
              let tprovs, tse = memoize tprovs src in
              let snp =
                Variable.Map.map (fun te ->
                    { factor = mult_expr e.factor te.factor;
                      const = add_expr (mult_expr e.factor te.const) e.const })
                  tse.pinned_src
              in
              tprovs, snp
            else
              tprovs, Variable.Map.singleton src e
          in
          let np =
            Variable.Map.union (fun _src e1 e2 ->
                Some (add_affine_expr e1 e2))
              np snp
          in
          tprovs, np)
        sourced.pinned_src (tprovs, Variable.Map.empty)
    in
    let tprovs, osrc = subst tprovs sourced.other_src.const in
    let other_src = { factor = Zero; const = osrc } in
    tprovs, { pinned_src; other_src }
  and memoize tprovs dest : prov_exprs * nf_expr =
    match Variable.Map.find_opt dest tprovs with
    | Some se -> tprovs, se
    | None ->
      let e = Variable.Map.find dest provs in
      let tprovs, e' = untangle_sources tprovs e in
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

let eq_normal_form (el : affine_expr) (er : affine_expr) : nf_eq BDT.t =
  let fel = lift_expr el.factor in
  let cel = lift_expr el.const in
  let fer = lift_expr er.factor in
  let cer = lift_expr er.const in
  let src_factor =
    BDT.merge (fun _ fl fr ->
        match fl, fr with
        | None, None -> None
        | Some fl, None -> Some fl
        | None, Some fr -> Some (EMinus fr)
        | Some fl, Some fr ->
          Some (EAdd (fl, EMinus fr)))
      fel fer
  in
  let const =
    BDT.merge (fun _ cl cr ->
        match cr, cl with
        | None, None -> None
        | Some cr, None -> Some cr
        | None, Some cl -> Some (EMinus cl)
        | Some cr, Some cl ->
          Some (EAdd (cr, EMinus cl)))
      cel cer
  in
  BDT.merge (fun _ f c ->
      match f, c with
      | None, _ | _, None -> None
      | Some src_factor, Some const ->
        Some { src_factor; const })
    src_factor const

let equality_to_equation (provs : prov_exprs) (f1 : formula) (f2 : formula) : event_eq =
  let e1 = formula_prov provs f1 in
  let e2 = formula_prov provs f2 in
  merge_sources eq_normal_form e1 e2

let condition_equations (prog : program) (provs : prov_exprs) : event_eq Variable.Map.t =
  let rec conv eqs expr : _ * event_eq =
    match expr with
    | EvtComp (Eq, f1, f2) -> eqs, equality_to_equation provs f1 f2
    | EvtVar evt -> memoize eqs evt
    | EvtOnRaise evt ->
      let eqs, eq = memoize eqs evt in
      (* [when e] events are transformed to [not (already_reached e) && (test e)].
         Mainly to avoid equation dependencies. *)
      let eq =
        map_source (fun eq ->
            if eq = BDT.NoAction then eq else
              BDT.only_when (Variable.Map.singleton evt false) eq)
          eq
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
    dep_graph = prog.dep_graph;
    eval_order = prog.eval_order;
  }
