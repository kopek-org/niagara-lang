open Ir

type expr =
  | Zero
  | Src
  | Const of literal
  | Factor of float * expr
  | Switch of {
      evt : Variable.t;
      before : expr;
      after : expr;
    }
  | Add of expr * expr
  | Pre of Variable.t


type 'a bdd =
  | Decide of Variable.t * 'a bdd * 'a bdd
  | Do of 'a
  | Nothing

type eqex =
  | ESrc
  | EConst of literal
  | EMult of float * eqex
  | EAdd of eqex * eqex
  | ESub of eqex * eqex
  | EVar of Variable.t

type cond =
  | Ref of Variable.t
  | Eq of eqex * eqex

type 'a sourced = {
  pinned_src : 'a Variable.Map.t;
  other_src : 'a;
}

type prov_exprs = expr sourced Variable.Map.t

type event_eqs = cond bdd sourced Variable.Map.t

let merge_expr (expr1 : expr) (expr2 : expr) =
  match expr1, expr2 with
  | Zero , expr
  | expr, Zero -> expr
  | Factor (p1, Src), Factor (p2, Src) -> Factor (p1 +. p2, Src)
  | _, Factor (_, Src) -> Add (expr2, expr1)
  | _ -> Add (expr1, expr2)

let rec factor_expr (e : expr) (f : float) =
  match e with
  | Zero -> Zero
  | Src | Const _ | Pre _ -> Factor (f, e)
  | Factor (f', e) -> Factor (f*.f',e)
  | Add (e1, e2) -> Add (factor_expr e1 f, factor_expr e2 f)
  | Switch { evt; before; after } ->
    Switch { evt; before = factor_expr before f; after = factor_expr after f }

let rec apply_expr (lexpr : expr) (xexpr : expr) =
  match lexpr with
  | Zero | Const _ | Pre _ -> lexpr
  | Src -> xexpr
  | Factor (f, le) -> factor_expr (apply_expr le xexpr) f
  | Add (le1, le2) -> Add (apply_expr le1 xexpr, apply_expr le2 xexpr)
  | Switch { evt; before; after } ->
    Switch { evt; before = apply_expr before xexpr; after = apply_expr after xexpr }

let rec bdd_map (f : 'a -> 'b) (bdd : 'b bdd) =
  match bdd with
  | Decide (c, d1, d2) -> Decide (c, bdd_map f d1, bdd_map f d2)
  | Do e -> Do (f e)
  | Nothing -> Nothing

let merge_bdd (f : 'a option -> 'a option -> 'b option) (d1 : 'a bdd) (d2 : 'a bdd) =
  let rec run_down (f : 'a option -> 'b option) decided d =
    match d with
    | Nothing -> begin match f None with
        | None -> Nothing
        | Some e -> Do e
      end
    | Do e -> begin
        match f (Some e) with
        | None -> Nothing
        | Some e -> Do e
      end
    | Decide (c, d1, d2) ->
      match Variable.Map.find_opt c decided with
      | None -> Decide (c, run_down f decided d1, run_down f decided d2)
      | Some decision ->
        let d = if decision then d1 else d2 in
        run_down f decided d
  in
  let rec aux decided d1 d2 =
    match d1 with
    | Decide (c, d11, d12) -> begin
      match Variable.Map.find_opt c decided with
        | None ->
          Decide (c,
            aux (Variable.Map.add c true decided) d11 d2,
            aux (Variable.Map.add c false decided) d12 d2)
        | Some decision ->
          let d = if decision then d11 else d12 in
          aux decided d d2
    end
    | Do e -> run_down (f (Some e)) decided d2
    | Nothing -> run_down (f None) decided d2
  in
  aux Variable.Map.empty d1 d2

let lift_expr (expr : expr) : eqex bdd =
  let rec aux decided expr =
    match expr with
    | Zero -> Nothing
    | Src -> Do ESrc
    | Const l -> Do (EConst l)
    | Factor (f, e) ->
      let d = aux decided e in
      bdd_map (fun e -> EMult (f, e)) d
    | Pre v -> Do (EVar v)
    | Add (e1, e2) ->
      let d1 = aux decided e1 in
      let d2 = aux decided e2 in
      merge_bdd
        (fun e1 e2 ->
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
        Decide (evt, b, a)
      | Some decision ->
        let e = if decision then before else after in
        aux decided e
  in
  aux Variable.Map.empty expr

let merge_sourced_expr (se1 : expr sourced) (se2 : expr sourced) =
  { pinned_src =
      Variable.Map.union (fun _dest e1 e2 -> Some (merge_expr e1 e2))
        se1.pinned_src se2.pinned_src;
    other_src = merge_expr se1.other_src se2.other_src;
  }

let prov_exprs_union (p1 : prov_exprs) (p2 : prov_exprs) =
  Variable.Map.union (fun _dest expr1 expr2 ->
      Some (merge_sourced_expr expr1 expr2))
    p1 p2

let formula_prov (provs : prov_exprs) (cf : formula) : expr sourced =
  match cf with
  | Literal l ->
    { pinned_src = Variable.Map.empty; other_src = Const l }
  | Variable (v, view) ->
    let wrap_view =
      match view with
      | AtInstant -> Fun.id
      | Cumulated -> function
        | Zero -> Pre v
        | e -> Add (Pre v, e)
    in
    begin match Variable.Map.find_opt v provs with
      | None ->
        {
          pinned_src =
            Variable.Map.singleton v (wrap_view Src);
          other_src = wrap_view Zero;
        }
      | Some srcmap ->
        {
          pinned_src = Variable.Map.map (fun expr -> wrap_view expr) srcmap.pinned_src;
          other_src = wrap_view srcmap.other_src;
        }
    end
  | Binop (_, _, _) -> assert false
  | RCast _ -> assert false

let redist_prov (type a) (src : Variable.t) (r : a RedistTree.redist) : prov_exprs =
  match r with
  | RedistTree.NoInfo -> Variable.Map.empty
  | RedistTree.Shares shares ->
    Variable.Map.map (fun f ->
        { pinned_src =
            Variable.Map.singleton src (Factor (f, Src));
          other_src = Zero;
        })
      shares
  | RedistTree.Flats fs ->
    Variable.Map.map (formula_prov Variable.Map.empty) fs

let rec tree_prov : type a. Variable.t -> a RedistTree.tree -> prov_exprs =
  fun src tree ->
  match tree with
  | Nothing -> assert false
  | Redist r -> redist_prov src r
  | When _ -> assert false
  | Branch { evt; before; after } ->
    let befores = tree_prov src before in
    let afters = tree_prov src after in
    Variable.Map.merge (fun _dest before after ->
        let bfs, bfo, afs, afo =
          match before, after with
          | None, None -> assert false
          | Some b, None -> b.pinned_src, b.other_src, Variable.Map.empty, Zero
          | None, Some a -> Variable.Map.empty, Zero, a.pinned_src, a.other_src
          | Some b, Some a -> b.pinned_src, b.other_src, a.pinned_src, a.other_src
        in
        Some {
          pinned_src =
            Variable.Map.merge (fun _src before after ->
                match before, after with
                | None, None -> assert false
                | Some before, None ->
                  Some (Switch { evt; before; after = Zero })
                | None, Some after ->
                  Some (Switch { evt; before = Zero; after })
                | Some before, Some after ->
                  Some (Switch { evt; before; after })
              )
              bfs afs;
          other_src =
            if bfo = afo then bfo
            else Switch { evt; before = bfo; after = afo }
        })
      befores afters

let trees_prov (src : Variable.t) (t : RedistTree.t) : prov_exprs =
  match t with
  | Flat fs ->
    List.fold_left (fun acc tree ->
        let provs = tree_prov src tree in
        prov_exprs_union acc provs)
      Variable.Map.empty fs
  | Fractions { base_shares; default; branches } ->
    if default <> None then
      Errors.raise_error "(internal) Default attribution should have been computed away";
    List.fold_left (fun acc tree ->
        let provs = tree_prov src tree in
        prov_exprs_union acc provs)
      (redist_prov src base_shares) branches

let provenance_expressions (program : program) : prov_exprs =
  Variable.Map.fold (fun source ts provs ->
      let tprovs = trees_prov source ts in
      prov_exprs_union tprovs provs)
    program.trees Variable.Map.empty

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
                  Some (merge_expr e e'))
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

let event_condition (provs : prov_exprs) (event : event) : cond bdd sourced =
  let merge_eq d1 d2 =
    merge_bdd (fun e1 e2 ->
        match e1, e2 with
        | None, None
        | Some _ , None
        | None, Some _ -> None
        | Some e1, Some e2 ->
          Some (Eq (e1, e2)))
      d1 d2
  in
  match event with
  | EvtVar evt ->
    { pinned_src = Variable.Map.empty; other_src = Do (Ref evt) }
  | EvtComp (Eq, f1, f2) ->
    let se1 = formula_prov provs f1 in
    let se2 = formula_prov provs f2 in
    let pinned_src =
      Variable.Map.merge (fun _src e1 e2 ->
          let e1, e2 =
            match e1 with
            | Some e1 ->
              if e2 <> None then
                Errors.raise_error "source term present in both side of equation";
              e1, se2.other_src
            | None ->
              match e2 with
              | None -> se1.other_src, se2.other_src
              | Some e2 -> e2, se1.other_src
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

let condition_equations (program : program) (provs : prov_exprs) : event_eqs =
  Variable.Map.map (event_condition provs) program.events

let rec print_expr fmt (expr : expr) =
  match expr with
  | Src -> Format.fprintf fmt "[src]"
  | Zero -> Format.fprintf fmt "0"
  | Const l -> FormatIr.print_literal fmt l
  | Factor (f, e) -> Format.fprintf fmt "%g*%a" f print_expr e
  | Switch { evt; before; after } ->
    Format.fprintf fmt "@[<hv 2>(v%d ?@ %a@ : %a)@]"
      evt
      print_expr before
      print_expr after
  | Add (expr1, expr2) ->
    Format.fprintf fmt "@[<hv>(%a@ + %a)@]"
      print_expr expr1
      print_expr expr2
  | Pre v -> Format.fprintf fmt "v%d" v

let print_provenances fmt (provs : prov_exprs) =
  Format.pp_open_vbox fmt 0;
  Variable.Map.iter (fun dest prov ->
      Format.fprintf fmt "@[<hv 2>provs %d:@ " dest;
      Variable.Map.iter (fun src expr ->
          Format.fprintf fmt "@[<hv 2>from %d:@ %a@],@ "
            src
            print_expr expr)
        prov.pinned_src;
      Format.fprintf fmt "from another:@ %a" print_expr prov.other_src;
      Format.fprintf fmt "@]@,")
    provs;
  Format.fprintf fmt "@]@."

let rec print_eqex fmt (e : eqex) =
  match e with
  | ESrc -> Format.fprintf fmt "[src]"
  | EConst l -> FormatIr.print_literal fmt l
  | EMult (f, e) -> Format.fprintf fmt "%g*%a" f print_eqex e
  | EAdd (e1, e2) ->
    Format.fprintf fmt "@[<hv>(%a@ + %a)@]"
      print_eqex e1 print_eqex e2
  | ESub (e1, e2) ->
    Format.fprintf fmt "@[<hv>(%a@ - %a)@]"
      print_eqex e1 print_eqex e2
  | EVar v -> Format.fprintf fmt "v%d" v

let print_cond fmt (cond : cond) =
  match cond with
  | Ref evt -> Format.fprintf fmt "event %d" evt
  | Eq (e1, e2) ->
    Format.fprintf fmt "@[<hv 1>(%a@ = %a)@]"
      print_eqex e1
      print_eqex e2

let rec print_bdd (pp : Format.formatter -> 'a -> unit) fmt (bdd : 'a bdd) =
  match bdd with
  | Nothing -> Format.fprintf fmt "nothing"
  | Do e -> pp fmt e
  | Decide (c, d1, d2) ->
    Format.fprintf fmt "@[<hv>if %d@ then %a@ else %a@]"
      c (print_bdd pp) d1 (print_bdd pp) d2

let print_conditions fmt (eqs : event_eqs) =
  Format.pp_open_vbox fmt 0;
  Variable.Map.iter (fun dest eqs ->
      Format.fprintf fmt "@[<hv 2>eqs %d:@ " dest;
      Variable.Map.iter (fun src bdd ->
          Format.fprintf fmt "@[<hv 2>from %d:@ %a@],@ "
            src
            (print_bdd print_cond) bdd)
        eqs.pinned_src;
      Format.fprintf fmt "from another:@ %a" (print_bdd print_cond) eqs.other_src;
      Format.fprintf fmt "@]@,")
    eqs;
  Format.fprintf fmt "@]@."

