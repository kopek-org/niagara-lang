(*****************************************************************************)
(*                                                                           *)
(*  Copyright (c) 2023 OCamlPro SAS                                          *)
(*                                                                           *)
(* All rights reserved.                                                      *)
(* This source code is licensed under the GNU Affero General Public License  *)
(* version 3 found in the LICENSE.md file in the root directory of this      *)
(* source tree.                                                              *)
(*                                                                           *)
(*****************************************************************************)

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
  | Current of Variable.t

module BDT = Variable.BDT

type eqex =
  | EZero
  | ESrc
  | EConst of literal
  | EMult of float * eqex
  | EAdd of eqex * eqex
  | EMinus of eqex
  | EVar of Variable.t
  | ECurrVar of Variable.t

type cond =
  | Ref of Variable.t
  | Raising of Variable.t
  | Eq of eqex * eqex
  | Norm of float * eqex

type 'a sourced = {
  pinned_src : 'a Variable.Map.t;
  other_src : 'a;
}

type prov_exprs = expr sourced Variable.Map.t

type event_eqs = cond BDT.t sourced Variable.Map.t

type program_with_threshold =
  { infos : Ast.program_infos;
    trees : RedistTree.t Variable.Map.t;
    equations : event_eqs;
    eval_order : Variable.t list;
  }

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
  | Current v -> Format.fprintf fmt "v%d'" v

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
  | EZero -> Format.fprintf fmt "0"
  | ESrc -> Format.fprintf fmt "[src]"
  | EConst l -> FormatIr.print_literal fmt l
  | EMult (f, e) -> Format.fprintf fmt "%g*%a" f print_eqex e
  | EAdd (e1, EMinus e2) ->
    Format.fprintf fmt "@[<hv>(%a@ - %a)@]"
      print_eqex e1 print_eqex e2
  | EAdd (e1, e2) ->
    Format.fprintf fmt "@[<hv>(%a@ + %a)@]"
      print_eqex e1 print_eqex e2
  | EMinus e ->
    Format.fprintf fmt "@[<hv>-%a@]" print_eqex e
  | EVar v -> Format.fprintf fmt "v%d" v
  | ECurrVar v -> Format.fprintf fmt "v%d'" v

let print_cond fmt (cond : cond) =
  match cond with
  | Ref evt -> Format.fprintf fmt "event %d" evt
  | Raising evt -> Format.fprintf fmt "when %d" evt
  | Norm (f, e) ->
    Format.fprintf fmt "@[<hv 1> %g*[src]@ = %a@]"
      f print_eqex e
  | Eq (e1, e2) ->
    Format.fprintf fmt "@[<hv 1>(%a@ = %a)@]"
      print_eqex e1
      print_eqex e2

let rec print_bdd (pp : Format.formatter -> 'a -> unit) fmt (bdd : 'a BDT.t) =
  match bdd with
  | NoAction -> Format.fprintf fmt "nothing"
  | Action e -> pp fmt e
  | Decision (c, d1, d2) ->
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
  | Src | Const _ | Pre _ | Current _ -> Factor (f, e)
  | Factor (f', e) -> Factor (f*.f',e)
  | Add (e1, e2) -> Add (factor_expr e1 f, factor_expr e2 f)
  | Switch { evt; before; after } ->
    Switch { evt; before = factor_expr before f; after = factor_expr after f }

let rec apply_expr (lexpr : expr) (xexpr : expr) =
  match lexpr with
  | Zero | Const _ | Pre _ | Current _ -> lexpr
  | Src -> xexpr
  | Factor (f, le) -> factor_expr (apply_expr le xexpr) f
  | Add (le1, le2) -> Add (apply_expr le1 xexpr, apply_expr le2 xexpr)
  | Switch { evt; before; after } ->
    Switch { evt; before = apply_expr before xexpr; after = apply_expr after xexpr }

let lift_expr (expr : expr) : eqex BDT.t =
  let rec aux decided expr =
    match expr with
    | Zero -> BDT.NoAction
    | Src -> BDT.Action ESrc
    | Const l -> BDT.Action (EConst l)
    | Factor (f, e) ->
      let d = aux decided e in
      Variable.BDT.map_action decided (fun _k ->
          Option.fold ~some:(fun e -> BDT.Action (EMult (f, e))) ~none:BDT.NoAction)
        d
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
        Variable.Map.union (fun _dest e1 e2 -> Some (merge_expr e1 e2))
          se1.pinned_src se2.pinned_src;
      other_src = merge_expr se1.other_src se2.other_src;
    }
  in
  Variable.Map.union (fun _dest expr1 expr2 ->
      Some (merge_sexpr expr1 expr2))
    p1 p2

let get_source (src : Variable.t) (sourced : 'a sourced) =
  match Variable.Map.find_opt src sourced.pinned_src with
  | None -> sourced.other_src
  | Some e -> e

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
      | IAdd
      | MAdd -> Add (e1, e2)
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
              Some (Switch { evt; before = e; after = merge_expr e on_when}))
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
  | Fractions { base_shares; default; branches } ->
    let branches =
      match default with
      | NoDefault -> branches
      | DefaultTree dt -> dt::branches
      | DefaultVariable _ ->
        Errors.raise_error "(internal) Default attribution should have been computed away"
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

let event_condition (provs : prov_exprs) (event : event) : cond BDT.t sourced =
  let merge_eq d1 d2 =
    Variable.BDT.merge (fun _k e1 e2 ->
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
    { pinned_src = Variable.Map.empty; other_src = Action (Ref evt) }
  | EvtOnRaise evt ->
    { pinned_src = Variable.Map.empty; other_src = Action (Raising evt)}
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

let transform_raising_cond (eqs : event_eqs) =
  let reduce_decide_on evt bdd =
    if bdd = BDT.NoAction then bdd else
      let bdd = BDT.add_decision evt bdd in
      BDT.map_action (Variable.Map.singleton evt true)
        (fun _k _c -> NoAction)
        bdd
  in
  let trans_raising cond =
    match cond with
    | Eq _ | Norm _ ->
      { pinned_src = Variable.Map.empty;
        other_src = BDT.Action cond;
      }
    | Ref evt -> Variable.Map.find evt eqs
    | Raising evt ->
      let srcd = Variable.Map.find evt eqs in
      { pinned_src =
          Variable.Map.map (reduce_decide_on evt) srcd.pinned_src;
        other_src = reduce_decide_on evt srcd.other_src;
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
            | Some _, Some _ -> assert false)
          d1 d2
      in
      let default_src =
        BDT.fold ~noaction:(fun _k -> any_source BDT.NoAction)
          ~action:(fun k cond ->
              let srcd = trans_raising cond in
              { pinned_src = Variable.Map.map (BDT.cut k) srcd.pinned_src;
                other_src = BDT.cut k srcd.other_src;
              })
          ~decision:(fun _k _evt s1 s2 ->
              { pinned_src =
                  Variable.Map.union (fun _src c1 c2 ->
                      Some (merge_bdd c1 c2))
                    s1.pinned_src s2.pinned_src;
                other_src =
                  merge_bdd s1.other_src s2.other_src
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

let rec extract_src_factor (e : eqex) =
  match e with
  | ESrc -> 1., EZero
  | EZero | EConst _ | EVar _ | ECurrVar _ -> 0., e
  | EMult (f, e) ->
    let srcf, e = extract_src_factor e in
    srcf *. f, if e = EZero then e else EMult (f, e)
  | EAdd (e1, e2) ->
    let srcf1, e1 = extract_src_factor e1 in
    let srcf2, e2 = extract_src_factor e2 in
    srcf1 +. srcf2,
    if e1 = EZero then e2
    else if e2 = EZero then e1
    else EAdd (e1, e2)
  | EMinus e ->
    let srcf, e = extract_src_factor e in
    -1. *. srcf,
    if e = EZero then e else EMinus e

let normalize_condition (e : cond) =
  match e with
  | Ref _ | Raising _ | Norm _ -> e
  | Eq (e1, e2) ->
    let src_factor1, const_part1 = extract_src_factor e1 in
    let src_factor2, const_part2 = extract_src_factor e2 in
    let factor = src_factor1 -. src_factor2 in
    let const_part =
      match const_part1, const_part2 with
      | EZero, EZero -> EZero
      | EZero, e -> e
      | e, EZero -> EMinus e
      | e1, e2 ->
        EAdd (e2, EMinus e1)
    in
    Norm (factor, const_part)

let normalize_equations (eqs : event_eqs) : event_eqs =
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
  let outfmt = Format.formatter_of_out_channel stdout in
  let provs = provenance_expressions prog in
  let tprovs = prov_transitivity provs in
  Format.fprintf outfmt "@[<hv 2>Transitive provenances:@ %a@]@,"
    print_provenances tprovs;
  let eqs = condition_equations prog tprovs in
  Format.fprintf outfmt "@[<hv 2>Equations:@ %a@]@,"
    print_conditions eqs;
  let eqs = normalize_equations eqs in
  Format.fprintf outfmt "@[<hv 2>Normalized equations:@ %a@]@,"
    print_conditions eqs;
  {
    infos = prog.infos;
    trees = prog.trees;
    equations = eqs;
    eval_order = prog.eval_order;
  }
