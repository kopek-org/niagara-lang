open Surface

type binop = Ast.binop

type flow_view =
  | AtInstant
  | Cumulated

type formula =
  | Literal of Literal.t
  | Variable of Variable.t * flow_view
  | Binop of binop * formula * formula

type comp = Eq

type event =
  | EvtVar of Variable.t
  | EvtOnRaise of Variable.t
  | EvtAnd of event * event
  | EvtOr of event * event
  | EvtComp of comp * formula * formula
  | EvtDate of formula

module RedistTree = struct

  type flat = private FLAT
  type frac = private FRAC

  type part_or_remain =
    | Part of R.t
    | Remain

  type 'a redist =
    | NoInfo
    | Shares : part_or_remain Variable.Map.t -> frac redist
    | Flats : { transfers : formula Variable.Map.t;
                balances : R.t Variable.Map.t }
        -> flat redist

  type 'a tree = 'a redist Variable.BDT.t

  type frac_balance =
    | BalanceVars of { default : Variable.t option; deficit : Variable.t option }
    | BalanceTree of frac tree

  type t =
    | Flat of flat tree list
    | Fractions of {
        base_shares : frac redist;
        balance : frac_balance;
        branches : frac tree list;
      }

  type kind_tree =
    | NothingTree
    | FlatTree of flat tree
    | FracTree of frac tree

  type kind_redist =
    | FlatRedist of flat redist
    | FracRedist of frac redist

  let share (dest : Variable.t) (formula, _ft : formula * ValueType.t) =
    match formula with
    | Literal (LRational r) ->
      FracRedist (Shares (Variable.Map.singleton dest (Part r)))
    | _ -> Errors.raise_error "Expected formula to be a rational literal"

  let remain (dest : Variable.t) =
    FracRedist (Shares (Variable.Map.singleton dest Remain))

  let flat (dest : Variable.t) (formula, ftype : formula * ValueType.t) =
    if ftype <> TMoney then
      Errors.raise_error "Expected formula of type money";
    FlatRedist (Flats { transfers = Variable.Map.singleton dest formula;
                        balances = Variable.Map.empty })

  let tredist (r : kind_redist) =
    match r with
    | FlatRedist r -> FlatTree (Action r)
    | FracRedist r -> FracTree (Action r)

  let merge_redist0 (type a) (r1 : a redist) (r2 : a redist) : a redist =
    match r1, r2 with
    | NoInfo, r | r, NoInfo -> r
    | Shares s1, Shares s2 ->
      let s =
        Variable.Map.union (fun _dest s1 s2 ->
            (* TODO warning *)
            match s1, s2 with
            | Remain, _ | _, Remain -> Some Remain
            | Part s1, Part s2 -> Some (Part R.(s1 + s2)))
          s1 s2
      in
      Shares s
    | Flats f1, Flats f2 ->
      let transfers =
        Variable.Map.union (fun _dest f1 f2 ->
          Some (Binop (Add, f1, f2)))
          f1.transfers f2.transfers
      in
      let balances =
        Variable.Map.union (fun _dest f1 f2 ->
          Some R.(f1 + f2))
          f1.balances f2.balances
      in
      Flats { transfers; balances }

  let twhen (ts : (Variable.t * kind_tree) list) =
    let tree =
      List.fold_left (fun tree (evt, kt) ->
          match kt with
          | NothingTree -> tree
          | FracTree _ -> Errors.raise_error "Quotepart should not be when-guarded"
          | FlatTree t ->
            let t = Variable.BDT.only_when (Variable.Map.singleton evt true) t in
            Variable.BDT.merge (fun _k tree t ->
                match tree, t with
                | None, None -> None
                | Some _, None -> tree
                | None, Some _ -> t
                | Some r1, Some r2 -> Some (merge_redist0 r1 r2)
              )
              tree t
        )
        Variable.BDT.empty ts
    in
    FlatTree tree

  let tbranch (evt : Variable.t) (before : kind_tree) (after : kind_tree) =
    let mixing_error () =
      Errors.raise_error "Mixing quotepart and bonuses between branches"
    in
    let build_branch before after =
      Variable.BDT.Decision (evt,
         Variable.BDT.cut (Variable.Map.singleton evt true) after,
         Variable.BDT.cut (Variable.Map.singleton evt false) before)
    in
    match before with
    | NothingTree -> begin
        match after with
        | FlatTree after ->
          FlatTree (build_branch NoAction after)
        | FracTree after ->
          FracTree (build_branch NoAction after)
        | NothingTree -> NothingTree
      end
    | FlatTree before -> begin
        match after with
        | FlatTree after -> FlatTree (build_branch before after)
        | NothingTree -> FlatTree (build_branch before NoAction)
        | FracTree _ -> mixing_error ()
      end
    | FracTree before ->
      match after with
      | FracTree after -> FracTree (build_branch before after)
      | NothingTree -> FracTree (build_branch before NoAction)
      | FlatTree _ -> mixing_error ()

  let merge_redist (r1 : kind_redist) (r2 : kind_redist) =
    match r1, r2 with
    | FlatRedist _, FracRedist _ | FracRedist _, FlatRedist _ ->
      Errors.raise_error "Mixing quotepart and bonuses in operation"
    | FlatRedist f1, FlatRedist f2 -> FlatRedist (merge_redist0 f1 f2)
    | FracRedist f1, FracRedist f2 -> FracRedist (merge_redist0 f1 f2)


  let add_remainder (d : Variable.t) (t : t) =
    match t with
    | Flat _ -> Errors.raise_error "Cannot add default to source of bonuses"
    | Fractions f ->
      Fractions
        { f with
          balance = match f.balance with
            | BalanceVars b ->
              if Option.is_some b.default then
                Errors.raise_error "Multiple default definition"
              else BalanceVars { b with default = Some d }
            | BalanceTree _ ->
              Errors.raise_error "(internal) Default variable assigned on computed tree"
        }

  let add_deficit (d : Variable.t) (t : t) =
    match t with
    | Flat _ -> Errors.raise_error "Cannot add deficit to source of bonuses"
    | Fractions f ->
      Fractions
        { f with
          balance = match f.balance with
            | BalanceVars b ->
              if Option.is_some b.deficit then
                Errors.raise_error "Multiple default definition"
              else BalanceVars { b with deficit = Some d }
            | BalanceTree _ ->
              Errors.raise_error "(internal) Deficit variable assigned on computed tree"
        }

  let add_tree (tree : kind_tree) (t : t) =
    let mixing_error () =
      Errors.raise_error "Mixing quotepart and bonuses between operations"
    in
    match tree with
    | NothingTree -> t
    | FlatTree tree -> begin
        match t with
          | Fractions _ -> mixing_error ()
          | Flat fs -> Flat (tree::fs)
      end
    | FracTree tree ->
      match t with
      | Flat _ -> mixing_error ()
      | Fractions f ->
        match tree with
        | NoAction -> assert false
        | Action r -> Fractions { f with base_shares = merge_redist0 r f.base_shares }
        | Decision _ -> Fractions { f with branches = tree::f.branches }

  let of_tree (tree : kind_tree) =
    match tree with
    | NothingTree | FracTree NoAction | FlatTree NoAction -> assert false
    | FlatTree tree -> Flat [tree]
    | FracTree (Action r) ->
      Fractions { base_shares = r;
                  balance = BalanceVars { default = None; deficit = None };
                  branches = [] }
    | FracTree (Decision _ as tree) ->
      Fractions { base_shares = NoInfo;
                  balance = BalanceVars { default = None; deficit = None };
                  branches = [tree] }

  let of_remainder (d : Variable.t) =
    Fractions { base_shares = NoInfo;
                balance = BalanceVars { default = Some d; deficit = None };
                branches = [] }

  let of_deficit (d : Variable.t) =
    Fractions { base_shares = NoInfo;
                balance = BalanceVars { default = None; deficit = Some d };
                branches = [] }

end

type eqex =
  | EZero
  | EConst of Literal.t
  | EMult of eqex * eqex
  | EAdd of eqex * eqex
  | EMinus of eqex
  | EVar of Variable.t
  | ECurrVar of Variable.t

type nf_eq = { (* ax+b *)
  src_factor : eqex;
  const : eqex;
}

type 'a sourced = {
  pinned_src : 'a Variable.Map.t;
  other_src : 'a;
}

type event_eq = nf_eq Variable.BDT.t sourced

type program = {
  infos : Ast.program_infos;
  trees : RedistTree.t Variable.Map.t;
  events : event Variable.Map.t;
  dep_graph : Variable.Graph.t;
  eval_order : Variable.t list;
  equations : event_eq Variable.Map.t;
}

let get_source (src : Variable.t) (sourced : 'a sourced) =
  match Variable.Map.find_opt src sourced.pinned_src with
  | None -> sourced.other_src
  | Some e -> e

let map_source (f : 'a -> 'b) (s : 'a sourced) =
  { pinned_src = Variable.Map.map f s.pinned_src;
    other_src = f s.other_src }

let merge_sources (f : 'a -> 'b -> 'c) (s1 : 'a sourced) (s2 : 'b sourced) =
  let pinned_src =
    Variable.Map.merge (fun _v e1 e2 ->
        match e1, e2 with
        | None, None -> None
        | Some e1, None -> Some (f e1 s2.other_src)
        | None, Some e2 -> Some (f s1.other_src e2)
        | Some e1, Some e2 -> Some (f e1 e2))
      s1.pinned_src s2.pinned_src
  in
  let other_src = f s1.other_src s2.other_src in
  { pinned_src; other_src }
