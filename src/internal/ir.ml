open Surface

type money = int

type literal =
  | LInteger of int
  | LRational of float
  | LMoney of money
  | LDate of Date.Date.t
  | LDuration of Date.Duration.t

type binop =
  | IAdd
  | RAdd
  | MAdd
  | DAdd
  | DrAdd
  | ISub
  | RSub
  | MSub
  | DSub
  | DrSub
  | IMult
  | RMult
  | MMult
  | DrMult
  | IDiv
  | RDiv
  | MDiv
  | DrDiv

type flow_view =
  | AtInstant
  | Cumulated

type formula =
  | Literal of literal
  | Variable of Variable.t * flow_view
  | Binop of binop * formula * formula
  | RCast of formula

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

  type 'a redist =
    | NoInfo
    | Shares : float Variable.Map.t -> frac redist
    | Flats : formula Variable.Map.t -> flat redist

  type 'a tree =
    | Nothing
    | Redist of 'a redist
    | When : (Variable.t * flat tree) list -> flat tree
    | Branch of { evt : Variable.t; before : 'a tree; after : 'a tree }

  type frac_default =
    | NoDefault
    | DefaultVariable of Variable.t
    | DefaultTree of frac tree

  type t =
    | Flat of flat tree list
    | Fractions of {
        base_shares : frac redist;
        default : frac_default;
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
    | Literal (LRational f) ->
      FracRedist (Shares (Variable.Map.singleton dest f))
    | _ -> Errors.raise_error "Expected formula to be a rational literal"

  let flat (dest : Variable.t) (formula, ftype : formula * ValueType.t) =
    if ftype <> TMoney then
      Errors.raise_error "Expected formula of type money";
    FlatRedist (Flats (Variable.Map.singleton dest formula))

  let tredist (r : kind_redist) =
    match r with
    | FlatRedist r -> FlatTree (Redist r)
    | FracRedist r -> FracTree (Redist r)

  let twhen (ts : (Variable.t * kind_tree) list) =
    FlatTree (When
      (List.map (fun (dest, tree) ->
           match tree with
           | NothingTree -> dest, Nothing
           | FlatTree t -> dest, t
           | FracTree _ -> Errors.raise_error "Quotepart should not be when-guarded")
          ts))

  let tbranch (evt : Variable.t) (before : kind_tree) (after : kind_tree) =
    let mixing_error () =
      Errors.raise_error "Mixing quotepart and bonuses between branches"
    in
    match before with
    | NothingTree -> begin
        match after with
        | FlatTree after -> FlatTree (Branch { evt; before = Nothing; after })
        | FracTree after -> FracTree (Branch { evt; before = Nothing; after })
        | NothingTree -> NothingTree
      end
    | FlatTree before -> begin
        match after with
        | FlatTree after -> FlatTree (Branch { evt; before; after })
        | NothingTree -> FlatTree (Branch { evt; before; after = Nothing })
        | FracTree _ -> mixing_error ()
      end
    | FracTree before ->
      match after with
      | FracTree after -> FracTree (Branch { evt; before; after })
      | NothingTree -> FracTree (Branch { evt; before; after = Nothing })
      | FlatTree _ -> mixing_error ()

  let merge_redist0 (type a) (r1 : a redist) (r2 : a redist) : a redist =
    match r1, r2 with
    | NoInfo, NoInfo -> NoInfo
    | NoInfo, r | r, NoInfo -> r
    | Shares s1, Shares s2 ->
      let s =
        Variable.Map.union (fun _dest s1 s2 ->
            (* TODO warning *)
            Some (s1 +. s2))
          s1 s2
      in
      Shares s
    | Flats f1, Flats f2 ->
      let new_f =
        Variable.Map.union (fun _dest f1 f2 ->
          Some (Binop (MAdd, f1, f2)))
          f1 f2
      in
      Flats new_f

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
          default = match f.default with
            | NoDefault -> DefaultVariable d
            | DefaultVariable _ -> Errors.raise_error "Multiple default definition"
            | DefaultTree _ ->
              Errors.raise_error "(internal) Default variable assigned on computed tree"
        }

  let add_tree (tree : kind_tree) (t : t) =
    let mixing_error () =
      Errors.raise_error "Mixing quotepart and bonuses between operations"
    in
    match tree with
    | NothingTree -> t
    | FlatTree tree -> begin
        if tree = Nothing then t else
          match t with
          | Fractions _ -> mixing_error ()
          | Flat fs -> Flat (tree::fs)
    end
    | FracTree tree ->
      match t with
      | Flat _ -> mixing_error ()
      | Fractions f ->
        match tree with
        | Nothing -> assert false
        | Redist r -> Fractions { f with base_shares = merge_redist0 r f.base_shares }
        | _ -> Fractions { f with branches = tree::f.branches }

  let of_tree (tree : kind_tree) =
    match tree with
    | NothingTree | FracTree Nothing | FlatTree Nothing -> assert false
    | FlatTree tree -> Flat [tree]
    | FracTree (Redist r) ->
      Fractions { base_shares = r; default = NoDefault; branches = [] }
    | FracTree (Branch _ as tree) ->
      Fractions { base_shares = NoInfo; default = NoDefault; branches = [tree] }

  let of_remainder (d : Variable.t) =
    Fractions { base_shares = NoInfo; default = DefaultVariable d; branches = [] }

  let merge_default (d1 : frac_default) (d2 : frac_default) =
    match d1, d2 with
    | NoDefault, NoDefault -> NoDefault
    | DefaultVariable d, NoDefault
    | NoDefault, DefaultVariable d -> DefaultVariable d
    | _, _ ->
      Errors.raise_error "Multiple default definition"

  let merge (t1 : t) (t2 : t) =
    match t1, t2 with
    | Flat _, Fractions _ | Fractions _, Flat _ ->
      Errors.raise_error "Mixing quotepart and bonuses between operations"
    | Flat fs, Flat fs' -> Flat (fs@fs')
    | Fractions f1, Fractions f2 ->
      Fractions {
        base_shares = merge_redist0 f1.base_shares f2.base_shares;
        default = merge_default f1.default f2.default;
        branches = f1.branches @ f2.branches;
      }

end

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
  | CRef of Variable.t
  | CRaising of Variable.t
  | CEq of eqex * eqex
  | CNorm of float * eqex

type 'a sourced = {
  pinned_src : 'a Variable.Map.t;
  other_src : 'a;
}

type event_eqs = cond Variable.BDT.t sourced Variable.Map.t

type program = {
  infos : Ast.program_infos;
  trees : RedistTree.t Variable.Map.t;
  events : event Variable.Map.t;
  eval_order : Variable.t list;
  equations : event_eqs;
}

let get_source (src : Variable.t) (sourced : 'a sourced) =
  match Variable.Map.find_opt src sourced.pinned_src with
  | None -> sourced.other_src
  | Some e -> e
