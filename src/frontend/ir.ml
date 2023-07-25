
type money = int

type literal =
  | LInteger of int
  | LRational of float
  | LMoney of money
  | LDate of CalendarLib.Date.t
  | LDuration of CalendarLib.Date.Period.t

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
  | IEq
  | REq
  | MEq
  | DEq
  | DrEq

type flow_view =
  | AtInstant
  | Cumulated

type formula =
  | Literal of literal
  | Variable of Variable.t * flow_view
  | Binop of binop * formula * formula
  | RCast of formula

type event =
  | EvtVar of Variable.t
  | EvtAnd of event * event
  | EvtOr of event * event
  | EvtCond of formula
  | EvtDate of formula

module RedistTree = struct

  type redist =
    | NoInfo
    | Shares of float Variable.Map.t
    | Flats of formula Variable.Map.t

  type tree =
    | Redist of redist
    | Branch of { evt : Variable.t; before : tree; after : tree }

  type t = tree list

  let share (dest : Variable.t) (formula, _ft : formula * ValueType.t) =
    match formula with
    | Literal (LRational f) ->
      Shares (Variable.Map.singleton dest f)
    | _ -> Errors.raise_error "Expected formula to be a rational literal"

  let flat (dest : Variable.t) (formula, ftype : formula * ValueType.t) =
    if ftype <> TMoney then
      Errors.raise_error "Expected formula of type money";
    Flats (Variable.Map.singleton dest formula)

  let merge_redist (r1 : redist) (r2 : redist) =
    match r1, r2 with
    | NoInfo, NoInfo -> NoInfo
    | NoInfo, r | r, NoInfo -> r
    | Shares s1, Shares s2 ->
      let new_s =
        Variable.Map.union (fun _dest s1 s2 ->
            (* TODO warning *)
            Some (s1 +. s2))
          s1 s2
      in
      Shares new_s
    | Flats f1, Flats f2 ->
      let new_f =
        Variable.Map.union (fun _dest f1 f2 ->
          Some (Binop (MAdd, f1, f2)))
          f1 f2
      in
      Flats new_f
    | Flats _, Shares _ | Shares _, Flats _ ->
      Errors.raise_error "Mixing quoteparts and bonuses"

end

type program = {
  infos : Ast.program_infos;
  ctx_derivations : Variable.t Context.GroupMap.t Variable.Map.t;
  trees : RedistTree.t Variable.Map.t;
  events : event Variable.Map.t;
}
