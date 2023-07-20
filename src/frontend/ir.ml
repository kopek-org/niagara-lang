
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

let rec reduce_formula _acc (f : formula) =
  match f with
  | Literal _
  | Variable _ -> f
  | RCast f ->
    begin match reduce_formula _acc f with
      | Literal (LInteger i) -> Literal (LRational (float_of_int i))
      | Literal _ -> assert false
      | _ -> f
    end
  | Binop (IAdd, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LInteger i1), Literal (LInteger i2) -> Literal (LInteger (i1 + i2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (RAdd, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LRational f1), Literal (LRational f2) -> Literal (LRational (f1 +. f2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (MAdd, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LMoney m1), Literal (LMoney m2) -> Literal (LMoney (m1 + m2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DAdd, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LDate d), Literal (LDuration dr) ->
        Literal (LDate (CalendarLib.Date.add d dr))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DrAdd, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LDuration dr1), Literal (LDuration dr2) ->
        Literal (LDuration (CalendarLib.Date.Period.add dr1 dr2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (ISub, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LInteger i1), Literal (LInteger i2) -> Literal (LInteger (i1 - i2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (RSub, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LRational f1), Literal (LRational f2) -> Literal (LRational (f1 -. f2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (MSub, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LMoney m1), Literal (LMoney m2) -> Literal (LMoney (m1 - m2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DSub, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LDate d), Literal (LDuration dr) ->
        Literal (LDate (CalendarLib.Date.rem d dr))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DrSub, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LDuration dr1), Literal (LDuration dr2) ->
        Literal (LDuration (CalendarLib.Date.Period.sub dr1 dr2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (IMult, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LInteger i1), Literal (LInteger i2) -> Literal (LInteger (i1 * i2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (RMult, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LRational f1), Literal (LRational f2) -> Literal (LRational (f1 *. f2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (MMult, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LMoney m), Literal (LRational f) ->
        Literal (LMoney (int_of_float ((float_of_int m) *. f)))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DrMult, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LDuration dr), Literal (LRational f) ->
        let open CalendarLib in
        let ddr = Date.Period.nb_days dr in
        Literal (LDuration (Date.Period.day (int_of_float (float_of_int ddr *. f +. 0.5))))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (IDiv, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LInteger i1), Literal (LInteger i2) -> Literal (LInteger (i1 / i2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (RDiv, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LRational f1), Literal (LRational f2) -> Literal (LRational (f1 /. f2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (MDiv, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LMoney m), Literal (LRational f) ->
        Literal (LMoney (int_of_float ((float_of_int m) /. f)))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DrDiv, f1, f2) ->
    begin match reduce_formula _acc f1, reduce_formula _acc f2 with
      | Literal (LDuration dr), Literal (LRational f) ->
        let open CalendarLib in
        let ddr = Date.Period.nb_days dr in
        Literal (LDuration (Date.Period.day (int_of_float (float_of_int ddr /. f +. 0.5))))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop ((IEq|REq|MEq|DEq|DrEq), _, _) -> f

module RedistTree = struct

  type redist =
    | NoInfo
    | Shares of float Variable.Map.t
    | Flats of formula Variable.Map.t

  type tree =
    | Redist of redist
    | Branch of Variable.t * tree * tree

  let share (dest : Variable.t) (formula, _ft : formula * ValueType.t) =
    match formula with
    | Literal (LRational f) ->
      Shares (Variable.Map.singleton dest f)
    | _ -> Errors.raise_error "Expected formula to be a rational literal"

  let flat (dest : Variable.t) (formula, ftype : formula * ValueType.t) =
    if ftype <> TMoney then
      Errors.raise_error "Expected formula of type money";
    Flats (Variable.Map.singleton dest formula)

  let redist (r : redist) = Redist r

  let until (evt : Variable.t) (t : tree) =
    Branch(evt, t, Redist NoInfo)

  let from (evt : Variable.t) (t : tree) =
    Branch(evt, Redist NoInfo, t)

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

  let rec ordered_merge (t1 : tree) (t2 : tree) =
    match t1, t2 with
    | Redist r1, Redist r2 -> Redist (merge_redist r1 r2)
    | Redist _, Branch(evt2, b2b, b2a) ->
      Branch(evt2, ordered_merge t1 b2b, b2a)
    | Branch(evt1, b1b, b1a), Redist _ ->
      Branch(evt1, b1b, ordered_merge b1a t2)
    | Branch (evt1, b1b, b1a), Branch(evt2, b2b, b2a) ->
      if evt1 = evt2 then
        Branch (evt1, ordered_merge b1b b2b, ordered_merge b1a b2a)
      else
        Branch (evt2, ordered_merge t1 b2b, b2a)

  let rec unordered_merge (t1 : tree) (t2 : tree) =
    match t1, t2 with
    | Redist r1, Redist r2 -> Redist (merge_redist r1 r2)
    | (Redist _ as t), Branch(evt, bb, ba)
    | Branch(evt, bb, ba), (Redist _ as t) ->
      Branch(evt, unordered_merge t bb, unordered_merge t ba)
    | Branch (evt1, b1b, b1a), Branch(evt2, b2b, b2a) ->
      if evt1 = evt2 then
        Branch (evt1, unordered_merge b1b b2b, unordered_merge b1a b2a)
      else
        Branch (evt1,
                Branch(evt2, unordered_merge b1b b2b, unordered_merge b1b b2a),
                Branch(evt2, unordered_merge b1a b2b, unordered_merge b1a b2a))

end

type program = {
  infos : Ast.program_infos;
  ctx_derivations : Variable.t Context.GroupMap.t Variable.Map.t;
  trees : RedistTree.tree Variable.Map.t;
  events : event Variable.Map.t;
}
