
module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

(**************)

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

(*************)

module Acc = struct

  type t = {
    infos : Ast.program_infos;
    trees : RedistTree.tree Variable.Map.t;
    events : event Variable.Map.t;
  }

  let make (infos : Ast.program_infos) =
    { infos;
      trees = Variable.Map.empty;
      events = Variable.Map.empty; }

  (* let contexts t = t.infos.contexts *)

  let type_of t v =
    match Variable.Map.find_opt v t.infos.types with
    | Some typ -> typ
    | None -> Errors.raise_error "(internal) Cannot find type of variable"

  let anon_var_name =
    let c = ref 0 in
    fun name ->
      let i = !c in incr c;
      "anon_" ^ name ^ "_" ^ string_of_int i

  let register_event t (v : Variable.t) (event : event) =
    { t with
      events = Variable.Map.add v event t.events
    }

  let lift_event t (event : event) =
    let var_name = anon_var_name "event" in
    let v = Variable.new_var () in
    let t =
      { t with
        infos =
          { t.infos with
            var_info = Variable.Map.add v { Variable.var_name } t.infos.var_info;
            types = Variable.Map.add v ValueType.TEvent t.infos.types;
          };
      }
    in
    let t = register_event t v event in
    t, v

  let add_redist t ~(source : Variable.t) (tree : RedistTree.tree) =
    let trees =
      Variable.Map.update source (function
      | None -> Some tree
      | Some existing_tree ->
        Some (RedistTree.unordered_merge existing_tree tree))
        t.trees
    in
    { t with trees }

end

(* module Env = struct *)

(*   type t = { *)
(*     view : flow_view; *)
(*     context : Context.t; *)
(*     source_layout : var_layout; *)
(*   } *)

(*   let empty = { *)
(*     view = AtInstant; *)
(*     context = Context.everything; *)
(*     source_layout = SimpleVar; *)
(*   } *)

(*   let with_view view t = { t with view } *)

(*   let with_context context t = { t with context } *)

(*   let with_source_layout source_layout t = { t with source_layout } *)

(*   let view t = t.view *)

(*   let context t = t.context *)

(*   let source_layout t = t.source_layout *)

(* end *)

let translate_literal (l : Ast.literal) =
  match l with
  | LitInt i -> LInteger i, ValueType.TInteger
  | LitRational r -> LRational r, ValueType.TRational
  | LitMoney c -> LMoney c, ValueType.TMoney
  | LitDate d -> LDate d, ValueType.TDate
  | LitDuration d -> LDuration d, ValueType.TDuration

let translate_binop (op : Ast.binop)
    (f1, t1 : formula * ValueType.t)
    (f2, t2 : formula * ValueType.t) =
  match op, t1, t2 with
  | Add, TInteger, TInteger -> Binop (IAdd, f1, f2), ValueType.TInteger
  | Add, TInteger, TRational -> Binop (RAdd, RCast f1, f2), ValueType.TRational
  | Add, TRational, TInteger -> Binop (RAdd, f1, RCast f2), ValueType.TRational
  | Add, TRational, TRational -> Binop (RAdd, f1, f2), ValueType.TRational
  | Add, TMoney, TMoney -> Binop (MAdd, f1, f2), ValueType.TMoney
  | Add, TDate, TDuration -> Binop (DAdd, f1, f2), ValueType.TDate
  | Add, TDuration, TDate -> Binop (DAdd, f2, f1), ValueType.TDate
  | Add, TDuration, TDuration -> Binop (DrAdd, f1, f2), ValueType.TDuration
  | Sub, TInteger, TInteger -> Binop (ISub, f1, f2), ValueType.TInteger
  | Sub, TInteger, TRational -> Binop (RSub, RCast f1, f2), ValueType.TRational
  | Sub, TRational, TInteger -> Binop (RSub, f1, RCast f2), ValueType.TRational
  | Sub, TRational, TRational -> Binop (RSub, f1, f2), ValueType.TRational
  | Sub, TMoney, TMoney -> Binop (MSub, f1, f2), ValueType.TMoney
  | Sub, TDate, TDuration -> Binop (DSub, f1, f2), ValueType.TDate
  | Sub, TDuration, TDuration -> Binop (DrSub, f1, f2), ValueType.TDuration
  | Mult, TInteger, TInteger -> Binop (IMult, f1, f2), ValueType.TInteger
  | Mult, TInteger, TRational -> Binop (RMult, RCast f1, f2), ValueType.TRational
  | Mult, TRational, TInteger -> Binop (RMult, f1, RCast f2), ValueType.TRational
  | Mult, TRational, TRational -> Binop(RMult, f1, f2), ValueType.TRational
  | Mult, TMoney, TInteger -> Binop (MMult, f1, RCast f2), ValueType.TMoney
  | Mult, TMoney, TRational -> Binop (MMult, f1, f2), ValueType.TMoney
  | Mult, TInteger, TMoney -> Binop (MMult, f2, RCast f1), ValueType.TMoney
  | Mult, TRational, TMoney -> Binop (MMult, f2, f1), ValueType.TMoney
  | Mult, TDuration, TInteger -> Binop (DrMult, f1, RCast f2), ValueType.TDuration
  | Mult, TDuration, TRational -> Binop (DrMult, f1, f2), ValueType.TDuration
  | Mult, TInteger, TDuration -> Binop (DrMult, f2, RCast f1), ValueType.TDuration
  | Mult, TRational, TDuration -> Binop (DrMult, f2, f1), ValueType.TDuration
  | Div, TInteger, TInteger -> Binop (IDiv, f1, f2), ValueType.TInteger
  | Div, TInteger, TRational -> Binop (RDiv, RCast f1, f2), ValueType.TRational
  | Div, TRational, TInteger -> Binop (RDiv, f1, RCast f2), ValueType.TRational
  | Div, TRational, TRational -> Binop (RDiv, f1, f2), ValueType.TRational
  | Div, TMoney, TInteger -> Binop (MDiv, f1, RCast f2), ValueType.TMoney
  | Div, TMoney, TRational -> Binop (MDiv, f1, f2), ValueType.TMoney
  | Div, TDuration, TInteger -> Binop (DrDiv, f1, RCast f2), ValueType.TDuration
  | Div, TDuration, TRational -> Binop (DrDiv, f1, f2), ValueType.TDuration
  | _ -> Errors.raise_error "Mismatching types for binop"

let translate_comp (comp : Ast.comp)
    (f1, t1 : formula * ValueType.t)
    (f2, t2 : formula * ValueType.t) =
  match comp, t1, t2 with
  | Eq, TInteger, TInteger -> Binop (IEq, f1, f2), ValueType.TEvent
  | Eq, TInteger, TRational -> Binop (REq, RCast f1, f2), ValueType.TEvent
  | Eq, TRational, TInteger -> Binop (REq, f1, RCast f2), ValueType.TEvent
  | Eq, TRational, TRational -> Binop (REq, f1, f2), ValueType.TEvent
  | Eq, TMoney, TMoney -> Binop (MEq, f1, f2), ValueType.TEvent
  | Eq, TDate, TDate -> Binop (DEq, f1, f2), ValueType.TEvent
  | Eq, TDuration, TDuration -> Binop (DrEq, f1, f2), ValueType.TEvent
  | _ -> Errors.raise_error "Mismatching types for comp"

let rec translate_formula acc ~(view : flow_view) (f : Ast.contextualized Ast.formula) =
  match f with
  | Literal l ->
    let f, t = translate_literal l in
    Literal f, t
  | Variable v ->
    let v = fst v in (* TODO *)
    let t = Acc.type_of acc v in
    Variable (v, view), t
  | Binop (op, f1, f2) ->
    let f1 = translate_formula acc ~view f1 in
    let f2 = translate_formula acc ~view f2 in
    translate_binop op f1 f2
  | Comp (comp, f1, f2) ->
    let f1 = translate_formula acc ~view f1 in
    let f2 = translate_formula acc ~view f2 in
    translate_comp comp f1 f2
  | Instant f -> translate_formula acc ~view:AtInstant f
  | Total f -> translate_formula acc ~view:Cumulated f

let translate_redist acc ~(dest : Ast.contextualized_variable)
    (redist : Ast.contextualized Ast.redistribution) =
  let dest = fst dest in (* TODO *)
  match redist with
  | Part f ->
    let f, ft = translate_formula acc ~view:AtInstant f in
    RedistTree.share dest (reduce_formula acc f, ft)
  | Flat f ->
    let f = translate_formula acc ~view:AtInstant f in
    RedistTree.flat dest f

let rec translate_event acc (event : Ast.contextualized Ast.event_expr) =
  match event with
  | EventVar v -> EvtVar v
  | EventConj (e1, e2) ->
    let e1 = translate_event acc e1 in
    let e2 = translate_event acc e2 in
    EvtAnd(e1,e2)
  | EventDisj (e1, e2) ->
    let e1 = translate_event acc e1 in
    let e2 = translate_event acc e2 in
    EvtOr(e1,e2)
  | EventFormula f ->
    let f, t = translate_formula acc ~view:Cumulated f in
    match (t : ValueType.t) with
    | TEvent -> EvtCond f
    | TDate -> EvtDate f
    | TInteger | TRational
    | TMoney | TDuration -> Errors.raise_error "Formula is not an event"

let lift_event acc (event : Ast.contextualized Ast.event_expr) =
  let evt = translate_event acc event in
  match evt with
  | EvtVar v -> acc, v
  | _ ->
    let acc, v = Acc.lift_event acc evt in
    acc, v

let rec translate_guarded_redist env acc
    ~(default_dest : Ast.contextualized_variable option)
    (redist : Ast.contextualized Ast.guarded_redistrib) =
  match redist with
  | Redist (WithVar (redist, dest)) ->
    let dest =
      match default_dest, dest with
      | Some _, Some dest (* TODO warning *)
      | None, Some dest -> dest
      | Some default, None -> default
      | None, None -> Errors.raise_error "No destination for repartition"
    in
    acc, RedistTree.redist (translate_redist acc ~dest redist)
  | Seq grs ->
    let acc, trees =
      List.fold_left_map (translate_guarded_redist env ~default_dest) acc grs
    in
    begin match trees with
      | [] -> assert false
      | t::ts -> acc, List.fold_left RedistTree.ordered_merge t ts
    end
  | Guarded (guard, redist) ->
    let acc, tree = translate_guarded_redist env acc ~default_dest redist in
    match guard with
    | Before event ->
      let acc, evt = lift_event acc event in
      acc, RedistTree.until evt tree
    | After event ->
      let acc, evt = lift_event acc event in
      acc, RedistTree.from evt tree
    | When _ -> assert false

let translate_operation env acc (o : Ast.ctx_operation_decl) =
  let source = fst o.ctx_op_source in (* TODO *)
  let acc, tree =
    translate_guarded_redist env acc ~default_dest:o.ctx_op_default_dest
      o.ctx_op_guarded_redistrib
  in
  Acc.add_redist acc ~source tree

let translate_declaration env acc (decl : Ast.contextualized Ast.declaration) =
  match decl with
  | DVarOperation o -> translate_operation env acc o
  | DVarEvent e ->
    let evt_formula = translate_event acc e.ctx_event_expr in
    Acc.register_event acc e.ctx_event_var evt_formula
  | DVarAdvance _
  | DVarDefault _
  | DVarDeficit _ -> assert false

let translate_program (Contextualized (infos, prog) : Ast.contextualized Ast.program) =
  let acc = Acc.make infos in
  let acc =
    List.fold_left
      (fun acc decl ->
         translate_declaration () acc decl
      )
      acc prog
  in
  ignore acc
