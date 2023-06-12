
module StrMap = Map.Make(String)

type variable = int

module VariableSet = Set.Make(Int)
module VariableMap = Map.Make(Int)

type money = int

type value_type =
  | TInteger
  | TRational
  | TMoney
  | TEvent
  | TDate
  | TDuration

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
  | Variable of variable * flow_view
  | Binop of binop * formula * formula
  | RCast of formula

type event =
  | EvtVar of variable
  | EvtAnd of event * event
  | EvtOr of event * event
  | EvtCond of formula
  | EvtDate of formula

let rec reduce_formula _env (f : formula) =
  match f with
  | Literal _
  | Variable _ -> f
  | RCast f ->
    begin match reduce_formula _env f with
      | Literal (LInteger i) -> Literal (LRational (float_of_int i))
      | Literal _ -> assert false
      | _ -> f
    end
  | Binop (IAdd, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LInteger i1), Literal (LInteger i2) -> Literal (LInteger (i1 + i2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (RAdd, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LRational f1), Literal (LRational f2) -> Literal (LRational (f1 +. f2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (MAdd, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LMoney m1), Literal (LMoney m2) -> Literal (LMoney (m1 + m2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DAdd, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LDate d), Literal (LDuration dr) ->
        Literal (LDate (CalendarLib.Date.add d dr))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DrAdd, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LDuration dr1), Literal (LDuration dr2) ->
        Literal (LDuration (CalendarLib.Date.Period.add dr1 dr2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (ISub, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LInteger i1), Literal (LInteger i2) -> Literal (LInteger (i1 - i2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (RSub, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LRational f1), Literal (LRational f2) -> Literal (LRational (f1 -. f2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (MSub, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LMoney m1), Literal (LMoney m2) -> Literal (LMoney (m1 - m2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DSub, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LDate d), Literal (LDuration dr) ->
        Literal (LDate (CalendarLib.Date.rem d dr))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DrSub, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LDuration dr1), Literal (LDuration dr2) ->
        Literal (LDuration (CalendarLib.Date.Period.sub dr1 dr2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (IMult, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LInteger i1), Literal (LInteger i2) -> Literal (LInteger (i1 * i2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (RMult, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LRational f1), Literal (LRational f2) -> Literal (LRational (f1 *. f2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (MMult, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LMoney m), Literal (LRational f) ->
        Literal (LMoney (int_of_float ((float_of_int m) *. f)))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DrMult, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LDuration dr), Literal (LRational f) ->
        let open CalendarLib in
        let ddr = Date.Period.nb_days dr in
        Literal (LDuration (Date.Period.day (int_of_float (float_of_int ddr *. f +. 0.5))))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (IDiv, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LInteger i1), Literal (LInteger i2) -> Literal (LInteger (i1 / i2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (RDiv, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LRational f1), Literal (LRational f2) -> Literal (LRational (f1 /. f2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (MDiv, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LMoney m), Literal (LRational f) ->
        Literal (LMoney (int_of_float ((float_of_int m) /. f)))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DrDiv, f1, f2) ->
    begin match reduce_formula _env f1, reduce_formula _env f2 with
      | Literal (LDuration dr), Literal (LRational f) ->
        let open CalendarLib in
        let ddr = Date.Period.nb_days dr in
        Literal (LDuration (Date.Period.day (int_of_float (float_of_int ddr /. f +. 0.5))))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop ((IEq|REq|MEq|DEq|DrEq), _, _) -> f

let squash_to_rational env (f : formula) =
  let f = reduce_formula env f in
  match f with
  | Literal (LRational f) -> f
  | Literal _
  | RCast _
  | Binop _
  | Variable _ -> Errors.raise_error "Expected formula to be a constant rational"

module RedistTree = struct

  type redist =
    | NoInfo
    | Shares of float VariableMap.t
    | Flats of formula VariableMap.t

  type tree =
    | Redist of redist
    | Branch of variable * tree * tree

  let share (dest : variable) (formula, _ft : formula * value_type) =
    match formula with
    | Literal (LRational f) ->
      Shares (VariableMap.singleton dest f)
    | _ -> Errors.raise_error "Expected formula to be a rational literal"

  let flat (dest : variable) (formula, ftype : formula * value_type) =
    if ftype <> TMoney then
      Errors.raise_error "Expected formula of type money";
    Flats (VariableMap.singleton dest formula)

  let redist (r : redist) = Redist r

  let until (evt : variable) (t : tree) =
    Branch(evt, t, Redist NoInfo)

  let from (evt : variable) (t : tree) =
    Branch(evt, Redist NoInfo, t)

  let merge_redist (r1 : redist) (r2 : redist) =
    match r1, r2 with
    | NoInfo, NoInfo -> NoInfo
    | NoInfo, r | r, NoInfo -> r
    | Shares s1, Shares s2 ->
      let new_s =
        VariableMap.union (fun _dest s1 s2 ->
            (* TODO warning *)
            Some (s1 +. s2))
          s1 s2
      in
      Shares new_s
    | Flats f1, Flats f2 ->
      let new_f =
        VariableMap.union (fun _dest f1 f2 ->
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


module Env = struct

  type t = {
    name_table : string VariableMap.t;
    var_table : variable StrMap.t;
    inputs : VariableSet.t;
    actors : VariableSet.t;
    types : value_type VariableMap.t;
    trees : RedistTree.tree VariableMap.t;
    events : event VariableMap.t;
  }

  let empty = {
    name_table = VariableMap.empty;
    var_table = StrMap.empty;
    inputs = VariableSet.empty;
    actors = VariableSet.empty;
    types = VariableMap.empty;
    trees = VariableMap.empty;
    events = VariableMap.empty;
  }

  let anon_var_name =
    let c = ref 0 in
    fun name ->
      let i = !c in incr c;
      "anon_" ^ name ^ "_" ^ string_of_int i

  let fresh_id =
    let c = ref 0 in
    fun () -> let i = !c in incr c; i

  let add_var t name =
    let id = fresh_id () in
    let t = { t with
      name_table = VariableMap.add id name t.name_table;
      var_table = StrMap.add name id t.var_table;
    }
    in
    t, id

  let bind_type t v typ =
    { t with
      types = VariableMap.add v typ t.types
    }

  let bind_actor t v =
    { t with
      actors = VariableSet.add v t.actors
    }

  let bind_input t v =
    { t with
      inputs = VariableSet.add v t.inputs
    }

  let bind_event t v event =
    { t with
      events = VariableMap.add v event t.events
    }

  let type_from_ast (typ : Ast.typ) =
    (* TODO factorize *)
    match typ with
    | Rational -> TRational
    | Money -> TMoney
    | Integer -> TInteger
    | _ -> assert false

  let register_input t (i : Ast.input_decl) =
    let t, v = add_var t i.input_name in
    let t = bind_type t v (type_from_ast i.input_type) in
    bind_input t v

  let register_actor t (a : Ast.actor_decl) =
    let t, v = add_var t a in
    let t = bind_type t v TMoney in
    bind_actor t v

  let register_event t (name : string) (event_expr : event) =
    let t, v = add_var t name in
    let t = bind_type t v TEvent in
    bind_event t v event_expr

  let find_var_opt t (name : string) =
    StrMap.find_opt name t.var_table

  let find_var t (name : string) =
    match find_var_opt t name with
    | Some v -> v
    | None -> Errors.raise_error "Unknown identifier %s" name

  let find_var_with_type t (name : string) : variable * value_type =
    let v = find_var t name in
    v, VariableMap.find v t.types

  let find_holder_with_name t (flow : Ast.holder) =
    match flow with
    | Pool (name, None)
    | Actor (PlainActor name) -> find_var_opt t name, name
    | Pool (_, Some _)
    | Actor (LabeledActor _) -> assert false

  let find_holder t (flow : Ast.holder) =
    match find_holder_with_name t flow with
    | Some v, _name -> v
    | None, name -> Errors.raise_error "Unknown identifier" ~span:("Unknown holder "^name)

  let pull_holder t (flow : Ast.holder) =
    match find_holder_with_name t flow with
    | Some v, _name -> t, v
    | None, name ->
      let t, v = add_var t name in
      bind_type t v TMoney, v

  let add_redist t ~(source : variable) (tree : RedistTree.tree) =
    let trees =
      VariableMap.update source (function
      | None -> Some tree
      | Some existing_tree ->
        Some (RedistTree.unordered_merge existing_tree tree))
        t.trees
    in
    { t with trees }

end

let translate_literal (l : Ast.literal) =
  match l with
  | LitInt i -> LInteger i, TInteger
  | LitRational r -> LRational r, TRational
  | LitMoney c -> LMoney c, TMoney
  | LitDate d -> LDate d, TDate
  | LitDuration d -> LDuration d, TDuration

let translate_binop (op : Ast.binop) (f1, t1 : formula * value_type) (f2, t2 : formula * value_type) =
  match op, t1, t2 with
  | Add, TInteger, TInteger -> Binop (IAdd, f1, f2), TInteger
  | Add, TInteger, TRational -> Binop (RAdd, RCast f1, f2), TRational
  | Add, TRational, TInteger -> Binop (RAdd, f1, RCast f2), TRational
  | Add, TRational, TRational -> Binop (RAdd, f1, f2), TRational
  | Add, TMoney, TMoney -> Binop (MAdd, f1, f2), TMoney
  | Add, TDate, TDuration -> Binop (DAdd, f1, f2), TDate
  | Add, TDuration, TDate -> Binop (DAdd, f2, f1), TDate
  | Sub, TInteger, TInteger -> Binop (ISub, f1, f2), TInteger
  | Sub, TInteger, TRational -> Binop (RSub, RCast f1, f2), TRational
  | Sub, TRational, TInteger -> Binop (RSub, f1, RCast f2), TRational
  | Sub, TRational, TRational -> Binop (RSub, f1, f2), TRational
  | Sub, TMoney, TMoney -> Binop (MSub, f1, f2), TMoney
  | Sub, TDate, TDuration -> Binop (DSub, f1, f2), TDate
  | Mult, TInteger, TInteger -> Binop (IMult, f1, f2), TInteger
  | Mult, TInteger, TRational -> Binop (RMult, RCast f1, f2), TRational
  | Mult, TRational, TInteger -> Binop (RMult, f1, RCast f2), TRational
  | Mult, TRational, TRational -> Binop(RMult, f1, f2), TRational
  | Mult, TMoney, TInteger -> Binop (MMult, f1, RCast f2), TMoney
  | Mult, TMoney, TRational -> Binop (MMult, f1, f2), TMoney
  | Mult, TInteger, TMoney -> Binop (MMult, f2, RCast f1), TMoney
  | Mult, TRational, TMoney -> Binop (MMult, f2, f1), TMoney
  | Mult, TDuration, TInteger -> Binop (DrMult, f1, RCast f2), TDuration
  | Mult, TDuration, TRational -> Binop (DrMult, f1, f2), TDuration
  | Mult, TInteger, TDuration -> Binop (DrMult, f2, RCast f1), TDuration
  | Mult, TRational, TDuration -> Binop (DrMult, f2, f1), TDuration
  | Div, TInteger, TInteger -> Binop (IDiv, f1, f2), TInteger
  | Div, TInteger, TRational -> Binop (RDiv, RCast f1, f2), TRational
  | Div, TRational, TInteger -> Binop (RDiv, f1, RCast f2), TRational
  | Div, TRational, TRational -> Binop (RDiv, f1, f2), TRational
  | Div, TMoney, TInteger -> Binop (MDiv, f1, RCast f2), TMoney
  | Div, TMoney, TRational -> Binop (MDiv, f1, f2), TMoney
  | Div, TDuration, TInteger -> Binop (DrDiv, f1, RCast f2), TDuration
  | Div, TDuration, TRational -> Binop (DrDiv, f1, f2), TDuration
  | _ -> Errors.raise_error "Mismatching types for binop"

let translate_comp (comp : Ast.comp) (f1, t1 : formula * value_type) (f2, t2 : formula * value_type) =
  match comp, t1, t2 with
  | Eq, TInteger, TInteger -> Binop (IEq, f1, f2), TEvent
  | Eq, TInteger, TRational -> Binop (REq, RCast f1, f2), TEvent
  | Eq, TRational, TInteger -> Binop (REq, f1, RCast f2), TEvent
  | Eq, TRational, TRational -> Binop (REq, f1, f2), TEvent
  | Eq, TMoney, TMoney -> Binop (MEq, f1, f2), TEvent
  | _ -> Errors.raise_error "Mismatching types for comp"

let rec translate_formula env ~(view : flow_view) (f : Ast.formula) =
  match f with
  | Literal l ->
    let f, t = translate_literal l in
    Literal f, t
  | Named (Name (name, None)) ->
    let v, t = Env.find_var_with_type env name in
    Variable (v, view), t
  | Binop (op, f1, f2) ->
    let f1 = translate_formula env ~view f1 in
    let f2 = translate_formula env ~view f2 in
    translate_binop op f1 f2
  | Comp (comp, f1, f2) ->
    let f1 = translate_formula env ~view f1 in
    let f2 = translate_formula env ~view f2 in
    translate_comp comp f1 f2
  | Instant f -> translate_formula env ~view:AtInstant f
  | Total f -> translate_formula env ~view:Cumulated f
  | Named (Name (_, Some _))
  | Named (Holder _)
  | HolderExpr _ -> assert false

let translate_redist env ~(dest : variable) (redist : Ast.redistribution) =
  match redist with
  | Part f ->
    let f, ft = translate_formula env ~view:AtInstant f in
    RedistTree.share dest (reduce_formula env f, ft)
  | Flat f ->
    let f = translate_formula env ~view:AtInstant f in
    RedistTree.flat dest f
  | Retrocession _ -> assert false

let rec translate_event env (event : Ast.event_expr) =
  match event with
  | EventId name ->
    let v, t = Env.find_var_with_type env name in
    if t <> TEvent then
      Errors.raise_error "Identifier is not an event";
    EvtVar v
  | EventConj (e1, e2) ->
    let e1 = translate_event env e1 in
    let e2 = translate_event env e2 in
    EvtAnd(e1,e2)
  | EventDisj (e1, e2) ->
    let e1 = translate_event env e1 in
    let e2 = translate_event env e2 in
    EvtOr(e1,e2)
  | EventFormula f ->
    let f, t = translate_formula env ~view:Cumulated f in
    match t with
    | TEvent -> EvtCond f
    | TDate -> EvtDate f
    | TInteger | TRational
    | TMoney | TDuration -> Errors.raise_error "Formula is not an event"

let lift_event env (event : Ast.event_expr) =
  match event with
  | EventId name ->
     let v, t = Env.find_var_with_type env name in
     if t <> TEvent then
       Errors.raise_error "Identifier is not an event";
     env, v
  | event ->
    let evt = translate_event env event in
    let name = Env.anon_var_name "event" in
    let env = Env.register_event env name evt in
    env, Env.find_var env name

let rec translate_guarded_redist env ~(default_dest : variable option)
    (redist : Ast.guarded_redistrib) =
  match redist with
  | Redist (redist, dest) ->
    let env, dest =
      match default_dest, dest with
      | Some _, Some (dest, NoOpposition) (* TODO warning *)
      | None, Some (dest, NoOpposition) -> Env.pull_holder env dest
      | Some _, Some (_, Opposable _)
      | None, Some (_, Opposable _) -> assert false
      | Some default, None -> env, default
      | None, None -> Errors.raise_error "No source for repartition"
    in
    env, RedistTree.redist (translate_redist env ~dest redist)
  | Seq grs ->
    let env, trees =
      List.fold_left_map (translate_guarded_redist ~default_dest) env grs
    in
    begin match trees with
      | [] -> assert false
      | t::ts -> env, List.fold_left RedistTree.ordered_merge t ts
    end
  | Guarded (guard, redist) ->
    let env, tree = translate_guarded_redist env ~default_dest redist in
    match guard with
    | Before event ->
      let env, evt = lift_event env event in
      env, RedistTree.until evt tree
    | After event ->
      let env, evt = lift_event env event in
      env, RedistTree.from evt tree
    | When _ -> assert false

let translate_declaration env (decl : Ast.declaration) =
  match decl with
  | DInput i -> Env.register_input env i
  | DActor a -> Env.register_actor env a
  | DOperation o ->
    let source = Env.find_holder env o.op_source in
    let env, default_dest =
      match o.op_default_dest with
      | None -> env, None
      | Some (out, Ast.NoOpposition) ->
        let env, dest = Env.pull_holder env out in
        env, Some dest
      | Some (_, Opposable _) -> assert false
    in
    let env, tree =
      translate_guarded_redist env ~default_dest o.op_guarded_redistrib
    in
    Env.add_redist env ~source tree
  | DEvent e ->
    let evt_formula = translate_event env e.event_expr in
    Env.register_event env e.event_name evt_formula
  | DAdvance _
  | DConstant _
  | DContext _
  | DDefault _
  | DDeficit _ -> assert false

let translate_program (p : Ast.program) =
  let env =
    List.fold_left
      (fun env decl ->
         translate_declaration env decl
      )
      Env.empty p
  in
  ignore env
