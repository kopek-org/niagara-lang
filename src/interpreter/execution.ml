open Internal
open Ir

type value = Value.t

(* TODO: replace with Internal.Interface.variable_kind *)
type var_kind =
  | Pool

let var_kind (p : program) (var : Variable.t) =
  (* TODO *)
  ignore (p, var);
  Pool

(* A note on execution semantics:

   A computation step is caracterized by the fact that it processes the entire
   value of an input. Although, this value may be broken down into several
   pieces to account for event thresholds. This makes several sequential
   sub-steps that, together, we call a stage.

   Taking staged values is important because a program can query either the
   integrated value since the beginning of time of the value currently flowing
   in. "Currently", from the user point of view, being the value passing at one
   input (i.e. a computation step), and not one sub-step.

   Hence, the following type holding this distinction:
   - [next] for the currently computed sub-step
   - [staged] for the previouly computed value cumulated since the beginning of
     the current stage.
   - [past_total] the cumulated values of all previous stages. *)

type 'a var_value = {
  next : 'a;
  stage : 'a;
  past_total : 'a;
}

type event = (* TODO *) bool

(* The interpreter uses a stack of action to make during a stage. This is to
   handle the consecutive splits of input value. As well as a good basis for
   futur extensions. *)

type action =
  | PoolRep of Variable.t * value

(* Some types to output computation traces. *)

type tally = {
  at_instant : value;
  total : value;
}

type count = {
  tally : tally;
  repartition : value Variable.Map.t;
}

type event_switch =
  | NoEvent
  | SwitchTo of { event : Variable.t; value : bool }

type count_changes = count Variable.Map.t

(* Piled up, this is in reverse chronological order *)
type output_line = (event_switch * count_changes) list

type input_line = {
  input_variable : Variable.t;
  input_value : Ir.literal;
  input_date : Date.Date.t;
}

module InputLineMap = IntMap
(* any uid linking input and output lines would do *)

type computation_inputs = input_line InputLineMap.t

type computation_outputs = output_line InputLineMap.t

(* The whole state of execution.

   It contains the traces for outputing results. I feel the execution itself
   should not handle it itself, and rather be instrumented by another piece of
   code. But it will do for now. *)
type state = {
  event_state : event var_value Variable.Map.t;
  values : value var_value Variable.Map.t;
  queue : action list;
  stage_trace : output_line;
}

let empty_tally = { at_instant = Value.zero; total = Value.zero }

let get_event_value (s : state) (e : Variable.t) =
  match Variable.Map.find_opt e s.event_state with
  | None -> false
  | Some vv -> vv.stage

let get_event_state (p : program) (s : state) =
  Variable.Map.mapi (fun evt _ ->
      match Variable.Map.find_opt evt s.event_state with
      | None -> false
      | Some vv -> vv.stage)
    p.equations

let get_state_next_value (s : state) (v : Variable.t) =
  match Variable.Map.find_opt v s.values with
  | None -> Value.zero
  | Some vv -> vv.next

let get_state_cumulated_value (s : state) (v : Variable.t) =
  match Variable.Map.find_opt v s.values with
  | None -> Value.zero
  | Some vv -> Value.add (Value.add vv.next vv.stage) vv.past_total

let update_trace_top (f : count Variable.Map.t -> count Variable.Map.t) (s : state) =
  { s with
    stage_trace =
      match s.stage_trace with
      | [] -> Errors.raise_error "(internal) trace stack should have been initialized"
      | (evt, count)::t -> (evt, f count)::t
  }

let add_repartition (s : state) (src : Variable.t) (values : value Variable.Map.t) =
  let values = Variable.Map.filter (fun _ v -> not (Value.is_zero v)) values in
  if Variable.Map.is_empty values then s else
    update_trace_top (Variable.Map.update src (function
        | None -> Some { tally = empty_tally; repartition = values }
        | Some c -> Some { c with repartition = values }))
      s

let add_next_to_trace (s : state) =
  update_trace_top (fun count ->
      Variable.Map.fold (fun var vv count ->
          Variable.Map.update var (function
              | None -> Some {
                  tally = {
                    at_instant = vv.next;
                    total = Value.add (Value.add vv.next vv.stage) vv.past_total;
                  };
                  repartition = Variable.Map.empty
                }
              | Some c -> Some {
                  c with
                  tally = {
                    at_instant = Value.add vv.next c.tally.at_instant;
                    total = Value.add vv.next c.tally.total;
                  }
                })
            count)
        s.values count)
    s

let add_events_to_trace (s : state) (events : event Variable.Map.t) =
  let stage_trace =
    Variable.Map.fold (fun event value trace ->
        let past = get_event_value s event in
        (* Push only flipped events *)
        if past <> value then
          (SwitchTo { event; value }, Variable.Map.empty)::trace
        else trace)
      events s.stage_trace
  in
  { s with stage_trace }

let add_val_to_next (s : state) (values : value Variable.Map.t) =
  Variable.Map.fold (fun var value s ->
      { s with
        values =
          Variable.Map.update var (function
              | None ->
                let zero = Value.zero in
                Some { next = value;
                       stage = zero;
                       past_total = zero }
              | Some vv -> Some { vv with next = Value.add vv.next value })
            s.values
      })
    values s

let add_evt_to_stage (s : state) (evts : event Variable.Map.t) =
  let event_state =
    Variable.Map.merge (fun _evt s e ->
        match s, e with
        | None, None -> None
        | Some s, None -> Some s
        | None, Some e -> Some { next = false; stage = e; past_total = false }
        | Some s, Some e -> Some { s with stage = e })
      s.event_state evts
  in
  { s with event_state }

(* Fractional operations are handled poorly. This is however enough to have a
   plausible approximation (and in most cases correct) of a strictly correct
   results. For this to become actually reliable in all cases, the changes are
   two-folds:
   - Handle rationnals as rationnals and not as floating point numbers.

   - Propose a solution for fractionnal reattributions of values lower that 1
   (in most cases, cent). Not only this is a fundamental issue that the user
   must be aware of (and control is some way), but this also mean that we need
   to change how fraction are represented in order to make sure the invariant
   "for each cent in, a cent out" holds, which is not trival and might depend on
   the solution adopted to the more fundamental issue. *)

let literal_value (l : Ir.literal) : value =
  match l with
  | Ir.LInteger i
  | Ir.LMoney i -> VRat R.(~$i)(* VInt i *)
  | Ir.LRational r -> VRat r
  | Ir.LDate _
  | Ir.LDuration _ -> assert false

let rec evaluate_eqex (s : state) (src : value) (expr : eqex) : value =
  match expr with
  | EZero -> VRat R.zero (* VZero *)
  | ESrc -> src
  | EConst l -> literal_value l
  | EMult (e1, e2) ->
    let e1 = evaluate_eqex s src e1 in
    let e2 = evaluate_eqex s src e2 in
    Value.mult e1 e2
  | EAdd (e1, e2) ->
    let e1 = evaluate_eqex s src e1 in
    let e2 = evaluate_eqex s src e2 in
    Value.add e1 e2
  | EMinus e ->
    let e = evaluate_eqex s src e in
    Value.minus e
  | EVar v -> begin
    match Variable.Map.find_opt v s.values with
    | None -> VRat R.zero (* VZero *)
    | Some vv -> vv.past_total
  end
  | ECurrVar v ->
    match Variable.Map.find_opt v s.values with
    | None -> VRat R.zero (* VZero *)
    | Some vv -> vv.stage

(* Regarding event threshold crossing, we must be able to identify on which side
   of the equality we are (i.e. < or =>). *)
type eq_sem =
  | LimLt
  | LimGe

(* Distinction of threshold approach behavior *)
type limit_approach =
  | Reach (* attains equality *)
  | MustCross (* reaching equality won't change state, going across will *)
  | Diverge   (* getting farther away from the threshold *)
  | NoLim (* no movement, or unreachable threshold *)

let find_event_threshold (p : program) (s : state) (src : Variable.t) (value : value) =
  let compute_equation_diff (eq : cond) (sem : eq_sem) : limit_approach * value =
    match eq with
    | CNorm { src_factor; const } ->
      let const_val = evaluate_eqex s value const in
      let factor_val = evaluate_eqex s value src_factor in
      if Value.is_zero factor_val then NoLim, Value.minus const_val else
        (* Assuming normalized equations have the property of indicating their
           direction regarding the signed difference between their sides. A
           positive (resp. negative) factor mean the difference is increasing
           (resp. decreasing). I.e. "below" the equality it gets closer (resp.
           farther), and above father (resp. closer). *)
        let diff = Value.div const_val factor_val in
        let diff_is_decreasing =
          Value.is_negative diff || (Value.is_negative factor_val && Value.is_zero const_val)
        in
        let lim_app =
          match sem with
          | LimLt -> if diff_is_decreasing then Diverge else Reach
          | LimGe -> if diff_is_decreasing then MustCross else Diverge
        in
        lim_app, diff
    | _ -> Errors.raise_error "(internal) equation should have been normalized"
  in
  let event_state = get_event_state p s in
  Variable.Map.fold (fun evt sourced min_val ->
      let deq = get_source src sourced in
      match Variable.BDT.find event_state deq with
      | None -> min_val
      | Some eq ->
        let sem =
          if Variable.Map.find evt event_state
          then LimGe
          else LimLt
        in
        let lim_app, diff = compute_equation_diff eq sem in
        (* Assuming every threshold is a discrete value (mainly because it will
           be on money amount), we can discretise the result with the adapted
           policy. Recall that the exact equality is defined to be "after",
           reverting to a "before" state means we need to go strictly below *)
        let diff =
          match lim_app with
          | Reach -> Value.discretise ~mode:Ceil diff
          | MustCross -> Value.discretise ~mode:StrictIncrement diff
          | NoLim | Diverge -> min_val
        in
        Value.min min_val diff)
    p.equations value

let push_rep_action (p : program) (s : state) (var : Variable.t) (value : value) =
  match var_kind p var with
  | Pool ->
    { s with queue = PoolRep (var, value)::s.queue }

let flush_stage (s : state) =
  { s with
    values =
      Variable.Map.map (fun v ->
          { next = Value.VRat R.zero (* Value.VZero *);
            stage = Value.VRat R.zero (* Value.VZero *);
            past_total = Value.add v.past_total v.stage })
        s.values;
    event_state =
      Variable.Map.map (fun e ->
          { next = false;
            stage = e.stage;
            past_total = e.stage })
        s.event_state;
    stage_trace = [NoEvent, Variable.Map.empty];
  }

let flush_next (s : state) =
  let s = add_next_to_trace s in
  { s with
    values =
      Variable.Map.map (fun v ->
          { v with
            next = Value.zero;
            stage = Value.add v.stage v.next; })
        s.values;
  }

let rec compute_formula (p : program) (s : state) (formula : Ir.formula) : value =
  match formula with
  | Literal l -> literal_value l
  | Variable (v, view) ->
    begin match view with
    | AtInstant -> get_state_next_value s v
    | Cumulated -> get_state_cumulated_value s v
    end
  | Binop (op, f1, f2) ->
    let f1 = compute_formula p s f1 in
    let f2 = compute_formula p s f2 in
    match op with
    | Add -> Value.add f1 f2
    | Sub -> Value.add f1 (Value.minus f2)
    | Mult -> Value.mult f1 f2
    | Div -> Value.div f1 f2

let compute_redist (type a) (p : program) (s : state) (r : a Ir.RedistTree.redist) (value : value) =
  match r with
  | NoInfo -> Variable.Map.empty
  | Shares sh -> Variable.Map.map (fun f -> Value.mult value (VRat f)) sh
  | Flats fs ->
    let tsf = Variable.Map.map (compute_formula p s) fs.transfers in
    let blc =
      Variable.Map.mapi (fun v f ->
          let v_val = get_state_next_value s v in
          Value.mult v_val (VRat f))
        fs.balances
    in
    Variable.Map.union (fun _v r1 r2 -> Some (Value.add r1 r2)) tsf blc

let rec compute_tree : type a. program -> state -> a Ir.RedistTree.tree -> value -> value Variable.Map.t =
  fun p s tree value ->
  match tree with
  | Nothing -> Variable.Map.empty
  | Redist r -> compute_redist p s r value
  | When ws ->
    List.fold_left (fun res (evt, tree) ->
        if get_event_value s evt then
          let r = compute_tree p s tree value in
          Variable.Map.union (fun _v r1 r2 -> Some (Value.add r1 r2)) res r
        else res)
      Variable.Map.empty ws
  | Branch { evt; before; after } ->
    if get_event_value s evt
    then compute_tree p s after value
    else compute_tree p s before value

let compute_trees (p : program) (s : state) (trees : Ir.RedistTree.t) (value : value) =
  let res_union = Variable.Map.union (fun _v r1 r2 -> Some (Value.add r1 r2)) in
  match trees with
  | Flat fs ->
    List.fold_left (fun res tree ->
        let r = compute_tree p s tree value in
        res_union res r)
      Variable.Map.empty fs
  | Fractions { base_shares; balance; branches } ->
    let base = compute_redist p s base_shares value in
    let default =
      match balance with
        | BalanceVars _ ->
          Errors.raise_error "(internal) Default should have been computed"
        | BalanceTree default -> compute_tree p s default value
    in
    List.fold_left (fun res tree ->
        let r = compute_tree p s tree value in
        res_union res r)
      (res_union base default) branches

let compute_events (p : program) (s : state) =
  let rec memo mem (evt : Variable.t) =
    match Variable.Map.find_opt evt mem with
    | Some v -> mem, v
    | None ->
      match Variable.Map.find_opt evt p.events with
      | None -> Errors.raise_error "(internal) Event %d not found" (Variable.uid evt)
      | Some expr ->
        match expr with
        | EvtVar e ->
          let mem, evt_val = memo mem e in
          Variable.Map.add evt evt_val mem, evt_val
        | EvtOnRaise e ->
          let mem, rised = memo mem e in
          let past = get_event_value s e in
          let evt_val =
            if past then false else rised
          in
          Variable.Map.add evt evt_val mem, evt_val
        | EvtComp (Eq, f1, f2) ->
          let f1 = compute_formula p s f1 in
          let f2 = compute_formula p s f2 in
          let evt_val = not (Value.is_negative (Value.add f1 (Value.minus f2))) in
          Variable.Map.add evt evt_val mem, evt_val
        | EvtAnd (_, _)
        | EvtOr (_, _)
        | EvtDate _ -> assert false
  in
  Variable.Map.fold (fun evt _expr mem -> fst @@ memo mem evt)
    p.events Variable.Map.empty

let update_state_values (p : program) (s : state) =
  List.fold_left (fun s var ->
      let src_is_actor = Variable.Map.mem var p.infos.actors in
      let value = if src_is_actor then Value.zero else get_state_next_value s var in
      let redist_res =
        match Variable.Map.find_opt var p.trees with
        | None ->
          if Variable.Map.mem var p.infos.actors then Variable.Map.empty else
            Errors.raise_error "(internal) No tree found for variable redist"
        | Some trees -> compute_trees p s trees value
      in
      let s =
        if src_is_actor then
          (* providing actors are valuated on what they give *)
          let sum =
            Variable.Map.fold (fun _dest v acc -> Value.add acc v) redist_res Value.zero
          in
          add_val_to_next s (Variable.Map.singleton var sum)
        else s
      in
      let s = add_repartition s var redist_res in
      add_val_to_next s redist_res)
    s p.eval_order

(* This function uses equations to compute flipped events.

   In the event we want to try out more low-brow semantics of reattribution
   without stoping at event thresholds (i.e. every inputs is processed in one
   substep), we can change them with IR event expressions evaluation. This would
   be a rather elegant way to swap out semantics whever we choose to populate
   equations or not (or even choose which event needs a checkpoint!). *)
let update_state_events (p : program) (s : state) =
  let new_events_values = compute_events p s in
  let s = add_events_to_trace s new_events_values in
  add_evt_to_stage s new_events_values

let compute_action (p : program) (s : state) (action : action) =
  let s =
    match action with
    | PoolRep (var, value) ->
      let value_at_threshold = find_event_threshold p s var value in
      let s =
        if Value.is_negative (Value.minus value) then
          push_rep_action p s var (Value.add value (Value.minus value_at_threshold))
        else s
      in
      let s = add_val_to_next s (Variable.Map.singleton var value_at_threshold) in
      update_state_values p s
  in
  let s = flush_next s in
  update_state_events p s

let rec compute_queue (p : program) (s : state) =
  match s.queue with
  | [] -> s
  | action::queue ->
    let s = compute_action p { s with queue } action in
    compute_queue p s

let compute_input_value (p : program) (s : state) (i : Variable.t) (value : Ir.literal) =
  let s = flush_stage s in
  let s = push_rep_action p s i (literal_value value) in
  compute_queue p s

let init_state (p : program) =
  let s = {
    event_state = Variable.Map.empty;
    values = Variable.Map.empty;
    queue = [];
    stage_trace = []; }
  in
  let s = update_state_events p s in
  flush_stage s

let compute_input_lines (p : program) (lines : computation_inputs) =
  let s = init_state p in
  let _s, output_lines =
    List.fold_left (fun (s, out_ls) (i, input) ->
        let s = compute_input_value p s input.input_variable input.input_value in
        s, IntMap.add i s.stage_trace out_ls
      )
      (s, IntMap.empty)
      (IntMap.bindings lines
       |> List.sort (fun x y -> compare (fst x) (fst y)))
  in
  output_lines
