open Internal
open Ir

type var_kind =
  | Pool

let var_kind (p : program) (var : Variable.t) =
  (* TODO *)
  ignore (p, var);
  Pool

(******************)

type value = (* TODO *) int

type 'a var_value = {
  next : 'a;
  stage : 'a;
  past_total : 'a;
}

type event = (* TODO *) bool

type action =
  | PoolRep of Variable.t * value
  | FlatCheck

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

type output_line = (event_switch * count_changes) list

type input_line = {
  input_variable : Variable.t;
  input_value : Ir.literal;
  input_date : Date.Date.t;
}

module InputLineMap = IntMap

type computation_inputs = input_line InputLineMap.t

type computation_outputs = output_line InputLineMap.t

type state = {
  event_state : event var_value Variable.Map.t;
  values : value var_value Variable.Map.t;
  queue : action list;
  stage_trace : output_line;
}

let empty_tally = { at_instant = 0; total = 0 }

let empty_count = { tally = empty_tally; repartition = Variable.Map.empty }

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
  | None -> 0
  | Some vv -> vv.next

let get_state_stage_value (s : state) (v : Variable.t) =
  match Variable.Map.find_opt v s.values with
  | None -> 0
  | Some vv -> vv.stage

let update_trace_top (f : count Variable.Map.t -> count Variable.Map.t) (s : state) =
  { s with
    stage_trace =
      match s.stage_trace with
      | [] -> Errors.raise_error "(internal) trace stack should have been initialized"
      | (evt, count)::t -> (evt, f count)::t
  }

let add_repartition (s : state) (src : Variable.t) (values : value Variable.Map.t) =
  let values = Variable.Map.filter (fun _ v -> v <> 0) values in
  if Variable.Map.is_empty values then s else
    update_trace_top (Variable.Map.update src (function
        | None -> Some { tally = empty_tally; repartition = values }
        | Some c -> Some { c with repartition = values }))
      s

let add_next_to_trace (s : state) =
  update_trace_top (fun count ->
      Variable.Map.fold (fun var vv count ->
          if vv.next = 0 then count else
            let tally = {
              at_instant = vv.next;
              total = vv.next + vv.stage + vv.past_total;
            }
            in
            Variable.Map.update var (function
                | None -> Some { tally; repartition = Variable.Map.empty }
                | Some c -> Some { c with tally })
              count)
        s.values count)
    s

let add_events_to_trace (s : state) (events : event Variable.Map.t) =
  let stage_trace =
    Variable.Map.fold (fun event value trace ->
        (SwitchTo { event; value }, Variable.Map.empty)::trace)
      events s.stage_trace
  in
  { s with stage_trace }

let add_val_to_next (s : state) (values : value Variable.Map.t) =
  Variable.Map.fold (fun var value s ->
      { s with
        values =
          Variable.Map.update var (function
              | None -> Some { next = value; stage = 0; past_total = 0 }
              | Some vv -> Some { vv with next = vv.next + value })
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

let div_value (v : value) (f : float) =
  (* TODO proper computation for each type *)
  Int.of_float (Float.round (Int.to_float v /. f))

let mult_value (v : value) (f : float) =
  (* TODO proper computation for each type *)
  Int.of_float (Float.round (Int.to_float v *. f))

let literal_value (l : Ir.literal) =
  match l with
  | Ir.LInteger i -> i
  | Ir.LMoney m -> m
  | Ir.LRational _
  | Ir.LDate _
  | Ir.LDuration _ -> assert false

let rec evaluate_eqex (s : state) (src : value) (expr : eqex) =
  match expr with
  | EZero -> 0
  | ESrc -> src
  | EConst l -> literal_value l
  | EMult (f, e) ->
    let e = evaluate_eqex s src e in
    mult_value e f
  | EAdd (e1, e2) ->
    let e1 = evaluate_eqex s src e1 in
    let e2 = evaluate_eqex s src e2 in
    e1 + e2
  | EMinus e ->
    let e = evaluate_eqex s src e in
    -e
  | EVar v -> begin
    match Variable.Map.find_opt v s.values with
    | None -> 0
    | Some vv -> vv.past_total (* + vv.stage *)
  end
  | ECurrVar v ->
    match Variable.Map.find_opt v s.values with
    | None -> 0
    | Some vv -> vv.stage

type eq_sem =
  | LimLt
  | LimGe

type limit_approach =
  | Reach
  | MustCross
  | Diverge
  | NoLim

let compute_equation_diff (s : state) (value : value) (eq : cond)
    (sem : eq_sem) =
  match eq with
  | CNorm (f, expr) ->
    let expr_val = evaluate_eqex s value expr in
    let lim_app =
      if f = 0. then NoLim else
        match sem with
        | LimLt -> if f < 0. then Diverge else Reach
        | LimGe -> if f > 0. then Diverge else MustCross
    in
    lim_app, if lim_app = NoLim then - expr_val else div_value expr_val f
  | _ -> Errors.raise_error "(internal) equation should have been normalized"

let find_event_threshold (p : program) (s : state) (src : Variable.t) (value : value) =
    let event_state = get_event_state p s in
    Variable.Map.fold (fun evt sourced min_val ->
        let deq = get_source src sourced in
        match Variable.BDT.find event_state deq with
        | None -> min_val
        | Some eq ->
          (* let value = get_state_stage_value s src + value in *)
          let sem =
            if Variable.Map.find evt event_state
            then LimGe
            else LimLt
          in
          let lim_app, diff = compute_equation_diff s value eq sem in
          match lim_app with
          | Reach -> min diff min_val
          | MustCross -> min min_val (diff+1)
          | NoLim | Diverge -> min_val)
      p.equations value

let push_rep_action (p : program) (s : state) (var : Variable.t) (value : value) =
  match var_kind p var with
  | Pool ->
    { s with queue = PoolRep (var, value)::s.queue }

let push_flat_check (s : state) =
  { s with
    queue = match s.queue with
      | [] -> [ FlatCheck ]
      | q -> q
  }

let flush_stage (s : state) =
  { s with
    values =
      Variable.Map.map (fun v ->
          { next = 0;
            stage = 0;
            past_total = v.past_total + v.stage })
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
  { s with
    values =
      Variable.Map.map (fun v ->
          { v with
            next = 0;
            stage = v.stage + v.next; })
        s.values;
  }

let compute_formula (_s : state) (formula : Ir.formula) =
  match formula with
  | Literal l -> literal_value l
  | Ir.Variable (_, _) -> assert false
  | Ir.Binop (_, _, _) -> assert false
  | Ir.RCast _ -> assert false

let compute_redist (type a) (s : state) (r : a Ir.RedistTree.redist) (value : value) =
  match r with
  | NoInfo -> Variable.Map.empty
  | Shares sh -> Variable.Map.map (mult_value value) sh
  | Flats fs -> Variable.Map.map (compute_formula s) fs

let rec compute_tree : type a. state -> a Ir.RedistTree.tree -> value -> value Variable.Map.t =
  fun s tree value ->
  match tree with
  | Nothing -> Variable.Map.empty
  | Redist r -> compute_redist s r value
  | When ws ->
    List.fold_left (fun res (evt, tree) ->
        if get_event_value s evt then
          let r = compute_tree s tree value in
          Variable.Map.union (fun _v r1 r2 -> Some (r1 + r2)) res r
        else res)
      Variable.Map.empty ws
  | Branch { evt; before; after } ->
    if get_event_value s evt
    then compute_tree s after value
    else compute_tree s before value

let compute_trees (s : state) (trees : Ir.RedistTree.t) (value : value) =
  let res_union = Variable.Map.union (fun _v r1 r2 -> Some (r1 + r2)) in
  match trees with
  | Flat fs ->
    List.fold_left (fun res tree ->
        let r = compute_tree s tree value in
        res_union res r)
      Variable.Map.empty fs
  | Fractions { base_shares; default; branches } ->
    let base = compute_redist s base_shares value in
    let default =
      match default with
        | NoDefault -> Variable.Map.empty
        | DefaultVariable _ ->
          Errors.raise_error "(internal) Default should have been computed"
        | DefaultTree default -> compute_tree s default value
    in
    List.fold_left (fun res tree ->
        let r = compute_tree s tree value in
        res_union res r)
      (res_union base default) branches

let update_state_values (p : program) (s : state) =
  let s =
    List.fold_left (fun s var ->
        let value = get_state_next_value s var in
        let redist_res =
          match Variable.Map.find_opt var p.trees with
          | None ->
            if Variable.Map.mem var p.infos.actors then Variable.Map.empty else
              Errors.raise_error "(internal) No tree found for variable redist"
          | Some trees ->
            compute_trees s trees value
        in
        let s = add_repartition s var redist_res in
        add_val_to_next s redist_res)
      s p.eval_order
  in
  add_next_to_trace s

let update_state_events (p : program) (s : state) =
  let event_state = get_event_state p s in
  let flipping_events =
    Variable.Map.fold (fun evt sourced flipped ->
        let evt_state = get_event_value s evt in
        match Variable.BDT.find event_state sourced.other_src with
        | None ->
          if evt_state
          then Variable.Map.add evt false flipped
          else flipped
        | Some eq ->
          let sem = if evt_state then LimGe else LimLt in
          let _, diff = compute_equation_diff s 0 eq sem in
          if evt_state && diff < 0
          then Variable.Map.add evt false flipped
          else if not evt_state && diff >= 0
          then Variable.Map.add evt true flipped
          else flipped)
      p.equations Variable.Map.empty
  in
  let s = add_events_to_trace s flipping_events in
  add_evt_to_stage s flipping_events

let compute_action (p : program) (s : state) (action : action) =
  let s =
    match action with
    | FlatCheck -> update_state_values p s
    | PoolRep (var, value) ->
      let value_at_threshold = find_event_threshold p s var value in
      let s =
        if value > 0 then
          push_rep_action p s var (value - value_at_threshold)
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
  let value =
    match value with
    | Ir.LInteger i -> i
    | Ir.LMoney m -> m
    | Ir.LRational _ -> assert false
    | Ir.LDate _ -> assert false
    | Ir.LDuration _ -> assert false
  in
  let s = flush_stage s in
  let s = push_rep_action p s i value in
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
    IntMap.fold (fun i input (s, out_ls) ->
        let s = compute_input_value p s input.input_variable input.input_value in
        s, IntMap.add i s.stage_trace out_ls
      )
      lines (s, IntMap.empty)
  in
  output_lines
