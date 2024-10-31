open Dataflow
open Equ

type value_presence =
  | Absent
  | Present of Value.t

(* The interpreter uses a stack of action to make during a stage. This is to
   handle the consecutive splits of input value. As well as a good basis for
   futur extensions. *)
type action =
  | PoolRep of Variable.t * Value.t

(* Represents knowledge related to activation conditions *)
type cond_state = bool Variable.Map.t

type state = {
  valuations : value_presence Variable.Map.t;
  events : bool Variable.Map.t;
  queue : action list;
}

let update_value (s : state) (var : Variable.t) (value : value_presence) =
  { s with
    valuations =
      Variable.Map.add var value s.valuations
  }

let update_event (s : state) (evt : Variable.t) (b : bool) =
  { s with
    events =
      Variable.Map.add evt b s.events
  }

let find_valuation (s : state) (v : Variable.t) =
  match Variable.Map.find_opt v s.valuations with
  | None | Some Absent ->
    Errors.raise_error "(internal) Unable to find valuation for var %d"
      (Variable.uid v)
  | Some (Present v) -> v

let rec find_valuation_in (s : state) (vs : Variable.t list) =
  match vs with
  | [] -> Errors.raise_error "(internal) Unable to find a valuation in vars"
  | v::vs ->
    match Variable.Map.find_opt v s.valuations with
    | Some (Present v) -> v
    | None | Some Absent -> find_valuation_in s vs

let find_truth (s : state) (v : Variable.t) =
  match Variable.Map.find_opt v s.events with
  | None ->
    Errors.raise_error "(internal) Unable to find truth for event %d"
      (Variable.uid v)
  | Some b -> b

let rec find_truth_in (s : state) (vs : Variable.t list) =
  match vs with
  | [] -> Errors.raise_error "(internal) Unable to find a truth in events"
  | v::vs ->
    match Variable.Map.find_opt v s.events with
    | Some b -> b
    | None -> find_truth_in s vs

(* TODO: replace with Internal.Interface.variable_kind *)
type var_kind =
  | Pool

let var_kind (p : program) (var : Variable.t) =
  (* TODO *)
  ignore (p, var);
  Pool

type output_step = {
  step_valuations : value_presence Variable.Map.t;
  step_events : bool Variable.Map.t;
}

type output_line = output_step list

type input_line = {
  input_variable : Variable.t;
  input_value : Literal.t;
}

module InputLineMap = IntMap
(* any uid linking input and output lines would do *)

type computation_inputs = input_line InputLineMap.t

type computation_outputs = output_line InputLineMap.t

(* We use rationnals throughout the interpreter for valuation which enable us to
   keep exact precision all the way (with specific rounding for event
   thresholds).

   This defers the issue of how to present the result and the handling of
   rounding errors to the user.

   Rationnal values are, in the general, sensible to complexity. However for our
   purpose its will mostly remain within combination of humain-readable
   percentages which reduces greatly the possibility of hard-to-reduce fractions
   appearance.
*)

let literal_value (l : Literal.t) : Value.t =
  match l with
  | LInteger i
  | LMoney i -> VRat R.(~$$i)
  | LRational r -> VRat r
  | LDate _
  | LDuration _ -> assert false

let push_rep_action (p : program) (s : state) (var : Variable.t) (value : Value.t) =
  match var_kind p var with
  | Pool ->
    { s with queue = PoolRep (var, value)::s.queue }

let value_inputs (p : program) (s : state) =
  let valuations =
    Variable.Map.fold (fun v infos vals ->
        if VarInfo.is_input infos then
          Variable.Map.add v Absent vals
        else vals)
      p.infos.Surface.Ast.var_info s.valuations
  in
  { s with valuations }

let rec valuate (s : state) (e : expr) =
  match e with
  | EVar v | EPre v -> (* assumes proper ordering/duplication *)
    find_valuation s v
  | EConst c -> literal_value c
  | EAdd (e1, e2) -> Value.add (valuate s e1) (valuate s e2)
  | EMult (e1, e2) -> Value.mult (valuate s e1) (valuate s e2)
  | ENeg e -> Value.minus (valuate s e)
  | EInv e -> Value.inv (valuate s e)
  | EMerge vs -> find_valuation_in s vs
  | ENot _ | EAnd _ | EGe _ ->
    Errors.raise_error "(internal) Tried to valuate@ %a,@, which it not a value"
      FormatEqu.print_expr e

let rec check (s : state) (e : expr) =
  match e with
  | EVar v | EPre v -> (* assumes proper ordering/duplication *)
    find_truth s v
  | ENot e -> not (check s e)
  | EAnd (e1, e2) -> (check s e1) && (check s e2)
  | EGe (e1, e2) -> not Value.(lt (valuate s e1) (valuate s e2))
  | EMerge vs -> find_truth_in s vs
  | EConst _ | EAdd _ | EMult _ | ENeg _ | EInv _ ->
    Errors.raise_error "(internal) Tried to check@ %a,@, which is not an event"
      FormatEqu.print_expr e

let compute_events (p : program) (s : state) =
  List.fold_left (fun s evt ->
      let { eq_act = _; eq_expr } = Variable.Map.find evt p.act_eqs in
      let b = check s eq_expr in
      update_event s evt b)
    s p.act_order

let compute_value (s : state) (cond_state : cond_state)
    ({ eq_act; eq_expr } : guarded_eq) =
  match Condition.satisfies cond_state eq_act with
  | MaySat ->
    Errors.raise_error "(internal) Unable to check activation, missing var value"
  | Unsat -> Absent
  | Sat -> Present (valuate s eq_expr)

let compute_values (p : program) (s : state) (cond_state : cond_state)
    (input : Variable.t) (in_value : Value.t) =
  let s = value_inputs p s in
  let s = update_value s input (Present in_value) in
  List.fold_left (fun s var ->
      let eq = Variable.Map.find var p.val_eqs in
      let value = compute_value s cond_state eq in
      update_value s var value)
    s p.val_order

let find_event_threshold (l : limits) (s : state) (cond_state : cond_state) =
  Variable.Map.fold (fun _ e_thres min_threshold ->
      let thres_values =
        List.filter_map (fun thres ->
            match compute_value s cond_state thres.value with
            | Absent -> None
            | Present v ->
              if Value.is_positive v then Some v
              else if Value.is_negative v then None
              else if thres.edge = Falling then Some Value.one
              else None)
          e_thres
      in
      List.fold_left (fun m t ->
          match m with None -> Some t | Some m -> Some (Value.min m t))
        min_threshold thres_values)
    l None

let compute_action (p : program) (l : limits) (s : state) (action : action) =
  let s = compute_events p s in
  match action with
  | PoolRep (var, value) ->
    let cond_state = Variable.Map.add var true s.events in
    let s, value =
      match find_event_threshold l s cond_state with
      | None -> s, value
      | Some threshold ->
        if Value.lt threshold value then
          push_rep_action p s var (Value.sub value threshold),
          threshold
        else s, value
    in
    compute_values p s cond_state var value

let compute_queue (p : program) (l : limits) (s : state) =
  let rec aux outs s =
    match s.queue with
    | [] -> s, List.rev outs
    | action::queue ->
      let s = compute_action p l { s with queue } action in
      let step = { step_valuations = s.valuations; step_events = s.events } in
      aux (step::outs) s
  in
  aux [] s

let compute_input_value (p : program) (l : limits) (s : state) (i : Variable.t)
    (value : Literal.t) =
  let s = push_rep_action p s i (literal_value value) in
  compute_queue p l s

let init_state (p : program) =
  let init_map map init_val =
    Variable.Map.filter_map
      (fun _ { eq_act; _ } ->
         if Condition.is_always eq_act then Some init_val else None)
      map
  in
  {
    events = init_map p.act_eqs false;
    valuations = init_map p.val_eqs (Present Value.zero);
    queue = [];
  }

let compute_input_lines (p : program) (l : limits) (lines : computation_inputs) =
  let s = init_state p in
  let _s, output_lines =
    InputLineMap.fold (fun i line (s, outputs) ->
        let s, output =
          compute_input_value p l s line.input_variable line.input_value
        in
        s, InputLineMap.add i output outputs)
      lines (s, InputLineMap.empty)
  in
  output_lines
