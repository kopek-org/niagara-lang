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
    Report.raise_internal_error "Unable to find valuation for var %d"
      (Variable.uid v)
  | Some (Present v) -> v

let rec find_valuation_in (s : state) (vs : Variable.t list) =
  match vs with
  | [] -> Report.raise_internal_error "Unable to find a valuation in vars"
  | v::vs ->
    match Variable.Map.find_opt v s.valuations with
    | Some (Present v) -> v
    | None | Some Absent -> find_valuation_in s vs

let find_truth (s : state) (v : Variable.t) =
  match Variable.Map.find_opt v s.events with
  | None ->
    Report.raise_internal_error "Unable to find truth for event %d"
      (Variable.uid v)
  | Some b -> b

let rec find_truth_in (s : state) (vs : Variable.t list) =
  match vs with
  | [] -> Report.raise_internal_error "Unable to find a truth in events"
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

module InputLineMap = IntMap
(* any uid linking input and output lines would do *)

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
      p.infos.var_info s.valuations
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
    Report.raise_internal_error "Tried to valuate@ %a,@, which it not a value"
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
    Report.raise_internal_error "Tried to check@ %a,@, which is not an event"
      FormatEqu.print_expr e

let compute_events (p : program) (s : state) =
  Array.fold_left (fun s evt ->
      let { eq_act = _; eq_expr } = Variable.Map.find evt p.act_eqs in
      let b = check s eq_expr in
      update_event s evt b)
    s p.act_order

let compute_value (s : state) (cond_state : cond_state)
    ({ eq_act; eq_expr } : guarded_eq) =
  match Condition.satisfies cond_state eq_act with
  | MaySat ->
    Report.raise_internal_error "Unable to check activation, missing var value"
  | Unsat -> Absent
  | Sat -> Present (valuate s eq_expr)

let compute_values (p : program) (s : state) (cond_state : cond_state) =
  Array.fold_left (fun s var ->
      let eq = Variable.Map.find var p.val_eqs in
      let value = compute_value s cond_state eq in
      update_value s var value)
    s p.val_order

type linear = Compiler.Limits.linear = {
  factor : (Variable.t * expr) option;
  const : expr option;
}

let rec expr_threshold_linear (p : program) (cond_state : cond_state) (expr : expr) =
  match expr with
  | EConst _ | EPre _ -> { factor = None; const = Some expr }
  | EVar v ->
    begin
      match Variable.Map.find_opt v p.val_eqs with
      | None -> (* Should be input *)
        { factor = Some (v, EConst (LRational R.one)); const = None }
      | Some { eq_expr; eq_act = _ } ->
        expr_threshold_linear p cond_state eq_expr
    end
  | ENeg expr ->
    let { factor; const } = expr_threshold_linear p cond_state expr in
    { factor = Option.map (fun (v, e) -> v, ENeg e) factor;
      const = Option.map (fun e -> ENeg e) const;
    }
  | EInv expr ->
    let { factor; const } = expr_threshold_linear p cond_state expr in
    { factor = Option.map (fun (v, e) -> v, EInv e) factor;
      const = Option.map (fun e -> EInv e) const;
    }
  | EAdd (e1, e2) ->
    let l1 = expr_threshold_linear p cond_state e1 in
    let l2 = expr_threshold_linear p cond_state e2 in
    let factor =
      match l1.factor, l2.factor with
      | f, None | None, f -> f
      | Some (v1,f1), Some (v2,f2) ->
        if not (Variable.equal v1 v2) then
          (Format.eprintf "two var factors for %a@." FormatEqu.print_expr expr;
           assert false)
        else
        Some (v1, EAdd (f1, f2))
    in
    let const =
      match l1.const, l2.const with
      | c, None | None, c -> c
      | Some c1, Some c2 -> Some (EAdd (c1,c2))
    in
    { factor; const }
  | EMult (e1, e2) ->
    let l1 = expr_threshold_linear p cond_state e1 in
    let l2 = expr_threshold_linear p cond_state e2 in
    let factor =
      match l1.factor, l2.factor with
      | Some _, Some _ ->
        Report.raise_error "Non-linear values are not allowed"
      | None, None -> None
      | Some (v,f), None ->
        (match l2.const with None -> Some (v,f) | Some c -> Some (v, EMult(c,f)))
      | None, Some (v,f) ->
        (match l1.const with None -> Some (v,f) | Some c -> Some (v, EMult(c,f)))
    in
    let const =
      match l1.const, l2.const with
      | _, None | None, _ -> None
      | Some c1, Some c2 -> Some (EMult (c1,c2))
    in
    { factor; const }
  | EMerge vl ->
    let le =
      List.find_map (fun v ->
          match Variable.Map.find_opt v p.val_eqs with
        | None -> (* Should be input *)
          Some { factor = Some (v, expr); const = None }
        | Some { eq_expr; eq_act } ->
          match Condition.satisfies cond_state eq_act with
          | Sat -> Some (expr_threshold_linear p cond_state eq_expr)
          | Unsat -> None
          | MaySat -> assert false)
        vl
    in
    begin match le with
    | Some le -> le
    | None -> assert false (* should be unreachable *)
    end
  | ENot _ | EAnd (_, _) | EGe (_, _) -> assert false

let rec dyn_event_thresholds (p : program) (cond_state : cond_state) (evt_expr : expr) =
  match evt_expr with
  | EConst _ | ENeg _ | EInv _ | EAdd _ | EMult _ | EMerge [] -> assert false
  | EVar _ | EPre _ | EMerge _ -> []
  | ENot expr -> dyn_event_thresholds p cond_state expr
  | EAnd (e1, e2) ->
    dyn_event_thresholds p cond_state e1
    @ dyn_event_thresholds p cond_state e2
  | EGe (e1, e2) ->
    let l1 = expr_threshold_linear p cond_state e1 in
    let l2 = expr_threshold_linear p cond_state e2 in
    Option.to_list @@
    Compiler.Limits.threshold_of_linears Condition.always l1 l2

let compute_dyn_threshold (p : program) (cond_state : cond_state) (evt : Variable.t) =
  match Variable.Map.find_opt evt p.act_eqs with
  | None -> []
  | Some { eq_act; eq_expr } ->
    match Condition.satisfies cond_state eq_act with
    | Unsat -> []
    | MaySat -> assert false
    | Sat -> dyn_event_thresholds p cond_state eq_expr

let find_event_threshold (p : program) (l : limits) (s : state) (cond_state : cond_state) =
  Variable.Map.fold (fun evt e_thres min_threshold ->
      let thres_l =
        match e_thres with
        | Dynamic -> compute_dyn_threshold p cond_state evt
        | Static thres_l -> thres_l
      in
      let thres_values =
        List.filter_map (fun thres ->
            match compute_value s cond_state thres.value with
            | Absent -> None
            | Present v ->
              if Value.is_positive v then Some v
              else if Value.is_negative v then None
              else if thres.edge = Falling then Some Value.one
              else None)
          thres_l
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
      match find_event_threshold p l s cond_state with
      | None -> s, value
      | Some threshold ->
        if Value.lt threshold value then
          push_rep_action p s var (Value.sub value threshold),
          threshold
        else s, value
    in
    let s = value_inputs p s in
    let s = update_value s var (Present value) in
    compute_values p s cond_state

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

let init_state (p : program) (init : Initialization.t) =
  let init_map map init_val =
    Variable.Map.filter_map
      (fun v { eq_act; _ } ->
         if Condition.is_always eq_act then Some (init_val v) else None)
      map
  in
  let const_init vals =
    Variable.Map.fold (fun c l vals ->
        Variable.Map.add c (Present (literal_value l)) vals)
      p.infos.constants vals
  in
  let val_init =
    let f =
      match init with
      | Zeros -> fun _ -> Present Value.zero
      | FromValues vals ->
        fun v ->
          match Variable.Map.find_opt v vals with
          | None -> Present Value.zero
          | Some l -> Present (literal_value l)
    in
    init_map p.val_eqs f
  in
  let s =
    {
      (* Setting events to passed is a hack to avoid Raising event to
         trigger at the very first step. Works because the first thing
         computed at each steps are events. *)
      events = init_map p.act_eqs (fun _ -> true);
      valuations = const_init val_init;
      queue = [];
    }
  in
  (* compute once to propagate init values, without triggering when conditions *)
  compute_values p s (Variable.Map.map (fun _ -> false) s.events)

let compute_input_lines (p : program) (l : limits) (init : Initialization.t) (lines : Input.t) =
  let s = init_state p init in
  let _s, output_lines =
    InputLineMap.fold (fun i line (s, outputs) ->
        let s, output =
          compute_input_value p l s line.Input.input_variable line.Input.input_value
        in
        s, InputLineMap.add i output outputs)
      lines (s, InputLineMap.empty)
  in
  output_lines
