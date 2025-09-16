
type op_kind =
  | Quotepart of R.t
  | Bonus of Variable.Set.t
  | Default of Condition.t R.Map.t
  | Deficit of Condition.t R.Map.t

type event_loc =
  | NoEvent
  | Before of Variable.t
  | After of Variable.t

type origin =
  | Named of string
  | LabelOfPartner of { partner : Variable.t; label : string }
  | Cumulative of Variable.t
  | AnonEvent
  | Peeking of Variable.t
  | RisingEvent of Variable.t
  | ContextSpecialized of { origin : Variable.t; context : Context.Group.t }
  | OperationDetail of {
      label : string option;
      op_kind : op_kind;
      condition : event_loc;
      source : Variable.t;
      target : Variable.t
    }
  | TriggerOperation of {
      source : Variable.t;
      target : Variable.t;
      label : string option;
      trigger : Variable.t;
      trigger_vars : Variable.Set.t;
    }
  | LocalValuation of {
      target : Variable.t;
      trigger : Variable.t option;
      deps : Variable.Set.t;
    }
  | OperationSum of { source : Variable.t; target : Variable.t }
  | RepartitionSum of Variable.t
  | DeficitSum of Variable.t
  | StagedRepartition of { rep : Variable.t; stage : Condition.t }
  | PoolStage of Variable.t
  | ConditionExistential
  | OpposingVariant of { target : Variable.t; origin : Variable.t; variant : origin }
  | OppositionDelta of { target : Variable.t }

type partner_role = Provider | Receiver

type kind =
  | Partner of partner_role
  | ParameterInput of { shadow : bool }
  | PoolInput of { shadow : bool }
  | Intermediary
  | Computed
  | Value of { observable : bool; cumulative : bool }
  | Event
  | Constant

type t = {
  origin : origin;
  typ : ValueType.t;
  kind : kind;
}

type collection = t Variable.Map.t

let is_input t =
  match t.kind with
  | ParameterInput { shadow  = false }
  | PoolInput { shadow  = false } -> true
  | _ -> false

let is_partner t =
  match t.kind with
  | Partner _ -> true
  | _ -> false

let is_provider t =
  match t.kind with
  | Partner Provider -> true
  | _ -> false

let is_event t = t.kind = Event

let is_original_partner t =
  match t.kind, t.origin with
  | Partner Receiver, Named s -> Some s
  | _ -> None

let rec get_name coll v =
  match Variable.Map.find_opt v coll with
  | None -> None
  | Some t ->
    match t.origin with
    | Named name -> Some name
    | LabelOfPartner { partner; _ } -> get_name coll partner
    | Cumulative v -> get_name coll v
    | AnonEvent -> None
    | Peeking v -> get_name coll v
    | RisingEvent v -> get_name coll v
    | ContextSpecialized { origin; _ } -> get_name coll origin
    | OperationDetail _ -> None
    | TriggerOperation _ -> None
    | LocalValuation _ -> None
    | OperationSum _ -> None
    | RepartitionSum _ -> None
    | DeficitSum _ -> None
    | StagedRepartition _ -> None
    | PoolStage _ -> None
    | ConditionExistential -> None
    | OpposingVariant { origin; _ } -> get_name coll origin
    | OppositionDelta _ -> None

let print fmt t =
  let open Format in
  match t.origin with
  | Named name -> pp_print_string fmt name
  | LabelOfPartner { partner; label } ->
    fprintf fmt "%d$%s" (Variable.uid partner) label
  | Cumulative v ->
    fprintf fmt "#%d" (Variable.uid v)
  | AnonEvent -> pp_print_string fmt "anon_event"
  | Peeking v -> fprintf fmt "@%d" (Variable.uid v)
  | RisingEvent v -> fprintf fmt "^%d" (Variable.uid v)
  | ContextSpecialized { origin; context } ->
    fprintf fmt "%d(%a)" (Variable.uid origin) Context.Group.print context
  | OperationDetail { label = _; source; target; op_kind; condition = _ } ->
    fprintf fmt "[%d->%d]%s" (Variable.uid source) (Variable.uid target)
      (match op_kind with
       | Quotepart _ -> "%"
       | Bonus _ -> "$"
       | Default _ -> "?"
       | Deficit _ -> "!")
  | TriggerOperation { label = _; source; target; trigger; trigger_vars = _ } ->
    fprintf fmt "[%d->%d]@@%d" (Variable.uid source) (Variable.uid target)
      (Variable.uid trigger)
  | LocalValuation { target; trigger; deps = _ } ->
    fprintf fmt "[=%d]%a" (Variable.uid target)
      (pp_print_option (fun fmt trigger ->
           fprintf fmt"@@%d" (Variable.uid trigger)))
      trigger
  | OperationSum { source; target } ->
    fprintf fmt "[%d->%d]*" (Variable.uid source) (Variable.uid target)
  | RepartitionSum v -> fprintf fmt "%d->*" (Variable.uid v)
  | DeficitSum v -> fprintf fmt "%d->!" (Variable.uid v)
  | StagedRepartition { rep; _ } -> fprintf fmt "~>%d" (Variable.uid rep)
  | PoolStage v -> fprintf fmt "~%d~" (Variable.uid v)
  | ConditionExistential ->
    fprintf fmt "`E"
  | OpposingVariant { target; origin; variant = _ } ->
    fprintf fmt "%d<%d>" (Variable.uid origin) (Variable.uid target)
  | OppositionDelta { target } ->
    fprintf fmt "\u{0394}%d" (Variable.uid target)

let get_any_name coll v =
  match get_name coll v with
  | Some n -> n
  | None -> Format.asprintf "%a" print (Variable.Map.find v coll)
