
type op_kind =
  | Quotepart
  | Bonus
  | Default of Condition.t R.Map.t
  | Deficit of Condition.t R.Map.t

type origin =
  | Named of string
  | LabelOfPartner of { partner : Variable.t; label : string }
  | Cumulative of Variable.t
  | AnonEvent
  | Peeking of Variable.t
  | RisingEvent of Variable.t
  | ContextSpecialized of { origin : Variable.t; context : Context.Group.t }
  | OperationDetail of { op_kind : op_kind; source : Variable.t; target : Variable.t }
  | OperationSum of { source : Variable.t; target : Variable.t }
  | RepartitionSum of Variable.t
  | DeficitSum of Variable.t
  | ConditionExistential

type kind =
  | ReceivingPartner
  | ProvidingPartner
  | ParameterInput of { shadow : bool }
  | PoolInput of { shadow : bool }
  | Intermediary
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
  | ReceivingPartner
  | ProvidingPartner -> true
  | _ -> false

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
    | OperationSum _ -> None
    | RepartitionSum _ -> None
    | DeficitSum _ -> None
    | ConditionExistential -> None

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
  | OperationDetail { source; target; op_kind } ->
    fprintf fmt "[%d->%d]%s" (Variable.uid source) (Variable.uid target)
      (match op_kind with
       | Quotepart -> "%"
       | Bonus -> "$"
       | Default _ -> "?"
       | Deficit _ -> "!")
  | OperationSum { source; target } ->
    fprintf fmt "[%d->%d]*" (Variable.uid source) (Variable.uid target)
  | RepartitionSum v -> fprintf fmt "%d->*" (Variable.uid v)
  | DeficitSum v -> fprintf fmt "%d->!" (Variable.uid v)
  | ConditionExistential ->
    fprintf fmt "`E"

let get_any_name coll v =
  match get_name coll v with
  | Some n -> n
  | None -> Format.asprintf "%a" print (Variable.Map.find v coll)
