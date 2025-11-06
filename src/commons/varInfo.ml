
type op_kind =
  | Quotepart of R.t
  | Bonus of Variable.Set.t
  | Default of Condition.t R.Map.t
  | Deficit of Condition.t R.Map.t

let op_kind_encoding =
  let open Json_encoding in
  union
    [
      case
        (tup2 (constant "quotepart") R.encoding)
        (function
          | Quotepart q -> Some ((), q)
          | Bonus _ | Default _ | Deficit _ -> None)
        (fun ((), q) -> Quotepart q);
      case
        (tup2 (constant "bonus") Variable.Set.encoding)
        (function Bonus b -> Some ((), b) | _ -> None)
        (fun ((), b) -> Bonus b);
      case
        (tup2
           (string_enum ["default",true;"deficit",false])
           (R.Map.encoding Condition.encoding))
        (function Default m -> Some (true, m)
                | Deficit m -> Some (false, m)
                | _ -> None)
        (fun (b, m) -> if b then Default m else Deficit m);
    ]

type event_loc =
  | NoEvent
  | Before of Variable.t
  | After of Variable.t
  | When of Variable.t

let event_loc_encoding =
  let open Json_encoding in
  union
    [
      case null
        (function NoEvent -> Some () | Before _ | After _ -> None)
        (fun () -> NoEvent);
      case
        (tup2 (string_enum ["before",true;"after",false]) Variable.encoding)
        (function Before v -> Some (true, v)
                | After v -> Some (false, v)
                | NoEvent -> None)
        (fun (b, v) -> if b then Before v else After v)
    ]

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
  | LocalValuation of {
      target : Variable.t;
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

let origin_encoding =
  let open Json_encoding in
  mu "origin" (fun self ->
      union
        [
          case
            (obj1
               (req "case" (string_enum ["anonymous event",0;"existential",1])))
            (function AnonEvent -> Some 0
                    | ConditionExistential -> Some 1
                    | _ -> None)
            (fun i -> if i = 0 then AnonEvent else ConditionExistential);
          case
            (obj2
               (req "case"
                  (string_enum ["cumulative", 0;
                                "peeking", 1;
                                "rising event", 2;
                                "deficit", 3;
                                "repartition", 4;
                                "pool", 5;
                                "delta", 6]))
               (req "value" Variable.encoding))
            (function Cumulative n -> Some (0, n)
                    | Peeking n -> Some (1,n)
                    | RisingEvent n -> Some (2,n)
                    | DeficitSum n -> Some (3,n)
                    | RepartitionSum n -> Some (4,n)
                    | PoolStage n -> Some (5,n)
                    | OppositionDelta { target } -> Some (6,target)
                    | _ -> None)
            (fun (i, n) ->
               if i = 0 then Cumulative n else
               if i = 1 then Peeking n else
               if i = 2 then RisingEvent n else
               if i = 3 then DeficitSum n else
               if i = 4 then RepartitionSum n else
               if i = 5 then PoolStage n else
                 OppositionDelta {target = n});
          case
            (obj2 (req "case" (constant "named")) (req "value" string))
            (function Named n -> Some ((), n) | _ -> None)
            (fun ((), n) -> Named n);
          case
            (obj3
               (req "case" (constant "label"))
               (req "partner" Variable.encoding)
               (req "label" string))
            (function
              | LabelOfPartner { partner; label } -> Some ((), partner, label)
              | _ -> None)
            (fun ((), partner, label) -> LabelOfPartner { label; partner });
          case
            (obj3
               (req "case" (constant "contextualized"))
               (req "origin" Variable.encoding)
               (req "context" Context.Group.encoding))
            (function
              | ContextSpecialized { origin; context } ->
                  Some ((), origin, context)
              | _ -> None)
            (fun ((), origin, context) ->
              ContextSpecialized { origin; context });
          case
            (obj6
               (req "case" (constant "operation"))
               (opt "label" string)
               (req "class" op_kind_encoding)
               (req "condition" event_loc_encoding)
               (req "source" Variable.encoding)
               (req "target" Variable.encoding))
            (function
              | OperationDetail { label; op_kind; condition; source; target } ->
                  Some ((), label, op_kind, condition, source, target)
              | _ -> None)
            (fun ((), label, op_kind, condition, source, target) ->
              OperationDetail { label; op_kind; condition; source; target });
          case
            (obj6
               (req "case" (constant "trigger"))
               (opt "label" string)
               (req "trigger" Variable.encoding)
               (req "vars" Variable.Set.encoding)
               (req "source" Variable.encoding)
               (req "target" Variable.encoding))
            (function
              | TriggerOperation
                  { label; trigger; trigger_vars; source; target } ->
                  Some ((), label, trigger, trigger_vars, source, target)
              | _ -> None)
            (fun ((), label, trigger, trigger_vars, source, target) ->
              TriggerOperation { label; trigger; trigger_vars; source; target });
          case
            (obj4
               (req "case" (constant "valuation"))
               (req "target" Variable.encoding)
               (opt "trigger" Variable.encoding)
               (req "deps" Variable.Set.encoding))
            (function
              | LocalValuation { target; trigger; deps } ->
                  Some ((), target, trigger, deps)
              | _ -> None)
            (fun ((), target, trigger, deps) ->
              LocalValuation { target; trigger; deps });
          case
            (obj3
               (req "case" (constant "sum"))
               (req "source" Variable.encoding)
               (req "target" Variable.encoding))
            (function
              | OperationSum { source; target } -> Some ((), source, target)
              | _ -> None)
            (fun ((), source, target) -> OperationSum { source; target });
          case
            (obj3
               (req "case" (constant "staged"))
               (req "repartition" Variable.encoding)
               (req "stage" Condition.encoding))
            (function
              | StagedRepartition { rep; stage } -> Some ((), rep, stage)
              | _ -> None)
            (fun ((), rep, stage) -> StagedRepartition { rep; stage });
          case
            (obj4
               (req "case" (constant "variant"))
               (req "target" Variable.encoding)
               (req "origin" Variable.encoding)
               (req "variant" self))
            (function
              | OpposingVariant { target; origin; variant } ->
                  Some ((), target, origin, variant)
              | _ -> None)
            (fun ((), target, origin, variant) ->
              OpposingVariant { target; origin; variant });
        ])

type partner_role = Provider | Receiver

let partner_role_encoding =
  let open Json_encoding in
  string_enum [ ("provider", Provider); ("receiver", Receiver) ]

type kind =
  | Partner of partner_role
  | ParameterInput of { shadow : bool }
  | PoolInput of { shadow : bool }
  | Intermediary
  | Computed
  | Value of { observable : bool; cumulative : bool }
  | Event
  | Constant

let kind_encoding =
  let open Json_encoding in
  union
    [
      case (string_enum ["intermediary",0;"computed",1;"event",2;"constant",3])
        (function Intermediary -> Some 0
                | Computed -> Some 1
                | Event -> Some 2
                | Constant -> Some 3
                | _ -> None)
        (fun i ->
           if i  = 0 then Intermediary else
           if i  = 1 then Computed else
           if i = 2 then Event else
             Constant);
      case
        (tup2 (constant "partner") partner_role_encoding)
        (function Partner p -> Some ((), p) | _ -> None)
        (fun ((), p) -> Partner p);
      case
        (tup2 (string_enum ["parameter_input", true;"pool_input", false]) bool)
        (function ParameterInput { shadow } -> Some (true, shadow)
                | PoolInput { shadow } -> Some (false, shadow)
                | _ -> None)
        (fun (b, shadow) ->
           if b then ParameterInput { shadow } else PoolInput { shadow });
      case
        (tup2 (constant "value")
           (obj2 (req "observable" bool) (req "cumulative" bool)))
        (function
          | Value { observable; cumulative } ->
              Some ((), (observable, cumulative))
          | _ -> None)
        (fun ((), (observable, cumulative)) -> Value { observable; cumulative });
    ]

type t = {
  origin : origin;
  typ : ValueType.t;
  kind : kind;
}

let encoding =
  let open Json_encoding in
  conv
    (fun { origin; typ; kind } -> (origin, (typ, kind)))
    (fun (origin, (typ, kind)) -> { origin; typ; kind })
    (merge_objs
        origin_encoding
        (obj2
           (req "type" ValueType.encoding)
           (req "kind" kind_encoding)))

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
  | LocalValuation { target; deps = _ } ->
    fprintf fmt "[=%d]" (Variable.uid target)
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
