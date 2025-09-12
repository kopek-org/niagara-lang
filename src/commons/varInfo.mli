
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

val is_input : t -> bool
val is_partner : t -> bool
val is_provider : t -> bool
val is_event : t -> bool

(** Returns the name of the partner if this is a receiving partner (i.e. a
    partner of the original project), otherwise returns [None]. *)
val is_original_partner : t -> string option

val get_name : collection -> Variable.t -> string option
val get_any_name : collection -> Variable.t -> string

val print : Format.formatter -> t -> unit
