
type op_kind =
  | Quotepart of R.t
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
  | OpposingVariant of { target : Variable.t; origin : Variable.t; variant : origin }

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

val is_input : t -> bool
val is_partner : t -> bool
val is_event : t -> bool

(** Returns the name of the partner if this is a receiving partner (i.e. a
    partner of the original project), otherwise returns [None]. *)
val is_original_partner : t -> string option

val get_name : collection -> Variable.t -> string option
val get_any_name : collection -> Variable.t -> string

val print : Format.formatter -> t -> unit
