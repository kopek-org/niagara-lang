
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

val is_input : t -> bool
val is_partner : t -> bool

val print : Format.formatter -> t -> unit
