type activation = ACT
type value = VAL

type _ expr =
  | EVar of Variable.t
  | EPre
  | EAlways : activation expr
  | ENot : activation expr -> activation expr
  | EAnd : activation expr * activation expr -> activation expr
  | EGe : value expr * value expr -> activation expr
  | EConst : Literal.t -> value expr
  | EAdd : value expr * value expr -> value expr
  | EMult : value expr * value expr -> value expr
  | ENeg : value expr -> value expr
  | EInv : value expr -> value expr
  | EMerge of Variable.t list
  | EOrZero of Variable.t

type guarded_eq = {
  eq_act : Condition.t;
  eq_expr : value expr;
}

type event_eqs = activation expr Variable.Map.t

type aggregate_eqs = guarded_eq list Variable.Map.t

type program = {
  val_eqs : guarded_eq Variable.Map.t;
  act_eqs : event_eqs;
}
