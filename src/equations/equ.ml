type activation = ACT
type value = VAL

type _ expr =
  | EVar of Variable.t
  | ESelf
  | EAlways : activation expr
  | ENot : activation expr -> activation expr
  | EAnd : activation expr * activation expr -> activation expr
  | EGe : value expr * value expr -> activation expr
  | EConst : Literal.t -> value expr
  | EAdd : value expr * value expr -> value expr
  | EMult : value expr * value expr -> value expr
  | ENeg : value expr -> value expr
  | EInv : value expr -> value expr

type 'a aff =
  | ELast of Variable.t
  | EMerge of Variable.t list
  | EExpr of 'a expr

type guarded_eq = {
  eq_act : Condition.t;
  eq_aff : value aff;
}

type event_eqs = activation aff Variable.Map.t

type aggregate_eqs = guarded_eq list Variable.Map.t

type program = {
  val_eqs : guarded_eq Variable.Map.t;
  act_eqs : event_eqs;
}
