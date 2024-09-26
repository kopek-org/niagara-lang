type activation = ACT
type value = VAL

type _ expr =
  | EVar of Variable.t
  | EPre of Variable.t
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

type aggregation =
  | One of guarded_eq
  | More of (Variable.t * Condition.t) list

type aggregate_eqs = aggregation Variable.Map.t

type program = {
  infos : Surface.Ast.program_infos;
  val_eqs : guarded_eq Variable.Map.t;
  eqs_order : Variable.t list;
  act_eqs : event_eqs;
}
