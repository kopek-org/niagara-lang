
type expr =
  | EVar of Variable.t
  | EPre of Variable.t
  | ENot of expr
  | EAnd of expr * expr
  | EGe of expr * expr
  | EConst of Literal.t
  | EAdd of expr * expr
  | EMult of expr * expr
  | ENeg of expr
  | EInv of expr
  | EMerge of Variable.t list

type guarded_eq = {
  eq_act : Condition.t;
  eq_expr : expr;
}

type aggregation =
  | One of guarded_eq
  | More of (Variable.t * Condition.t) list

type aggregate_eqs = aggregation Variable.Map.t

type program = {
  infos : ProgramInfo.t;
  val_eqs : guarded_eq Variable.Map.t;
  val_order : Variable.t list;
  act_eqs : guarded_eq Variable.Map.t;
  act_order : Variable.t list;
}

type edge_way =
  | Raising
  | Falling

type threshold = {
  var : Variable.t;
  edge : edge_way;
  value : guarded_eq;
}

type limits = threshold list Variable.Map.t
