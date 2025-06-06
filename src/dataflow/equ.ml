
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
  val_order : Variable.t Array.t;
  act_eqs : guarded_eq Variable.Map.t;
  act_order : Variable.t Array.t;
}

type edge_way =
  | Raising
  | Falling

type static_threshold = {
  var : Variable.t;
  edge : edge_way;
  value : guarded_eq;
}

type threshold =
  | Static of static_threshold list
  | Dynamic

type limits = threshold Variable.Map.t


let vars_of_expr (e : expr) : Variable.Set.t =
  let rec aux acc e =
    match e with
    | EConst _ -> acc
    | EVar v | EPre v -> Variable.Set.add v acc
    | ENot e | ENeg e | EInv e -> aux acc e
    | EAnd (e1, e2) | EGe (e1, e2) | EAdd (e1, e2) | EMult (e1, e2) ->
      aux (aux acc e1) e2
    | EMerge l -> List.fold_left (fun acc v -> aux acc (EVar v)) acc l
  in
  aux Variable.Set.empty e
