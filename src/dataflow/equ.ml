
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

let expr_encoding =
  let open Json_encoding in
  mu "equ_expr" (fun self ->
      union
        [
          case
            (tup2 (string_enum ["var",true;"pre",false]) Variable.encoding)
            (function EVar v -> Some (true, v)
                    | EPre v -> Some (false,v)
                    | _ -> None)
            (fun (b, v) -> if b then EVar v else EPre v);
          case
            (tup2 (string_enum ["not",0;"neg",1;"inv",2]) self)
            (function ENot v -> Some (0, v)
                    | ENeg v  -> Some (1,v)
                    | EInv v -> Some (2,v)
                    | _ -> None)
            (fun (i, v) ->
               if i = 0 then ENot v else if i = 1 then ENeg v else EInv v);
          case
            (tup3 (string_enum ["and",0;"ge",1;"add",2;"mult",3]) self self)
            (function EAnd (x, y) -> Some (0, x, y)
                    | EGe (x, y) -> Some (1,x,y)
                    | EAdd (x, y) -> Some (2,x,y)
                    | EMult (x, y) -> Some (3,x,y)
                    | _ -> None)
            (fun (i, x, y) ->
               if i  = 0 then EAnd (x, y) else
               if i = 1 then EGe (x,y) else
               if i = 2 then EAdd (x,y) else
                 EMult (x,y));
          case
            (tup2 (constant "const") Literal.encoding)
            (function EConst v -> Some ((), v) | _ -> None)
            (fun ((), v) -> EConst v);
          case
            (tup2 (constant "merge") (list Variable.encoding))
            (function EMerge l -> Some ((), l) | _ -> None)
            (fun ((), l) -> EMerge l);
        ])

type guarded_eq = { eq_act : Condition.t; eq_expr : expr }

let guarded_eq_encoding =
  let open Json_encoding in
  conv
    (fun { eq_act; eq_expr } -> (eq_act, eq_expr))
    (fun (eq_act, eq_expr) -> { eq_act; eq_expr })
    (obj2 (req "act" Condition.encoding) (req "expr" expr_encoding))

type aggregation = One of guarded_eq | More of (Variable.t * Condition.t) list

let aggregation_encoding =
  let open Json_encoding in
  union
    [
      case guarded_eq_encoding
        (function One x -> Some x | More _ -> None)
        (fun x -> One x);
      case
        (list (tup2 Variable.encoding Condition.encoding))
        (function More x -> Some x | One _ -> None)
        (fun x -> More x);
    ]

type aggregate_eqs = aggregation Variable.Map.t

type program = {
  infos : ProgramInfo.t;
  val_eqs : guarded_eq Variable.Map.t;
  val_order : Variable.t Array.t;
  act_eqs : guarded_eq Variable.Map.t;
  act_order : Variable.t Array.t;
}

let program_encoding =
  let open Json_encoding in
  conv
    (fun { infos; val_eqs; val_order; act_eqs; act_order } ->
      (infos, val_eqs, val_order, act_eqs, act_order))
    (fun (infos, val_eqs, val_order, act_eqs, act_order) ->
      { infos; val_eqs; val_order; act_eqs; act_order })
    (obj5
       (req "infos" ProgramInfo.encoding)
       (req "val_eqs" (Variable.Map.array_encoding guarded_eq_encoding))
       (req "val_order" (array Variable.encoding))
       (req "act_eqs" (Variable.Map.tup_list_encoding guarded_eq_encoding))
       (req "act_order" (array Variable.encoding)))

type edge_way = Raising | Falling

let edge_way_encoding =
  Json_encoding.string_enum [ ("Raising", Raising); ("Falling", Falling) ]

type static_threshold = {
  var : Variable.t;
  edge : edge_way;
  value : guarded_eq;
}

let static_treshold_encoding =
  let open Json_encoding in
  conv
    (fun { var; edge; value } -> (var, edge, value))
    (fun (var, edge, value) -> { var; edge; value })
    (obj3
       (req "var" Variable.encoding)
       (req "edge" edge_way_encoding)
       (req "value" guarded_eq_encoding))

type threshold = Static of static_threshold list | Dynamic

let threshold_encoding =
  let open Json_encoding in
  union
    [
      case (constant "dynamic")
        (function Dynamic -> Some () | Static _ -> None)
        (fun () -> Dynamic);
      case
        (list static_treshold_encoding)
        (function Static s -> Some s | Dynamic -> None)
        (fun s -> Static s);
    ]

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
