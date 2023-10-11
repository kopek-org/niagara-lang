open Internal

type value =
  | VZero
  | VInt of int
  | VRat of float


let typing_error () =
  Errors.raise_error "(internal) Mismatching types during interpretation"

let zero_of (t : ValueType.t) : value =
  match t with
  | ValueType.TInteger | ValueType.TMoney -> VInt 0
  | ValueType.TRational -> VRat 0.
  | ValueType.TEvent
  | ValueType.TDate
  | ValueType.TDuration -> assert false

let zero_of_var (p : Ir.program) (var : Variable.t) : value =
  match Variable.Map.find_opt var p.infos.Surface.Ast.types with
  | None -> Errors.raise_error "(internal) Variable type not found"
  | Some t -> zero_of t

let is_zero (v : value) =
  match v with
  | VZero | VInt 0 | VRat 0. -> true
  | _ -> false

let is_negative (v : value) =
  match v with
  | VZero -> false
  | VInt i -> i < 0
  | VRat r -> r < 0.

let add_value (v1 : value) (v2 : value) =
  match v1, v2 with
  | VZero, v | v, VZero -> v
  | VInt i1, VInt i2 -> VInt (i1 + i2)
  | VInt i, VRat r
  | VRat r, VInt i -> VRat ((float_of_int i) +. r)
  | VRat r1, VRat r2 -> VRat (r1 +. r2)

let minus_value (v : value) =
  match v with
  | VZero -> VZero
  | VInt i -> VInt (-i)
  | VRat r -> VRat (-.r)

let mult_value (v1 : value) (v2 : value) =
  match v1, v2 with
  | VZero, _ | _, VZero -> VZero
  | VInt i1, VInt i2 -> VInt (i1 * i2)
  | VInt i, VRat r
  | VRat r, VInt i -> VRat ((float_of_int i) *. r)
  | VRat r1, VRat r2 -> VRat (r1 *. r2)

let div_value (v1 : value) (v2 : value) =
  match v1, v2 with
  | _, VZero -> Errors.raise_error "Division by zero"
  | VZero, _ -> VZero
  | VInt i1, VInt i2 -> VInt (i1 / i2)
  | VInt i, VRat r
  | VRat r, VInt i -> VRat ((float_of_int i) /. r)
  | VRat r1, VRat r2 -> VRat (r1 /. r2)

let min_value (v1 : value) (v2 : value) =
  match v1, v2 with
  | VZero, VZero -> VZero
  | VZero, VInt i | VInt i, VZero -> VInt (min 0 i)
  | VZero, VRat r | VRat r, VZero -> VRat (min 0. r)
  | VInt i1, VInt i2 -> VInt (min i1 i2)
  | VRat r1, VRat r2 -> VRat (min r1 r2)
  | _ -> typing_error ()

type discrete_policy =
  | Round
  (* | Ceil *)
  | StrictIncrement

let discretise_value ~(mode : discrete_policy) (v : value) =
  match mode, v with
  | Round, (VZero | VInt _) -> v
  | Round, VRat r ->
    VInt (Int.of_float (Float.round r))
  | StrictIncrement, VZero -> VInt 1
  | StrictIncrement, VInt i -> VInt (i+1)
  | StrictIncrement, VRat r ->
    let ceiled = Float.ceil r in
    let i =
      if ceiled = r then (Int.of_float ceiled) + 1
      else Int.of_float ceiled
    in
    VInt i

let cast_value (t : ValueType.t) (v : value) =
  match t, v with
  | _, VZero
  | (TInteger | TMoney), VInt _
  | TRational, VRat _ -> v
  | TRational, VInt i -> VRat (Float.of_int i)
  | (TInteger | TMoney), VRat _ ->
    discretise_value ~mode:Round v
  | _ -> assert false
