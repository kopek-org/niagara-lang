type[@unboxed] t =
  | VRat of R.t

let zero = VRat R.zero

let one = VRat R.one

let is_zero (v : t) =
  match v with
  | VRat r -> R.(equal zero r)

let is_positive (v : t) =
  match v with
  | VRat r -> R.(r > zero)

let is_negative (v : t) =
  match v with
  | VRat r -> R.(r < zero)

let add (v1 : t) (v2 : t) =
  match v1, v2 with
  | VRat r1, VRat r2 -> VRat R.(r1 + r2)

let sub (v1 : t) (v2 : t) =
  match v1, v2 with
  | VRat r1, VRat r2 -> VRat R.(r1 - r2)

let minus (v : t) =
  match v with
  | VRat r -> VRat R.(~- r)

let mult (v1 : t) (v2 : t) =
  match v1, v2 with
  | VRat r1, VRat r2 -> VRat R.(r1 * r2)

let div (v1 : t) (v2 : t) =
  match v1, v2 with
  | VRat r1, VRat r2 -> VRat R.(r1 / r2)

let inv (v : t) =
  match v with
  | VRat r -> VRat R.(inv r)

let min (v1 : t) (v2 : t) =
  match v1, v2 with
  | VRat r1, VRat r2 -> VRat R.(min r1 r2)

let lt (v1 : t) (v2 : t) =
  match v1, v2 with
  | VRat r1, VRat r2 -> R.(r1 < r2)

type discrete_policy =
  | Round
  | Ceil
  | StrictIncrement

let discretise ~(mode : discrete_policy) (v : t) =
  match mode, v with
  | Round, VRat r ->
    VRat R.(round r)
  | Ceil, VRat r ->
    VRat R.(ceil r)
  | StrictIncrement, VRat r ->
    let ceiled = R.ceil r in
    let i =
      if R.(ceiled = r) then R.(ceiled + ~$1)
      else ceiled
    in
    VRat i

let print fmt (v : t) =
  match v with
  | VRat r -> R.pp_print fmt r

let human_print fmt (v : t) =
  match v with
  | VRat r -> R.print_dec_approx fmt r

