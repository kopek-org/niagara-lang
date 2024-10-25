type t

val never : t
val always : t
val of_input : Variable.t -> t
val of_event : Variable.t -> bool -> t
val conj : t -> t -> t
val disj : t -> t -> t
val excluded : t -> t -> t
val is_never : t -> bool
val is_always : t -> bool
val print : Format.formatter -> t -> unit

type satisfaction =
  | Sat
  | Unsat
  | Unknown of Variable.t

val satisfies : bool Variable.Map.t -> t -> satisfaction
