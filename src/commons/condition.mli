type t

val never : t
val always : t
val of_input : Variable.t -> t
val of_event : Variable.t -> bool -> t
val conj : t -> t -> t
val disj : t -> t -> t
val print : Format.formatter -> t -> unit
