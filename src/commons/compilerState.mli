(** Some generation functions uses encapsulated mutable states.
    This module allows a global state reset of the compiler
    without braking encapsulation by provinding a local way
    to react to a reset command.
*)

(** Register function called on reset *)
val register_on_reset : (unit -> unit) -> unit

(** Global reset *)
val reset : unit -> unit
