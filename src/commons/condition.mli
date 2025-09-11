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

val time_ranking : t list -> t list
(** [time_ranking [c1;...;cn]] returns an ordered list of mutually
    exclusive conditions such that:

    - The disjuction of the result conditions equals the disjunction
    of the input conditions (which are not necessarily exclusives).

    - The output condition at index [i] is strictely "before" at least
    parts of the conditions at index [j] where [j>i].

    "Before" here refers to the timing notion implied in the
    conditions variables. An event variable [e] is true in time
    instant when the event has happened, and false in instant when it
    has not yet (regardless of it happening eventually or not at all).

    The order here is a partial order that can be described as a
    lattice of sets of booleans where [false < true].

    The function will sliced conditions only when necessary. Meaning
    that for example: a condition [c = (e1 && e2) || (!e1 && !e2)],
    which is non-monotonous, will be sliced if ordered with [c' = !e1
    && e2] because [(!e1 && !e2) < c' < (e1 && e2)]. [c'] however,
    will not be sliced by [e1 && !e2], because none of their subsets
    are comparable. *)

val print : Format.formatter -> t -> unit

type satisfaction =
  | Sat
  | Unsat
  | MaySat

val satisfies : bool Variable.Map.t -> t -> satisfaction


type var =
  | Input of Variable.t
  | Event of Variable.t

type tree = private
  | True
  | False
  | Branch of { var : var; yes : tree; no : tree }

val tree : t -> tree

val events_of : t -> Variable.Set.t
