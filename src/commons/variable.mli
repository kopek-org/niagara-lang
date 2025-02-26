(** Variable representation across the compiler *)
type t

(** Generate a fresh variable *)
val create : unit -> t

(** [uid v] returns the unique identifier of the variable [v] *)
val uid : t -> int

(** [unique_anon_name s] returns a unique variable name with prefix [s] *)
val unique_anon_name : string -> string

val compare : t -> t -> int
val equal : t -> t -> bool

module Map : Map.S with type key = t

module Set : Set.S with type elt = t

(** Directed graph whose nodes are variables and edges events conditionning the link *)
module Graph : sig
  include Graph.Sig.P
  with type V.t = t
   and type E.label = Set.t
   and type E.t = t * Set.t * t

  module Topology : sig
    val scc : t -> int * (vertex -> int)
    val scc_array : t -> vertex list array
    val scc_list : t -> vertex list list
  end

  (** transitive closure keeping as edges all events on the paths
      between two nodes *)
  val transitive_closure : t -> t

end
