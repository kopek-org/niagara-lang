(** Variable representation across the compiler *)
type t

val encoding : t Json_encoding.encoding

(** Generate a fresh variable *)
val create : unit -> t

(** [uid v] returns the unique identifier of the variable [v] *)
val uid : t -> int

(** [unique_anon_name s] returns a unique variable name with prefix [s] *)
val unique_anon_name : string -> string

val compare : t -> t -> int
val equal : t -> t -> bool

module Map : sig
  include Map.S with type key = t

  val tup_list_encoding :
    'a Json_encoding.encoding -> 'a t Json_encoding.encoding

  val array_encoding : 'a Json_encoding.encoding -> 'a t Json_encoding.encoding
end

module Set : sig
  include Set.S with type elt = t

  val encoding : t Json_encoding.encoding
end

(** Directed graph whose nodes are variables and edges events conditionning the link *)
module Graph : sig
  include Graph.Sig.P
  with type V.t = t
   and type E.label = Set.t
   and type E.t = t * Set.t * t

  (** Adjacency list representation.
      Edges are couples of their label and the destination *)
  val encoding : t Json_encoding.encoding

  module Topology : sig
    val scc : t -> int * (vertex -> int)
    val scc_array : t -> vertex list array
    val scc_list : t -> vertex list list
  end

  (** set of vertexes reachable from given start vertex *)
  val reachables : t -> V.t -> Set.t

  module DAG : sig
    (** transitive closure keeping as edges all events on the paths
        between two nodes *)
    val transitive_closure : t -> t

    val topological_depth_ordering : t -> V.t list
  end
end
