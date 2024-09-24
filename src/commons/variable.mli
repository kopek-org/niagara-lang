(** Variable representation across the compiler *)
type t

(** Useful properties of variables *)
(* Not sure if it would be better to regroup all infos
   in here instead of specific tables *)
type info = { var_name : string; }

(** Generate a fresh variable *)
val create : unit -> t

(** [uid v] returns the unique identifier of the variable [v] *)
val uid : t -> int

(** [unique_anon_name s] returns a unique variable name with prefix [s] *)
val unique_anon_name : string -> string

val equal : t -> t -> bool

module Map : Map.S with type key = t

module Set : Set.S with type elt = t

(** Binary decision tree whose branches are based on variables *)
module BDT : BinaryDecisionTree.S with type condition = t and module KnowledgeMap = Map

(** Directed graph whose nodes are variables and edges event conditions *)
module Graph : sig
  include Graph.Sig.P
  with type V.t = t
   and type E.label = bool Map.t
   and type E.t = t * bool Map.t * t

  module Topology : sig
    val scc : t -> int * (vertex -> int)
    val scc_array : t -> vertex list array
    val scc_list : t -> vertex list list
  end

end

module Info : sig

  type origin =
    | Named of string
    | LabelOfPartner of { partner : t; label : string }
    | Cumulative of t
    | AnonEvent
    | Peeking of t
    | RisingEvent of t
    | ContextSpecialized of { origin : t; context : Context.Group.t }
    | OperationDetail of { source : t; target : t }
    | ExistentialAggreg of t list

  type kind =
    | ReceivingPartner
    | ProvidingPartner
    | ParameterInput
    | PoolInput
    | Intermediary
    | Event
    | Constant

  type t = {
    origin : origin;
    typ : ValueType.t;
    kind : kind;
  }

  type collection = t Map.t

  val print : Format.formatter -> t -> unit

end
