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

module Map : Map.S with type key = t

module Set : Set.S with type elt = t

(** Binary decision tree whose branches are based on variables *)
module BDT : BinaryDecisionTree.S with type condition = t and module KnowledgeMap = Map

(** Directed graph whose nodes are variables *)
module Graph : Graph.Sig.P with type V.t = t
