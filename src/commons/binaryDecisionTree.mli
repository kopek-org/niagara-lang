module type S = sig
  type condition

  module KnowledgeMap : Map.S with type key = condition

  type 'a t =
    | Decision of condition * 'a t * 'a t
    (** [Decision (c, yes, no)] represents the decision node where [yes] is
        the subdiagram when [c] is satisfied and [no] the one when its
        not. *)
    | Action of 'a
    | NoAction

  type knowledge = bool KnowledgeMap.t

  (** test for contradiction between knowledge bases *)
  val contradictory_knowledge : knowledge -> knowledge -> bool

  (** BDT with no actions *)
  val empty : 'a t

  (** [add_action k f bdt] returns [bdt] where leaves satisfying [k] are updated
      into an action following [f]. If [k] contains decisions that does not
      already appears in the path, they will be added where necessary. *)
  val add_action : knowledge -> ('a option -> 'a option) -> 'a t -> 'a t

  (** [find k bdt] returns the leaf of the tree that match the given
      knowledge [k]. Returns [None] when there is no action, [Some a] when
      there is one, and will fail in case [k] doesn't contain enough
      information to fully make a decision. *)
  val find : knowledge -> 'a t -> 'a option

  (** [fold bdt ~noaction ~action ~decision] returns the result of the
      recursive calls of the three labeled function on the tree nodes in
      bottom-up order.

      These function have as first argument the accumulated knowledge on the
      path to their applied node. *)
  val fold :
    'a t ->
    noaction:(knowledge -> 'b) ->
    action:(knowledge -> 'a -> 'b) ->
    decision:(knowledge -> condition -> 'b -> 'b -> 'b) -> 'b

  (** [cut k bdt] returns a BDT corresponding to [bdt] where every decision
      node whose condition exists in [k] have been shortcuted to match [k] *)
  val cut : knowledge -> 'a t -> 'a t

  (** [only_when k bdt] returns [bdt] down the path described by [k]. *)
  val only_when : knowledge -> 'a t -> 'a t

  (** [merge f bdt1 bdt2] returns a BDT that respects the decision patterns
      of [bdt1] and [bdt2] with the leaves resulting from the merge [f k' l1
      l2] where [l1] and [l2] are the corresponding leaves of [bdt1] and
      [bdt2] resp. and [k'] is the combinated knowledge on the path of [l1]
      and [l2] *)
  val merge :
    (knowledge -> 'a option -> 'b option -> 'c option) ->
    'a t -> 'b t -> 'c t
end

module Make :
  functor (Cond : Map.OrderedType)
    (KnowledgeMap : Map.S with type key = Cond.t)
    -> S with type condition = Cond.t and module KnowledgeMap = KnowledgeMap
