module Make :
  functor (Cond : Map.OrderedType)
    (KnowledgeMap : Map.S with type key = Cond.t)
    -> sig
      type condition = Cond.t

      type 'a t =
        | Decision of Cond.t * 'a t * 'a t
        (** [Decision (c, yes, no)] represents the decision node where [yes] is
            the subdiagram when [c] is satisfied and [no] the one when its
            not. *)
        | Action of 'a
        | NoAction

      type knowledge = bool KnowledgeMap.t

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
        decision:(knowledge -> Cond.t -> 'b -> 'b -> 'b) -> 'b

      (** [add_decision c bdt] returns a BDT that represents the same decisions
          as [bdt] with the addition of [c] everywhere this decision didn't
          already existed. The existing actions are preserved regarding the
          decision of [bdt]. *)
      val add_decision : Cond.t -> 'a t -> 'a t

      (** [map_action k f bdt] returns a [bdt] where every leaves [l] satisfying
          [k] are replaced with the subdiagram [f k' l] where [k'] is the
          accumulated knowlegde on the path to [l]. *)
      val map_action :
        knowledge -> (knowledge -> 'a option -> 'a t) -> 'a t -> 'a t

      (** [cut k bdt] returns a BDT corresponding to [bdt] where every decision
          node whose condition exists in [k] have been shortcuted to match [k] *)
      val cut : knowledge -> 'a t -> 'a t

      (** [merge f bdt1 bdt2] returns a BDT that respects the decision patterns
          of [bdt1] and [bdt2] with the leaves resulting from the merge [f k' l1
          l2] where [l1] and [l2] are the corresponding leaves of [bdt1] and
          [bdt2] resp. and [k'] is the combinated knowledge on the path of [l1]
          and [l2] *)
      val merge :
        (knowledge -> 'a option -> 'a option -> 'b option) ->
        'a t -> 'a t -> 'b t
    end
