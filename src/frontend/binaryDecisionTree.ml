
module Make(Cond : Map.OrderedType)(KnowledgeMap : Map.S with type key = Cond.t) = struct

  type condition = Cond.t

  type 'a t =
    | Decision of Cond.t * 'a t * 'a t
    | Action of 'a
    | NoAction

  type knowledge = bool KnowledgeMap.t

  let fold (t : 'a t)
      ~(noaction : knowledge -> 'b)
      ~(action : knowledge -> 'a -> 'b )
      ~(decision : knowledge -> Cond.t -> 'b -> 'b -> 'b) =
    let rec aux knowledge t =
      match t with
      | NoAction -> noaction knowledge
      | Action a -> action knowledge a
      | Decision (cond, yes, no) ->
        decision knowledge cond
          (aux (KnowledgeMap.add cond true knowledge) yes)
          (aux (KnowledgeMap.add cond false knowledge) no)
    in
    aux KnowledgeMap.empty t

  let rec add_decision (cond : Cond.t) (t : 'a t) =
    match t with
    | NoAction | Action _ -> Decision (cond, t, t)
    | Decision (c, yes, no) ->
      if Cond.compare cond c = 0 then t
      else Decision (c, add_decision cond yes, add_decision cond no)

  let rec map_action (knowledge : knowledge)
      (f : knowledge -> 'a option -> 'a t) (t : 'a t) =
    match t with
    | NoAction -> f knowledge None
    | Action a -> f knowledge (Some a)
    | Decision (cond, yes, no) ->
      match KnowledgeMap.find_opt cond knowledge with
      | None ->
        Decision (cond,
                  map_action (KnowledgeMap.add cond true knowledge) f yes,
                  map_action (KnowledgeMap.add cond false knowledge) f no)
      | Some true ->
        Decision (cond, map_action knowledge f yes, no)
      | Some false ->
        Decision (cond, yes, map_action knowledge f no)

end
