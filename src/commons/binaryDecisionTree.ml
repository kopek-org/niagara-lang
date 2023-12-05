module type S = sig
  type condition
  module KnowledgeMap : Map.S with type key = condition
  type 'a t =
    | Decision of condition * 'a t * 'a t
    | Action of 'a
    | NoAction
  type knowledge = bool KnowledgeMap.t
  val contradictory_knowledge : knowledge -> knowledge -> bool
  val empty : 'a t
  val add_action : knowledge -> ('a option -> 'a option) -> 'a t -> 'a t
  val find : knowledge -> 'a t -> 'a option
  val fold :
    'a t ->
    noaction:(knowledge -> 'b) ->
    action:(knowledge -> 'a -> 'b) ->
    decision:(knowledge -> condition -> 'b -> 'b -> 'b) -> 'b
  val cut : knowledge -> 'a t -> 'a t
  val only_when : knowledge -> 'a t -> 'a t
  val merge :
    (knowledge -> 'a option -> 'b option -> 'c option) ->
    'a t -> 'b t -> 'c t
end


module Make(Cond : Map.OrderedType)(KnowledgeMap : Map.S with type key = Cond.t) = struct

  type condition = Cond.t

  module KnowledgeMap = KnowledgeMap

  type 'a t =
    | Decision of Cond.t * 'a t * 'a t
    | Action of 'a
    | NoAction

  type knowledge = bool KnowledgeMap.t

  let contradictory_knowledge (k1 : knowledge) (k2 : knowledge) =
    KnowledgeMap.exists (fun c d1 ->
      match KnowledgeMap.find_opt c k2 with
        | None -> false
        | Some d2 -> d1 <> d2)
      k1

  let empty = NoAction

  let add_action (k : knowledge) (act : 'a option -> 'a option) (t : 'a t) =
    let wrap_opt = function
      | None -> NoAction
      | Some a -> Action a
    in
    let build_on rk def =
      let up_act = match def with
        | Decision _ -> assert false
        | NoAction -> wrap_opt (act None)
        | Action a -> wrap_opt (act (Some a))
      in
      KnowledgeMap.fold (fun cond d t ->
          if d
          then Decision (cond, t, def)
          else Decision (cond, def, t))
        rk up_act
    in
    let rec aux rk t =
      match t with
      | Decision (cond, yes, no) ->
        begin match KnowledgeMap.find_opt cond rk with
          | None -> Decision (cond, aux rk yes, aux rk no)
          | Some d ->
            let rk = KnowledgeMap.remove cond rk in
            if d
            then Decision (cond, aux rk yes, no)
            else Decision (cond, yes, aux rk no)
        end
      | leaf -> build_on rk leaf
    in
    aux k t

  let rec find (knowledge : knowledge) (t : 'a t) =
    match t with
    | NoAction -> None
    | Action a -> Some a
    | Decision (cond, yes, no) ->
      match KnowledgeMap.find_opt cond knowledge with
      | None ->
        Errors.raise_error "(internal) Not enough information to decide"
      | Some true ->
        find knowledge yes
      | Some false ->
        find knowledge no

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

  let rec cut (knowledge : knowledge) (t : 'a t) =
    match t with
    | NoAction | Action _ -> t
    | Decision (cond, yes, no) ->
      match KnowledgeMap.find_opt cond knowledge with
      | None ->
        begin match cut knowledge yes, cut knowledge no with
          | NoAction, NoAction -> NoAction
          | yes, no -> Decision (cond, yes, no)
        end
      | Some true -> cut knowledge yes
      | Some false -> cut knowledge no

  let only_when (k : knowledge) (t : 'a t) =
    KnowledgeMap.fold (fun cond d t ->
        if d
        then Decision (cond, t, NoAction)
        else Decision (cond, NoAction, t))
      k (cut k t)

  let merge (f : knowledge -> 'a option -> 'b option -> 'c option) (d1 : 'a t) (d2 : 'b t) =
    let rec run_down knowledge act d =
      match d with
      | NoAction -> begin
          match f knowledge act None with
          | None -> NoAction
          | Some e -> Action e
        end
      | Action e -> begin
          match f knowledge act (Some e) with
          | None -> NoAction
          | Some e -> Action e
        end
      | Decision (c, d1, d2) ->
        match KnowledgeMap.find_opt c knowledge with
        | None -> Decision (c, run_down knowledge act d1, run_down knowledge act d2)
        | Some decision ->
          let d = if decision then d1 else d2 in
          run_down knowledge act d
    in
    let rec aux knowledge d1 d2 =
      match d1 with
      | Decision (c, d11, d12) -> begin
          match KnowledgeMap.find_opt c knowledge with
          | None ->
            Decision (c,
                    aux (KnowledgeMap.add c true knowledge) d11 d2,
                    aux (KnowledgeMap.add c false knowledge) d12 d2)
          | Some decision ->
            let d = if decision then d11 else d12 in
            aux knowledge d d2
        end
      | Action e -> run_down knowledge (Some e) d2
      | NoAction -> run_down knowledge None d2
    in
    aux KnowledgeMap.empty d1 d2

end
