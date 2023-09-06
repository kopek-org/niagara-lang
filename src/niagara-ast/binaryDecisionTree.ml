(*****************************************************************************)
(*                                                                           *)
(*  Copyright (c) 2023 OCamlPro SAS                                          *)
(*                                                                           *)
(* All rights reserved.                                                      *)
(* This source code is licensed under the GNU Affero General Public License  *)
(* version 3 found in the LICENSE.md file in the root directory of this      *)
(* source tree.                                                              *)
(*                                                                           *)
(*****************************************************************************)

module Make(Cond : Map.OrderedType)(KnowledgeMap : Map.S with type key = Cond.t) = struct

  type condition = Cond.t

  type 'a t =
    | Decision of Cond.t * 'a t * 'a t
    | Action of 'a
    | NoAction

  type knowledge = bool KnowledgeMap.t

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

  let rec cut (knowledge : knowledge) (t : 'a t) =
    match t with
    | NoAction | Action _ -> t
    | Decision (cond, yes, no) ->
      match KnowledgeMap.find_opt cond knowledge with
      | None -> Decision (cond, cut knowledge yes, cut knowledge no)
      | Some true -> cut knowledge yes
      | Some false -> cut knowledge no

  let merge (f : knowledge -> 'a option -> 'a option -> 'b option) (d1 : 'a t) (d2 : 'a t) =
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
