
type 'a hcd = {
  ctn : 'a;
  tag : int;
}

module Hashing = struct

module Make(H : Hashtbl.HashedType) = struct
  module E = Hashtbl.Make(H)

  let create = E.create

  let hash_cons =
    let fresh =
      let i = ref (-1) in
      CompilerState.register_on_reset (fun () -> i := -1);
      fun () -> incr i; !i
    in
    fun ht k ->
      try E.find ht k with
      | Not_found ->
          let v = { ctn = k; tag = fresh () } in
          E.add ht k v; v

end
end

module Memoizing = struct

module Make(H : Hashtbl.HashedType) : sig
  type t = H.t
  val memo : ((t -> 'a) -> t -> 'a) -> t -> 'a
  val memo2 : ((t -> t -> 'a) -> t -> t -> 'a) -> t -> t -> 'a
end = struct
  type t = H.t

  module Hash = Hashtbl.Make(H)
  let memo f =
    let ht = Hash.create 256 in
    CompilerState.register_on_reset (fun () -> Hash.clear ht);
    let rec ff k =
      try Hash.find ht k with
      | Not_found ->
          let v = f ff k in
          Hash.add ht k v; v
    in
    ff

  module Hash2 = Hashtbl.Make(struct
      type t = H.t * H.t
      let equal (x1,x2) (y1,y2) = H.equal x1 y1 && H.equal x2 y2
      let hash (x,y) = (H.hash x) lxor (H.hash y)
    end)
  let memo2 f =
    let ht = Hash2.create 256 in
    CompilerState.register_on_reset (fun () -> Hash2.clear ht);
    let rec ff k1 k2 =
      try Hash2.find ht (k1,k2) with
      | Not_found ->
          let v = f ff k1 k2 in
          Hash2.add ht (k1,k2) v; v
    in
    ff

end
end

type var =
  | Input of Variable.t
  | Event of Variable.t

type comp =
  | Eq
  | Lt
  | Gt
  | Contradicts of int

let compare2comp c =
  if c = 0 then Eq else if c < 0 then Lt else Gt

let var_comp v1 v2 =
  match v1, v2 with
  | Input v1, Input v2 ->
    let c = Variable.compare v1 v2 in
    if c <> 0 then Contradicts c
    else compare2comp c
  | Event v2, Event v1 -> (* reversed for events *)
    let c = Variable.compare v1 v2 in
    compare2comp c
  | Input _, Event _ -> Gt
  | Event _, Input _ -> Lt

type bdd =
  | T
  | F
  | Var of var * bdd hcd * bdd hcd

module HCBDD = Hashing.Make(struct
type t = bdd

let equal t1 t2 =
  match t1, t2 with
  | F, F | T, T -> true
  | Var (Input v1,l1,r1), Var (Input v2,l2,r2)
  | Var (Event v1,l1,r1), Var (Event v2,l2,r2) ->
    Variable.equal v1 v2
    && l1 == l2
    && r1 == r2
  | _ -> false

let hash t =
  match t with
  | F -> 0
  | T -> 1
  | Var ((Input v | Event v),l,r) ->
    19 * (19 * (Variable.uid v) + l.tag) + r.tag + 2

end)

module Memo = Memoizing.Make(struct
    type t = bdd hcd
    let equal = (==)
    let hash x = x.tag
  end)

let get hcd = hcd.ctn
let map f hcd = f (get hcd)
let hc = HCBDD.hash_cons (HCBDD.create 256)

let always = hc T
let never = hc F

let is_never t = get t = F
let is_always t = get t = T

let var_weight var =
  match var with
  | Input v -> Variable.uid v
  | Event v -> - Variable.uid v

let weight t =
  match get t with
  | T | F -> max_int
  | Var (v,_,_) -> var_weight v

let node v l r =
  if var_weight v >= weight l || var_weight v >= weight r
  then assert false;
  if l == r then l else hc (Var (v,l,r))

let neg = Memo.memo (fun neg t ->
    map
      (function
        | F -> always
        | T -> never
        | Var (v,l,r) -> node v (neg l) (neg r))
      t)

let of_bool b =
  if b then always else never

let no_more_inputs = Memo.memo (fun no_more_inputs ->
  map (function
  | T -> always | F -> never
  | Var (v,l,r) ->
    match v with
    | Event _ -> node v (no_more_inputs l) (no_more_inputs r)
    | Input _ -> no_more_inputs r))

let comb op = (fun comb l r ->
    match get l, get r with
    | T, T -> of_bool (op true true)
    | F, F -> of_bool (op false false)
    | T, F | F, T -> of_bool (op true false)
    | T, Var (v,l,r) | Var (v,l,r), T ->
      node v (comb l always) (comb r always)
    | F, Var (v,l,r) | Var (v,l,r), F ->
      node v (comb l never) (comb r never)
    | Var (v1,l1,r1), Var (v2,l2,r2) ->
      match var_comp v1 v2 with
      | Eq -> node v1 (comb l1 l2) (comb  r1 r2)
      | Lt -> node v1 (comb l1 r) (comb r1 r)
      | Gt -> node v2 (comb l l2) (comb l r2)
      | Contradicts c ->
        if c < 0 then node v1 (comb l1 (no_more_inputs r2)) (comb r1 r)
        else node v2 (comb l2 (no_more_inputs r1)) (comb l r2))


let conj = Memo.memo2 (comb (&&))

let disj = Memo.memo2 (comb (||))

let excluded = Memo.memo2 (fun _excl t1 t2 -> conj t1 (neg t2))

let past_of = Memo.memo (fun past_of t ->
    match get t with
    | F | T -> never
    | Var (Input _, _, _) -> t
    | Var (v, l, r) ->
      let l_past = past_of l in
      (match get r with
      | F -> node v l_past (disj l l_past)
      | T -> never
      | _ ->
        let r_past = past_of r in
        match get r_past with
        | F -> never
        | _ -> node v l_past r_past))
(* The condition that is strictely in the past of any comparable part of the input.

   The rational is the following:

   - If the tree has a "before" branch always true (included) then no
   past of it exists.

   - If that branch is false, then there is a past where the current
   event is false and the rest (below) is at the same point or before.

   - If it happens that the past of the before branch in non-existent,
   then there is no past for the whole tree. (Generalisation of the
   first point).

   - In every other case, the past of the tree is the past of either
   branch assuming the current event is in the same state.

   In non-monotonous cases, this excludes the "holes" that are the
   past of some subset but the future or present of others and
   incomparable states. *)


let of_event v p = node (Event v) (of_bool p) (of_bool (not p))

let of_input v = node (Input v) always never

let print_neg fmt () = Format.fprintf fmt "\u{00AC}"
let print_conj fmt () = Format.fprintf fmt "\u{22C0}"
let print_disj fmt () = Format.fprintf fmt "\u{22C1}"

let print_var fmt v b =
  let open Format in
  let v, d = match v with
    | Input v -> 'i', Variable.uid v
    | Event v -> 'e', Variable.uid v
  in
  if b then
    fprintf fmt "%c%d" v d
  else
    fprintf fmt "%a%c%d" print_neg () v d

let rec print fmt t =
  let open Format in
  match get t with
  | T -> pp_print_string fmt "always"
  | F -> pp_print_string fmt "never"
  | Var (v,l,r) ->
    match get l, get r with
    | T,F -> print_var fmt v true
    | F,T -> print_var fmt v false
    | _,F -> print_with_var v true fmt l
    | F,_ -> print_with_var v false fmt r
    | _ ->
      fprintf fmt "@[<hov 2>(%a@ %a %a@])"
        (print_with_var v true) l
        print_disj ()
        (print_with_var v false) r

and print_with_var v b fmt t =
  let open Format in
  match get t, v with
  | (T | F), _ ->
    let tv = is_always t in
    if tv && b || not (tv || b)
    then print_var fmt v true
    else print_var fmt v false
  | Var (Input _,_,_), Input _ ->
    (* we are above another input, it's redondant *)
    print fmt t
  | _ ->
    fprintf fmt "@[<hov 2>(%a@ %a %a@])"
      (fun fmt -> print_var fmt v) b
      print_conj ()
      print t

type t = bdd hcd

let time_split t1 t2 =
  let past1 = past_of t1 in
  let past2 = past_of t2 in
  let b1 = conj t1 past2 in
  let b2 = conj t2 past1 in
  let after = disj (excluded t1 b1) (excluded t2 b2) in
  let before = disj b1 b2 in
  before, after
(* Separates [t1 || t2] into [c1, c2] so that [c1] is the part of [t1]
   in the strict past of [t2] or the other way around, and [c2] the
   rest. *)

let time_ranking (ts : t list) : t list =
  let insert_rank ranks t =
    (* Each condition added may successively slice already present
       ranks. The recursion add incomparable subsets to the current
       rank, push the rest to the lower ranks, and makes the strict
       past bubble up, to be merge to the rank above. *)
    let rec aux ranks t =
      if is_never t then never, ranks else
        match ranks with
        | [] -> never, [t]
        | r::rs ->
          let bubble, after = time_split t r in
          let r, t =
            if is_never bubble then
              if is_never (conj r t) then
                never, after
              else
                (* Handling non exclusive inputs. The past is already
                   taken care of. The exclusive futures pushed down,
                   and the conjonction cannot be splited further. *)
                conj after r, excluded after r
            else excluded r bubble, excluded t bubble
          in
          let rs = if is_never r then rs else r::rs in
          let b, rs = aux rs t in
          bubble, if is_never b then rs else b::rs
    in
    let b, ranks = aux ranks t in
    if is_never b then ranks else b::ranks
  in
  List.fold_left insert_rank [] ts

type satisfaction =
  | Sat
  | Unsat
  | MaySat

let rec satisfies (k : bool Variable.Map.t) t =
  match get t with
  | T -> Sat
  | F -> Unsat
  | Var (Event v, l, r) ->
    (match Variable.Map.find_opt v k with
     | Some b -> if b then satisfies k l else satisfies k r
     | None ->
       match satisfies k l, satisfies k r with
       | Sat, Sat -> Sat
       | Unsat, Unsat -> Unsat
       | _ -> MaySat)
  | Var (Input v, l, r) ->
    match Variable.Map.find_opt v k with
    | None -> satisfies k r
    | Some b -> if b then satisfies k l else satisfies k r


type tree =
  | True
  | False
  | Branch of { var : var; yes : tree; no : tree }

let rec tree (t : t) =
  match get t with
  | T -> True
  | F -> False
  | Var (var, yes, no) -> Branch { var; yes = tree yes; no = tree no }

let events_of (t : t) =
  let rec aux evts t =
    match get t with
    | T | F -> evts
    | Var (Event v, yes, no) ->
      aux (aux (Variable.Set.add v evts) yes) no
    | Var (Input _, yes, no) ->
      aux (aux evts yes) no
  in
  aux Variable.Set.empty t
