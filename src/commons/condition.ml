
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


type satisfaction =
  | Sat
  | Unsat
  | Unknown of Variable.t

let rec satisfies (k : bool Variable.Map.t) t =
  match get t with
  | T -> Sat
  | F -> Unsat
  | Var (Event v, l, r) ->
    (match Variable.Map.find_opt v k with
     | None -> Unknown v
     | Some b -> if b then satisfies k l else satisfies k r)
  | Var (Input v, l, r) ->
    match Variable.Map.find_opt v k with
    | None -> satisfies k r
    | Some b -> if b then satisfies k l else satisfies k r
