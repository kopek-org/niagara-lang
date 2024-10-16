let pp_xor = "\u{2295}"
let pp_and = "\u{2227}"

module Conj = struct

type t = { input : Variable.t option; events : Variable.t list }

let rec merge_evts ~(xor : bool) c1 c2 =
  match c1, c2 with
  | [],t | t,[] -> t
  | h1::t1, h2::t2 ->
    let cmp = Variable.compare h1 h2 in
    if cmp = 0 then
      if xor then merge_evts ~xor t1 t2 else h1::(merge_evts ~xor t1 t2)
    else if cmp < 0 then
      h1::(merge_evts ~xor t1 c2)
    else
      h2::(merge_evts ~xor c1 t2)

let compare c1 c2 =
  match c1.input, c2.input with
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some i1, Some i2 ->
    let cmp = Variable.compare i1 i2 in
    if cmp = 0 then List.compare Variable.compare c1.events c2.events
    else cmp
  | None, None -> List.compare Variable.compare c1.events c2.events

let conj c1 c2 =
  match c1.input, c2.input with
  | Some i1, Some i2 when not (Variable.equal i1 i2) -> None
  | None, input | input, _ ->
    Some { input; events = merge_evts ~xor:false c1.events c2.events }

let print fmt { input; events; } =
  let open Format in
  let has_input =
    match input with
    | Some v -> fprintf fmt "i%d" (Variable.uid v); true
    | None -> false
  in
  if events <> [] then
    begin
      if has_input then fprintf fmt "@ %s " pp_and;
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt "@ %s " pp_and)
        (fun fmt v -> fprintf fmt "e%d" (Variable.uid v))
        fmt events
    end

end

module ConjSet = struct

  type t = Conj.t list

  let rec insert c cs =
    match cs with
    | [] -> [c]
    | h::t ->
      let cmp = Conj.compare c h in
      if cmp = 0 then t
      else if cmp < 0 then c::cs
      else h::(insert c t)

  let empty = []

  let is_empty = (=) []

  let disj_union cs1 cs2 =
    let rec aux u cs1 cs2 =
      match cs1, cs2 with
      | [], cs | cs, [] -> List.rev_append cs u
      | h1::t1, h2::t2 ->
        let cmp = Conj.compare h1 h2 in
        if cmp = 0 then aux u t1 t2
        else if cmp < 0 then aux (h1::u) t1 cs2
        else aux (h2::u) cs1 t2
    in
    List.rev @@ aux [] cs1 cs2

  let fold = List.fold_left

  let distribute c cs =
    let rec aux r cs =
      match cs with
      | [] -> r
      | h::t ->
        match Conj.conj c h with
        | None -> aux r t
        | Some c -> aux (insert c r) t
    in
    aux [] (List.rev cs)

  let print fmt cs =
    let open Format in
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt "@ %s " pp_xor)
      (fun fmt c ->
         fprintf fmt "@[<hov 2>(%a@])" Conj.print c)
      fmt cs

end


type anf = {
  neg : bool;
  clauses : ConjSet.t;
}

let never = { neg = false; clauses = ConjSet.empty }

let always = { never with neg = true }

let xor f1 f2 =
  { neg = if f1.neg then not f2.neg else f2.neg;
    clauses = ConjSet.disj_union f1.clauses f2.clauses;
  }

let neg f = { f with neg = not f.neg }

let add_conj c1 f2 =
  let clauses0 = ConjSet.distribute c1 f2.clauses in
  let clauses = if f2.neg then ConjSet.insert c1 clauses0 else clauses0 in
  { neg = false; clauses }

let conj f1 f2 =
  let clauses_conj =
    ConjSet.fold (fun cc c1 ->
        xor (add_conj c1 f2) cc)
      never f1.clauses
  in
  if f1.neg
  then xor f2 clauses_conj
  else clauses_conj

let disj f1 f2 =
  neg (conj (neg f1) (neg f2))

type t = anf

let is_never t = not t.neg && ConjSet.is_empty t.clauses

let is_always t = t.neg && ConjSet.is_empty t.clauses

let print_disj fmt t =
  let open Format in
  if t.neg then pp_print_char fmt 'T';
  if ConjSet.is_empty t.clauses then
    (if not t.neg then pp_print_string fmt "never")
  else
    begin
      if t.neg then fprintf fmt "@ %s " pp_xor;
      ConjSet.print fmt t.clauses
    end


let of_input v = {
  neg = false;
  clauses = [Conj.{ input = Some v; events = [] }];
}

let of_event v p = {
  neg = not p;
  clauses =  [Conj.{ input = None; events = [v] }];
}

let excluded t1 t2 = conj t1 (neg t2)

let print = print_disj
