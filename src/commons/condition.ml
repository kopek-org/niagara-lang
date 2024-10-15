
type conj = { input : Variable.t option; events : Variable.Set.t }

let compare_conj c1 c2 =
  match c1.input, c2.input with
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some i1, Some i2 ->
    let cmp = Variable.compare i1 i2 in
    if cmp = 0 then Variable.Set.compare c1.events c2.events
    else cmp
  | None, None -> Variable.Set.compare c1.events c2.events

let conj_conj c1 c2 =
  match c1.input, c2.input with
  | Some i1, Some i2 when not (Variable.equal i1 i2) -> None
  | None, input | input, _ ->
    Some { input; events = Variable.Set.union c1.events c2.events }


module ConjSet = struct
  module Impl = Map.Make(struct type t = conj let compare = compare_conj end)

  type t = unit Impl.t

  let empty : t = Impl.empty

  let is_empty = Impl.is_empty

  let singleton c = Impl.singleton c ()

  let xor c cs =
    if Impl.mem c cs then Impl.remove c cs else Impl.add c () cs

  let fold f cs acc =
    Impl.fold (fun c () acc -> f c acc) cs acc

  let merge f =
    Impl.merge (fun c u1 u2 ->
        if f c (Option.is_some u1) (Option.is_some u2)
        then Some ()
        else None)

  let elements f = Impl.bindings f |> List.map fst

end

type anf = {
  neg : bool;
  clauses : ConjSet.t;
}

let never = { neg = false; clauses = ConjSet.empty }

let always = { never with neg = true }

let xor f1 f2 =
  { neg = if f1.neg then not f2.neg else f2.neg;
    clauses =
      ConjSet.merge (fun _c p1 p2 ->
          not (p1 && p2))
        f1.clauses f2.clauses;
  }

let neg f = { f with neg = not f.neg }

let add_conj c1 f2 =
  let clauses0 =
    ConjSet.fold (fun c2 cs ->
        match conj_conj c1 c2 with
        | None -> cs
        | Some c -> ConjSet.xor c cs)
      f2.clauses ConjSet.empty
  in
  let clauses = if f2.neg then ConjSet.xor c1 clauses0 else clauses0 in
  { neg = false; clauses }

let conj f1 f2 =
  let clauses_conj =
    ConjSet.fold (fun c1 cc ->
        xor (add_conj c1 f2) cc)
      f1.clauses never
  in
  if f1.neg
  then xor f2 clauses_conj
  else clauses_conj

let disj f1 f2 =
  neg (conj (neg f1) (neg f2))

type t = anf

let is_never t = not t.neg && ConjSet.is_empty t.clauses

let is_always t = t.neg && ConjSet.is_empty t.clauses

let pp_xor = "\u{2295}"
let pp_and = "\u{2227}"

let print_conj fmt { input; events; } =
  let open Format in
  let has_input =
    match input with
    | Some v -> fprintf fmt "i%d" (Variable.uid v); true
    | None -> false
  in
  if not (Variable.Set.is_empty events) then
    begin
      if has_input then fprintf fmt "@ %s " pp_and;
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt "@ %s " pp_and)
        (fun fmt v -> fprintf fmt "e%d" (Variable.uid v))
        fmt
        (Variable.Set.elements events)
    end

let print_disj fmt t =
  let open Format in
  if t.neg then pp_print_char fmt 'T';
  if ConjSet.is_empty t.clauses then
    (if not t.neg then pp_print_string fmt "never")
  else
    begin
      if t.neg then fprintf fmt "@ %s " pp_xor;
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt "@ %s " pp_xor)
        (fun fmt c ->
           fprintf fmt "@[<hov 2>(%a@])" print_conj c)
        fmt
        (ConjSet.elements t.clauses)
    end


let of_input v = {
  neg = false;
  clauses = ConjSet.singleton { input = Some v; events = Variable.Set.empty };
}

let of_event v p = {
  neg = not p;
  clauses = ConjSet.singleton { input = None; events = Variable.Set.singleton v };
}

let excluded t1 t2 = conj t1 (neg t2)

let print = print_disj
