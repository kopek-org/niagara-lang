type t = int

module Map = Map.Make(Int)
module Set = Set.Make(Int)

module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(struct
    include Int
    let hash v = v
  end)
    (struct
      type t = Set.t
      let compare = Set.compare
      let default = Set.empty
    end)

module Graph = struct
  include G
  module Topology = Graph.Components.Make(G)

  let reachables (g : G.t) (start : G.V.t) : Set.t =
    let rec aux acc v =
      if Set.mem v acc || not (G.mem_vertex g v) then acc else
        List.fold_left aux
          (Set.add v acc)
          (G.succ g v)
    in
    aux Set.empty start

  module DAG = struct

    let rev_topological_depth_ordering (g : G.t) : G.V.t list =
      let rec aux v ((mem, ord, hold) as acc) =
        if Set.mem v mem then acc
        else if List.for_all (fun s -> Set.mem s mem) (G.succ g v) then
          let acc = Set.add v mem, v::ord, Set.remove v hold in
          let (mem, ord, hold) =
            List.fold_left (fun acc p -> aux p acc)
              acc
              (G.pred g v)
          in
          if G.in_degree g v = 0 then
            Set.fold aux hold (mem, ord, hold)
          else (mem, ord, hold)
        else
          (mem, ord, Set.add v hold)
      in
      let (_, ord, _) =
        G.fold_vertex (fun v acc ->
            if G.out_degree g v = 0 then aux v acc else acc)
          g (Set.empty, [], Set.empty)
      in
      ord

    let topological_depth_ordering (g : G.t) : G.V.t list =
      List.rev @@ rev_topological_depth_ordering g

    let transitive_closure (g0 : G.t) : G.t =
      let order = rev_topological_depth_ordering g0 in
      List.fold_left (fun g v ->
          G.fold_pred_e (fun (p, e, _) g ->
              G.fold_pred_e (fun (pp, pe, _) g ->
                  G.add_edge_e g (pp, Set.union e pe, v))
                g p g)
            g v g)
        g0
        order
  end

end

let create =
  let c = ref 0 in
  let () = CompilerState.register_on_reset (fun () -> c := 0) in
  fun () -> incr c; !c

let uid v = v

let compare = Int.compare

let unique_anon_name =
  let c = ref 0 in
  let () = CompilerState.register_on_reset (fun () -> c := 0) in
  fun name ->
    let i = !c in incr c;
    name ^ "_" ^ string_of_int i

let equal = (==)
