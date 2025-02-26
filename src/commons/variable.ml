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

  (* ocamlgraph code with edge merging *)
  let transitive_closure (g0 : G.t) : G.t =
    let phi v g =
      G.fold_succ_e
        (fun (_, se, sv) g ->
              G.fold_pred_e
                (fun (pv, pe, _) g ->
                   let me = Set.union pe se in
                   G.add_edge_e g (pv, me, sv))
                g v g)
        g v g
    in
    G.fold_vertex phi g0 g0

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
