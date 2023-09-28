type t = int

module Map = Map.Make(Int)
module Set = Set.Make(Int)
module BDT = BinaryDecisionTree.Make(Int)(Map)
module Graph = Graph.Persistent.Digraph.ConcreteBidirectional(struct
    include Int
    let hash v = v
  end)

type info = {
  var_name : string;
}

let create =
  let c = ref 0 in
  fun () -> incr c; !c

let uid v = v

let unique_anon_name =
  let c = ref 0 in
  fun name ->
    let i = !c in incr c;
    name ^ "_" ^ string_of_int i
