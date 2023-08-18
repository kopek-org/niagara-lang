type t = int

module Map = Map.Make(Int)
module Set = Set.Make(Int)
module BDT = BinaryDecisionTree.Make(Int)(Map)

(* type var_layout = *)
(*   | SimpleVar *)
(*   | ContextPoint of Context.point * t *)
(*   | MetaVariable of Context.point Map.t *)

type info = {
  var_name : string;
  (* var_layout : var_layout; *)
}

let new_var =
  let c = ref 0 in
  fun () -> incr c; !c
