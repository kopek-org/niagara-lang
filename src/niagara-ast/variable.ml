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

type t = int

module Map = Map.Make(Int)
module Set = Set.Make(Int)
module BDT = BinaryDecisionTree.Make(Int)(Map)
module Graph = Graph.Persistent.Digraph.ConcreteBidirectional(struct
    include Int
    let hash v = v
  end)

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
