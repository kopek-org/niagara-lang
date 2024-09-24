type t = int

module Map = Map.Make(Int)
module Set = Set.Make(Int)
module BDT = BinaryDecisionTree.Make(Int)(Map)

module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(struct
    include Int
    let hash v = v
  end)
    (struct
      type t = bool Map.t
      let compare = Map.compare (Stdlib.compare)
      let default = Map.empty
    end)

module Graph = struct
  include G
  module Topology = Graph.Components.Make(G)
end

module Info = struct

  type origin =
    | Named of string
    | LabelOfPartner of { partner : t; label : string }
    | Cumulative of t
    | AnonEvent
    | Peeking of t
    | RisingEvent of t
    | ContextSpecialized of { origin : t; context : Context.Group.t }
    | OperationDetail of { source : t; target : t }
    | ExistentialAggreg of t list

  type kind =
    | ReceivingPartner
    | ProvidingPartner
    | ParameterInput
    | PoolInput
    | Intermediary
    | Event
    | Constant

  type t = {
    origin : origin;
    typ : ValueType.t;
    kind : kind;
  }

  type collection = t Map.t

  let print fmt t =
    match t.origin with
    | Named name -> Format.pp_print_string fmt name
    | LabelOfPartner { partner; label } ->
      Format.fprintf fmt "%d$%s" partner label
    | Cumulative v ->
      Format.fprintf fmt "#%d" v
    | AnonEvent -> Format.pp_print_string fmt "anon_event"
    | Peeking v -> Format.fprintf fmt "@%d" v
    | RisingEvent v -> Format.fprintf fmt "^%d" v
    | ContextSpecialized { origin; context } ->
      Format.fprintf fmt "%d(%a)" origin Context.Group.print context
    | OperationDetail { source; target } ->
      Format.fprintf fmt "[%d->%d]" source target
    | ExistentialAggreg vs ->
      Format.(fprintf fmt "[%a]" (pp_print_list pp_print_int) vs)

end

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

let equal = (=)
