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

open EzCompat (* for IntSet... *)

type domain = int
type case = int

module StrMap = StringMap
module CaseSet = IntSet
module CaseMap = IntMap
module DomainSet = IntSet
module DomainMap = IntMap


type domain_info = {
  domain_name : string;
  domain_cases : CaseSet.t;
  domain_case_offset : int;
  domain_period : int;
}

type case_info = {
  case_name : string;
  case_domain : domain;
}

type desc = CaseSet.t DomainMap.t list

type world = {
  domains : domain_info DomainMap.t;
  cases : case_info CaseMap.t;
  domain_table : domain StringMap.t;
  case_table : case StringMap.t;
  size : int;
}

module Group = struct

  let max_val = Sys.int_size

  type t = int (* cartesian production of domains cases as bitvectors *)


  module Map = IntMap

  module Set = IntSet

  let empty = 0

  let everything_up_to n = (1 lsl (n+1)) - 1

  let add n g =
    if n > max_val then
      Errors.raise_error "Added value exceeds group capacity";
    g lor (1 lsl n)

  let is_empty g = g = 0

  let not = (lnot)

  let union = (lor)

  let inter = (land)

  let equal = (=)

  let diff g1 g2 = g1 land (lnot g2)

  type clip_result = { only_left : t; common : t; only_right : t }

  let clip g1 g2 =
    { only_left = diff g1 g2;
      common = inter g1 g2;
      only_right = diff g2 g1;
    }

  let select g (offset : int) (length : int) (period : int) =
    let rec aux off len acc =
      if off + len > max_val then acc else
        let acc = add off acc in
        if len = 0
        then aux (off-length+period+1) (length -1) acc
        else aux (off+1) (len-1) acc
    in
    let mask = aux offset (length -1) 0 in
    inter g mask

end

type shape = Group.t list

let empty_world = {
  domains = DomainMap.empty;
  cases = CaseMap.empty;
  domain_table = StringMap.empty;
  case_table = StringMap.empty;
  size = 1;
}

let empty_shape = []

let any_projection world = Group.everything_up_to (world.size - 1)

let shape_of_everything world = [ any_projection world ]

let shape_of_groups (groups : Group.t list) =
  let _ = List.fold_left (fun perim g ->
      if Group.is_empty (Group.inter perim g)
      then Group.union perim g
      else Errors.raise_error "Overlapping groups in shape definition")
      Group.empty groups
  in
  groups

let is_any_projection world (g : Group.t) = Group.equal g (any_projection world)

let shape_perimeter (s : shape) =
  List.fold_left Group.union Group.empty s

let shape_project (s : shape) (p : Group.t) =
  List.filter_map (fun g ->
      let i = Group.inter g p in
      if Group.is_empty i then None
      else if Group.equal i g then Some i
      else Errors.raise_error "Incompatible projection")
    s

let shape_clip (s1 : shape) (s2 : shape) =
  let clip_group s g =
    let clipped, grem =
      List.fold_left (fun (clipped, g) sg ->
          let clip = Group.clip sg g in
          let clipped =
            if Group.is_empty clip.only_left
            then clipped
            else clip.only_left::clipped
          in
          let clipped =
            if Group.is_empty clip.common
            then clipped
            else clip.common::clipped
          in
          clipped, clip.only_right)
        ([], g) s
    in
    if Group.is_empty grem then clipped else grem::clipped
  in
  List.fold_left (fun clipped g2 ->
      clip_group clipped g2)
    s1 s2

let shape_imprint_projection (s : shape) (g : Group.t) =
  List.map (fun sg ->
      let clip = Group.clip sg g in
      List.filter (fun g -> not (Group.is_empty g))
        [clip.only_left; clip.common])
    s
  |> List.flatten

let shape_cut_out (s : shape) (g : Group.t) =
  List.filter_map (fun sg ->
      let i = Group.inter g sg in
      if Group.is_empty i then None else Some i)
    s

let shape_overlap_subshape (s : shape) (g : Group.t) =
  List.filter (fun sg -> not (Group.is_empty (Group.inter sg g))) s

let shape_fold (f : 'a -> Group.t -> 'a) (acc : 'a) (s : shape) =
  List.fold_left f acc s

let find_domain world (dom : string) =
  match StrMap.find_opt dom world.domain_table with
  | Some d -> d
  | None -> Errors.raise_error "Unknown domain %s" dom

let find_case world (case : string) =
  match StrMap.find_opt case world.case_table with
  | Some d -> d
  | None -> Errors.raise_error "Unknown case %s" case

let case_is_in_domain world (case : case) (dom : domain) =
  (DomainMap.find dom world.domains).domain_cases |> CaseSet.mem case

let domain_of_case world (case : case) =
  (CaseMap.find case world.cases).case_domain

let group_of_selection world (select : CaseSet.t DomainMap.t) =
  let offsets =
    DomainMap.fold (fun dom dinfo offsets ->
        let cases =
          match DomainMap.find_opt dom select with
          | None -> dinfo.domain_cases
          | Some cases ->
            if CaseSet.is_empty cases
            then dinfo.domain_cases
            else cases
        in
        List.map (fun off ->
            CaseSet.fold (fun c offs ->
                (off + dinfo.domain_case_offset * (c - dom))::offs)
              cases [])
          offsets
        |> List.flatten)
      world.domains [0]
  in
  List.fold_left (fun g o -> Group.add o g) Group.empty offsets

let add_domain =
  let c = ref 0 in
  fun world (dom : string) (cases : string list) ->
    let domain = !c in
    let case_table, cases, domain_cases =
      List.fold_left (fun (ct, cs, dc) case_name ->
          if StrMap.mem case_name ct then
            Errors.raise_error "Case %s already declared" case_name;
          let i = !c in
          incr c;
          StrMap.add case_name i ct,
          CaseMap.add i { case_name; case_domain = domain } cs,
          CaseSet.add i dc
        )
        (world.case_table, world.cases, CaseSet.empty) cases
    in
    let domain_table =
      if StrMap.mem dom world.domain_table then
        Errors.raise_error "Domain %s already declared" dom;
      StrMap.add dom domain world.domain_table
    in
    let size =
      let s = world.size * (!c - domain) in
       if s > Group.max_val then
         Errors.raise_error "(internal) Too many contexts for %d bits bitvector"
           Group.max_val
       else s
    in
    let domains =
      DomainMap.add domain
        { domain_name = dom;
          domain_cases;
          domain_case_offset = world.size;
          domain_period = size;
        }
        world.domains
    in
    { domains; domain_table; case_table; cases; size }

let group_desc world (g : Group.t) =
  let select_case_in_group dinfos d c g =
    let off = dinfos.domain_case_offset in
    let period = dinfos.domain_period in
    let start = (c - d) * off in
    (Group.select g start off period), start
  in
  let doms = world.domains in
  let rec aux (d, infos) doms g =
    let aux g =
      let ndoms = DomainMap.remove d doms in
      if DomainMap.is_empty ndoms then [DomainMap.empty] else
        aux (DomainMap.choose ndoms) ndoms g
    in
    let case_split =
      CaseSet.fold (fun c acc ->
          let cg, align = select_case_in_group infos d c g in
          if cg = 0 then acc else
            IntMap.update (cg lsr align) (function
                | None -> Some (CaseSet.singleton c, cg)
                | Some (cs, csg) -> Some (CaseSet.add c cs, csg lor cg))
              acc)
        infos.domain_cases IntMap.empty
    in
    IntMap.fold (fun _ (cs, csg) acc ->
        let odoms = aux csg in
        List.map (DomainMap.add d cs) odoms
        @ acc)
      case_split []
  in
  if DomainMap.is_empty doms then [] else
    aux (DomainMap.choose doms) doms g


let print_domain world fmt (d : domain) =
  let dom = (DomainMap.find d world.domains).domain_name in
  Format.fprintf fmt "%s" dom

let print_case world fmt (c : case) =
  let case = (DomainMap.find c world.cases).case_name in
  Format.fprintf fmt "%s" case

let print_cases world fmt (cs : CaseSet.t) =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
      (print_case world) fmt
      (CaseSet.elements cs)

let print_dommap world fmt (dm : CaseSet.t DomainMap.t) =
  Format.fprintf fmt "@[<hv 1>(";
  DomainMap.iter (fun d cs ->
      Format.fprintf fmt "%a(%a),@,"
        (print_domain world) d
        (print_cases world) cs)
    dm;
  Format.fprintf fmt "@])"

let print_group world fmt (g : Group.t) =
  let dommaps = group_desc world g in
  Format.fprintf fmt "@[<hv>%X@," g;
    Format.pp_print_list
      (fun fmt map ->
        Format.fprintf fmt "- %a@ " (print_dommap world) map)
      fmt dommaps;
    Format.fprintf fmt "@]"

let print_projection world fmt (p : Group.t) =
  if is_any_projection world p then
    Format.fprintf fmt "[[any]]"
  else begin
    Format.fprintf fmt "@[<hov 2>[[@,";
    print_group world fmt p;
    Format.fprintf fmt "]]@]"
  end

let print_shape world fmt (s : shape) =
  if s = [] then
    Format.fprintf fmt "{nothing}"
  else begin
    Format.fprintf fmt "@[<hv 2>{ ";
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
      (print_group world)
      fmt s;
    Format.fprintf fmt "@]}"
  end

let print_group_id fmt (g : Group.t) =
  Format.fprintf fmt "%X" g

let print_group_desc world fmt (desc : desc) =
  let print_one fmt d =
    let cs = DomainMap.fold (fun _d -> CaseSet.union) d CaseSet.empty in
    if List.length desc > 1
    then Format.fprintf fmt "@[<hov>- %a@]" (print_cases world) cs
    else Format.fprintf fmt "@[<hov>%a@]" (print_cases world) cs
  in
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list print_one) desc

let print_domain_desc world fmt (dom, dom_info) =
  Format.fprintf fmt "@[<v 2>Domain %a:@,%a@]"
    (print_domain world) dom
    (Format.pp_print_list
       (fun fmt -> Format.fprintf fmt "Case %a" (print_case world)))
    (CaseSet.elements dom_info.domain_cases)

let print_world_desc fmt world =
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list (print_domain_desc world))
    (DomainMap.bindings world.domains)
