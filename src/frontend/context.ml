
module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

(***********)


type domain = int
type case = int

module CaseSet = IntSet
module CaseMap = IntMap
module DomainSet = IntSet
module DomainMap = IntMap

type group = CaseSet.t DomainMap.t
type projection = group

module GroupMap = Map.Make(struct
    type t = group
    let compare = DomainMap.compare CaseSet.compare
end)

type shape = group list

type domain_info = {
  domain_name : string;
  domain_cases : CaseSet.t;
  domain_extensible : bool;
}

type case_info = {
  case_name : string;
  case_domain : domain;
}

type world = {
  domains : domain_info DomainMap.t;
  cases : case_info CaseMap.t;
  domain_table : domain StrMap.t;
  case_table : case StrMap.t;
}

let empty_world = {
  domains = DomainMap.empty;
  cases = CaseMap.empty;
  domain_table = StrMap.empty;
  case_table = StrMap.empty;
}

let fresh_id =
  let c = ref 0 in
  fun () -> let i = !c in incr c; i


let any_projection = DomainMap.empty

let empty_shape = []

let shape_of_everything world = [
  DomainMap.map (fun info -> info.domain_cases) world.domains
 ]

let shape_of_groups (groups : group list) = groups

(* let is_any_shape world (s : shape) = *)
(*   match s with *)
(*   | [g] -> *)
(*     DomainMap.for_all (fun dom { domain_cases; _ } -> *)
(*         match DomainMap.find_opt dom g with *)
(*         | None -> false *)
(*         | Some cs -> CaseSet.equal domain_cases cs) *)
(*       world.domains *)
(*   | _ -> false *)

let find_domain world (dom_name : string) =
  match StrMap.find_opt dom_name world.domain_table with
  | Some dom -> dom
  | None -> Errors.raise_error "Unknown domain %s" dom_name

let find_case world (case_name : string) =
  match StrMap.find_opt case_name world.case_table with
  | Some dom -> dom
  | None -> Errors.raise_error "Unknown context case %s" case_name

let find_domain_info world (dom : domain) =
  match DomainMap.find_opt dom world.domains with
  | Some info -> info
  | None -> Errors.raise_error "Unregistered domain id"

let find_case_info world (case : case) =
  match DomainMap.find_opt case world.cases with
  | Some info -> info
  | None -> Errors.raise_error "Unregistered case id"

let domain_of_case world (case : case) =
  let infos = find_case_info world case in
  infos.case_domain

let print_cases world fmt (cases : CaseSet.t) =
  CaseSet.iter (fun case ->
      let { case_name; _ } = find_case_info world case in
      Format.fprintf fmt "%s," case_name)
    cases

let print_group world fmt (g : group) =
  DomainMap.iter (fun dom cases ->
      let { domain_name; _ } = find_domain_info world dom in
      Format.fprintf fmt "%s(" domain_name;
      print_cases world fmt cases;
      Format.fprintf fmt ") "
    ) g

let print_projection world fmt (p : projection) =
  if DomainMap.is_empty p then
    Format.fprintf fmt "[[any]]"
  else begin
    Format.fprintf fmt "@[<hov 2>[[";
    print_group world fmt p;
    Format.fprintf fmt "@]]]"
  end

let print_shape world fmt (s : shape) =
  if s = [] then
    Format.fprintf fmt "{nothing}"
  else begin
    Format.fprintf fmt "@[<hov 2>{@ ";
    List.iter (fun group ->
        Format.fprintf fmt "%a; " (print_group world) group)
      s;
    Format.fprintf fmt "@]}"
  end

let fold_shape = List.fold_left

let add_domain world (domain_name : string) (cases_names : string list) =
  let domain = fresh_id () in
  let cases = List.map (fun _ -> fresh_id ()) cases_names in
  let domain_table =
    StrMap.update domain_name (function
        | None -> Some domain
        | Some _ -> Errors.raise_error "Context domain %s already defined" domain_name
      )
      world.domain_table
  in
  let domains =
    DomainMap.add domain
      { domain_name;
        domain_cases = CaseSet.of_list cases;
        domain_extensible = false
      }
      world.domains
  in
  let cases, case_table =
    List.fold_left2 (fun (cases, case_table) name case ->
        let case_info = { case_name = name; case_domain = domain } in
        CaseMap.add case case_info cases,
        StrMap.update name (function
            | None -> Some case
            | Some _ -> Errors.raise_error "Context case %s already defined" name)
          case_table
      )
      (world.cases, world.case_table)
      cases_names cases
  in
  {
    domains;
    domain_table;
    cases;
    case_table;
  }

let extend_domain world (dom : string) (c : string) =
  let error_case_not_belong () =
    Errors.raise_error "Case %s does not belong to domain %s" c dom
  in
  let error_domain_not_extensible () =
    Errors.raise_error "Domain %s not extensible" dom
  in
  match
    StrMap.find_opt dom world.domain_table,
    StrMap.find_opt c world.case_table
  with
    | Some domain, Some case ->
      let domain_info = find_domain_info world domain in
      if CaseSet.mem case domain_info.domain_cases then
        if domain_info.domain_extensible then
          world, domain, case
        else
          error_domain_not_extensible ()
      else error_case_not_belong ()
    | Some domain, None ->
      let domain_info = find_domain_info world domain in
      if domain_info.domain_extensible then
        let case = fresh_id () in
        let domain_info = { domain_info with
          domain_cases = CaseSet.add case domain_info.domain_cases;
        }
        in
        let case_info = {
          case_name = c;
          case_domain = domain;
        }
        in
        let world = { world with
          domains = DomainMap.add domain domain_info world.domains;
          case_table = StrMap.add c case world.case_table;
          cases = CaseMap.add case case_info world.cases;
        }
        in
        world, domain, case
      else
        error_domain_not_extensible ()
    | None, Some _ -> error_case_not_belong ()
    | None, None ->
      let domain = fresh_id () in
      let case = fresh_id () in
      let domain_info = {
        domain_name = dom;
        domain_cases = CaseSet.singleton case;
        domain_extensible = true;
      }
      in
      let case_info = {
        case_name = c;
        case_domain = domain;
      }
      in
      let world = {
        domain_table = StrMap.add dom domain world.domain_table;
        domains = DomainMap.add domain domain_info world.domains;
        case_table = StrMap.add c case world.case_table;
        cases = CaseMap.add case case_info world.cases;
      }
      in
      world, domain, case

let projection_of world (dom : domain) (cases : case list) =
  let { domain_cases; _ } = find_domain_info world dom in
  let cases =
    List.fold_left (fun cases case ->
        if DomainSet.mem case domain_cases
        then CaseSet.add case cases
        else Errors.raise_error "Case does not belong in domain"
      )
      CaseSet.empty
      cases
  in
  DomainMap.singleton dom cases

let projection_of_group (g : group) = g

let is_any_projection (proj : projection) =
  DomainMap.is_empty proj

let projection_includes_domain (proj : projection) (dom : domain) =
  DomainMap.mem dom proj

let projection_includes_case (proj : projection) (dom : domain) (case : case) =
  match DomainMap.find_opt dom proj with
    | None -> false
    | Some cases ->
      CaseSet.is_empty cases || CaseSet.mem case cases

let projection_union (proj1 : projection) (proj2 : projection) =
  DomainMap.union (fun _dom c1 c2 ->
      if CaseSet.is_empty c1 || CaseSet.is_empty c2
      then Some CaseSet.empty
      else Some (CaseSet.union c1 c2))
    proj1
    proj2

let shape_of_projection world (proj : projection) =
  DomainMap.fold (fun dom cases groups ->
      let distinct_cases =
        if CaseSet.is_empty cases
        then
          [(find_domain_info world dom).domain_cases]
        else
          CaseSet.fold (fun case cases -> CaseSet.singleton case :: cases) cases []
      in
      List.map (fun cases ->
          List.map (fun group ->
              DomainMap.add dom cases group)
            groups
        )
        distinct_cases
      |> List.flatten)
    proj
    (shape_of_everything world)

let group_intersection (g1 : group) (g2 : group) =
  DomainMap.merge (fun _dom c1 c2 ->
      match c1, c2 with
      | None, _ | _, None -> None
      | Some c1, Some c2 -> Some (CaseSet.inter c1 c2)
    )
    g1 g2

type 'a disjonction =
  | Disjoint
  | Joint of {
      only_left : 'a;
      only_right : 'a;
      common : 'a;
    }

let group_is_empty (g : group) =
  DomainMap.is_empty g ||
  DomainMap.exists (fun _dom -> CaseSet.is_empty) g

let fracture_group (base : group) (hole : group) =
  let group_distinct_from_hole g =
    group_is_empty (group_intersection g hole)
  in
  let frac_groups =
    DomainMap.fold (fun dom hcs groups ->
        List.map (fun group ->
            if group_distinct_from_hole group then [group] else
              match DomainMap.find_opt dom group with
              | None -> Errors.raise_error "non exhaustive group"
              | Some bcs ->
                let commoncs = CaseSet.inter bcs hcs in
                let in_range = DomainMap.add dom commoncs group in
                let onlybcs = CaseSet.diff bcs commoncs in
                let out_range = DomainMap.add dom onlybcs group in
                let frac_group =
                  if group_is_empty out_range then []
                  else [out_range]
                in
                if group_is_empty in_range then frac_group
                else in_range::frac_group
          )
          groups
        |> List.flatten)
      hole [base]
  in
  List.filter group_distinct_from_hole frac_groups

let disjoint_group _world (g1 : group) (g2 : group) =
  (* Format.printf "disjoint %a %a@;" (print_group world) g1 (print_group world) g2; *)
  let common = group_intersection g1 g2 in
  (* Format.printf "inter %a@;" (print_group world) common; *)
  if group_is_empty common
  then Disjoint
  else
    let only_left = fracture_group g1 common in
    let only_right = fracture_group g2 common in
    (* Format.printf "left %a right %a@;" (print_shape world) only_left (print_shape world) only_right; *)
    Joint { common = [common]; only_left; only_right }

let group_distinct_of_shape world (s : shape) (g : group) =
  let rec slice_g (only_s, com, only_g) shape_groups frag_g =
    (* Format.printf "only_s: %a@ com: %a@ only_g: %a@ sgroups: %a@ frags: %a@;" *)
    (*   (print_shape world) only_s *)
    (*   (print_shape world) com *)
    (*   (print_shape world) only_g *)
    (*   (print_shape world) shape_groups *)
    (*   (print_shape world) frag_g; *)
    match shape_groups, frag_g with
    | _, [] -> (shape_groups@only_s, com, only_g)
    | [], frag1::frags -> slice_g ([], com, frag1::only_g) only_s frags
    | sg::groups, frag1::frags ->
      let join = disjoint_group world sg frag1 in
      match join with
      | Disjoint -> slice_g (sg::only_s, com, only_g) groups frag_g
      | Joint { common; only_left; only_right; } ->
        let only_s = only_left@only_s in
        let com = common@com in
        match only_right with
        | [] -> slice_g ([], com, only_g) (only_s@groups) frags
        | _::_ -> slice_g (only_s, com, only_g) groups (only_right@frags)
        (* due to the invariant, only remainder of [g] need to be considered *)
  in
  let (only_left, common, only_right) = slice_g ([], [], []) s [g] in
  if common = [] then Disjoint else Joint { only_left; common; only_right }

let shape_add_disjoint world (s1 : shape) (s2 : shape) =
  List.fold_left (fun shape group ->
      match group_distinct_of_shape world shape group with
      | Disjoint -> group::shape
      | Joint _ -> Errors.raise_error "overlapping shapes"
    )
    s1 s2

let shape_add_precise world (s_base : shape) (s_refine : shape) =
  List.fold_left (fun base group ->
      match group_distinct_of_shape world base group with
      | Disjoint -> group::base
      | Joint { only_left; common; only_right } ->
        (* Format.printf "common %a left %a right %a@;" (print_shape world) common (print_shape world) only_left (print_shape world) only_right; *)
        only_left @ common @ only_right)
    s_base s_refine

let shape_filter_strict_precise world (s_base : shape) (s_filter : shape) =
  let missing_part_error () =
    Errors.raise_error "strict shape filter strict not fully matched"
  in
  List.fold_left (fun filtered fgroup ->
      match group_distinct_of_shape world s_base fgroup with
      | Disjoint -> missing_part_error ()
      | Joint { only_left=_; common; only_right } ->
        (* Format.printf "common %a left %a right %a@;" (print_shape world) common (print_shape world) only_left (print_shape world) only_right; *)
        if only_right <> [] then missing_part_error () else
          common @ filtered
        )
    [] s_filter

(* let shape_filter_strict_loose world (s_base : shape) (s_filter : shape) = *)
(*   let missing_part_error () = *)
(*     Errors.raise_error "strict shape filter loose not fully matched" *)
(*   in *)
(*   List.fold_left (fun filtered fgroup -> *)
(*       match group_distinct_of_shape world s_base fgroup with *)
(*       | Disjoint -> (Format.printf "%a _ (%a)@;" (print_shape world) s_base (print_group world) fgroup; missing_part_error ()) *)
(*       | Joint { only_left; common; only_right } -> *)
(*         Format.printf "common %a left %a right %a@;" (print_shape world) common (print_shape world) only_left (print_shape world) only_right; *)
(*         if only_right <> [] then missing_part_error () else *)
(*           fgroup::filtered *)
(*           (\* nothing in right, so [fgroup] must be fully (but may be sliced) in [common] *\) *)
(*         ) *)
(*     [] s_filter *)

let shape_envelope (s : shape) =
  List.fold_left (fun env group ->
      DomainMap.union (fun _dom c1 c2 -> Some (CaseSet.union c1 c2)) env group)
    DomainMap.empty s

let extend_projection_to_group (p : projection) (g : group) =
  DomainMap.merge (fun _dom cp cg ->
      match cp, cg with
      | None, None -> None
      | None, Some cg -> Some cg
      | Some _, None -> Errors.raise_error "group is not exhaustive"
      | Some cp, Some cg ->
        if CaseSet.is_empty cp then Some cg else
        if CaseSet.subset cp cg then Some cp else
          Errors.raise_error "projection mismatch")
    p g

let extend_projection_to_shape (p : projection) (s : shape) =
  extend_projection_to_group p (shape_envelope s)

let fit_projection_to_shape (p : projection) (s : shape) =
  [ extend_projection_to_shape p s ]

(* let project_on_shape world (s: shape) (p : projection) = *)
(*   if is_any_projection p then s else *)
(*     let ex_proj = extend_projection_to_shape p s in *)
(*     List.fold_left (fun groups_in_proj group -> *)
(*         match disjoint_group world group ex_proj with *)
(*         | Disjoint -> groups_in_proj *)
(*         | Joint { common; only_left; only_right = _; } -> *)
(*           if only_left <> [] *)
(*           then Errors.raise_error "shape projection mismatch" *)
(*           else common @ groups_in_proj *)
(*       ) *)
(*       [] s *)

let projection_subshape_strict world (s : shape) (p : projection) =
  let ex_proj = extend_projection_to_shape p s in
  match group_distinct_of_shape world s ex_proj with
  | Disjoint -> Errors.raise_error "shape disjoint from projection"
  | Joint { only_left; common; only_right } ->
    if only_right <> []
    then Errors.raise_error "projection too large"
    else if List.length only_left + List.length common = List.length s
    then common
    else Errors.raise_error "shape less precise than projection"

let projection_subshape_inclusive world (s : shape) (p : projection) =
  let ex_proj = extend_projection_to_shape p s in
  List.fold_left (fun groups_inc_proj group ->
      match disjoint_group world group ex_proj with
      | Disjoint -> groups_inc_proj
      | Joint _ -> group::groups_inc_proj)
    [] s

  (* match group_distinct_of_shape world s ex_proj with *)
  (* | Disjoint -> Errors.raise_error "shape disjoint from projection" *)
  (* | Joint { only_left; common; only_right } -> *)
  (*   if only_right <> [] *)
  (*   then Errors.raise_error "projection too large" *)
  (*   else if List.length only_left + List.length common = List.length s *)
  (*   then common *)
  (*   else Errors.raise_error "shape less precise than projection" *)


(* let domains_of_strings world (domains : string list) = *)
(*   List.fold_left (fun domains dom_name -> *)
(*       let dom = find_domain world dom_name in *)
(*       let { domain_cases; _ } = find_domain_info world dom in *)
(*       DomainMap.add dom domain_cases domains *)
(*     ) *)
(*     DomainMap.empty *)
(*     domains *)

(* let string_of_cases world (cases : CaseSet.t) = *)
(*   CaseSet.fold (fun case str -> *)
(*       let { case_name; _ } = CaseMap.find case world.cases in *)
(*       str ^ "_" ^ case_name *)
(*     ) cases "" *)
