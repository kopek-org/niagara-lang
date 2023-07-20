type domain

type case

(* module CaseSet : Set.S with type elt = case *)
(* module CaseMap : Map.S with type key = case *)
(* module DomainSet : Set.S with type elt = domain *)
(* module DomainMap : Map.S with type key = domain *)

type group

type shape

type projection

module GroupMap : Map.S with type key = group

(* type domain_info = { *)
(*   domain_name : string; *)
(*   domain_cases : CaseSet.t; *)
(*   domain_extensible : bool; *)
(* } *)

(* type case_info = { case_name : string; case_domain : domain; } *)

type world(*  = { *)
(*   domains : domain_info DomainMap.t; *)
(*   cases : case_info CaseMap.t; *)
(*   domain_table : domain StrMap.t; *)
(*   case_table : case StrMap.t; *)
(* } *)

val any_projection : projection

val empty_shape : shape

val shape_of_everything : world -> shape

val shape_of_groups : group list -> shape

(* val is_any_shape : world -> shape -> bool *)

val empty_world : world

val fold_shape : ('a -> group -> 'a) -> 'a -> shape -> 'a

val add_domain : world -> string -> string list -> world

val extend_domain : world -> string -> string -> world * domain * case

val find_domain : world -> string -> domain

val find_case : world -> string -> case

val domain_of_case : world -> case -> domain

val projection_of : world -> domain -> case list -> projection

val projection_of_group : group -> projection

val is_any_projection : projection -> bool

val projection_includes_domain : projection -> domain -> bool

val projection_includes_case : projection -> domain -> case -> bool

val projection_union : projection -> projection -> projection

val shape_of_projection : world -> projection -> shape

val fit_projection_to_shape : projection -> shape -> shape

(* val project_on_shape : world -> shape -> projection -> shape *)

val projection_subshape_strict : world -> shape -> projection -> shape

val projection_subshape_inclusive : world -> shape -> projection -> shape

val shape_add_disjoint : world -> shape -> shape -> shape

val shape_add_precise : world -> shape -> shape -> shape

val shape_filter_strict_precise : world -> shape -> shape -> shape

(* val shape_filter_strict_loose : world -> shape -> shape -> shape *)

val print_projection : world -> Format.formatter -> projection -> unit

val print_shape : world -> Format.formatter -> shape -> unit

(* val domains_of_strings : world -> string list -> CaseSet.t DomainMap.t *)

(* val string_of_cases : world -> CaseSet.t -> string *)
