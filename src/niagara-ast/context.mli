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

type domain

type case

module CaseSet : Set.S with type elt = case
(* module CaseMap : Map.S with type key = case *)
(* module DomainSet : Set.S with type elt = domain *)
module DomainMap : Map.S with type key = domain

module Group : sig
  type t

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t

  val empty : t

  val not : t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

end

type shape

(* type projection *)

(* module GroupMap : Map.S with type key = group *)

(* type domain_info = { *)
(*   domain_name : string; *)
(*   domain_cases : CaseSet.t; *)
(*   domain_extensible : bool; *)
(* } *)

(* type case_info = { case_name : string; case_domain : domain; } *)

type desc = CaseSet.t DomainMap.t list

type world(*  = { *)
(*   domains : domain_info DomainMap.t; *)
(*   cases : case_info CaseMap.t; *)
(*   domain_table : domain StrMap.t; *)
(*   case_table : case StrMap.t; *)
(* } *)

val any_projection : world -> Group.t

val empty_shape : shape

val shape_of_everything : world -> shape

val shape_perimeter : shape -> Group.t

val shape_project : shape -> Group.t -> shape

val shape_clip : shape -> shape -> shape

val shape_imprint_projection : shape -> Group.t -> shape

val shape_overlap_subshape : shape -> Group.t -> shape

val shape_cut_out : shape -> Group.t -> shape

val shape_of_groups : Group.t list -> shape

val shape_fold : ('a -> Group.t -> 'a) -> 'a -> shape -> 'a

(* val is_any_shape : world -> shape -> bool *)

val empty_world : world

(* val proj_filter_extensible : world -> projection -> projection *)

(* val proj_filter_non_extensible : world -> projection -> projection *)

(* val shape_filter_extensible : world -> shape -> shape *)

(* val shape_filter_non_extensible : world -> shape -> shape *)

val add_domain : world -> string -> string list -> world

(* val extend_domain : world -> string -> string -> world * domain * case *)

val find_domain : world -> string -> domain

val find_case : world -> string -> case

val case_is_in_domain : world -> case -> domain -> bool

val group_of_selection : world -> CaseSet.t DomainMap.t -> Group.t

val domain_of_case : world -> case -> domain

(* val projection_of : world -> domain -> case list -> projection *)

(* val projection_of_group : group -> projection *)

val is_any_projection : world -> Group.t -> bool

(* val projection_includes_domain : world -> Group.t -> domain -> bool *)

(* val projection_includes_case : world -> Group.t -> case -> bool *)

(* val projection_union : projection -> projection -> projection *)

(* val shape_of_projection : world -> projection -> shape *)

(* val fit_projection_to_shape : projection -> shape -> shape *)

(* val project_on_shape : world -> shape -> projection -> shape *)

(* val projection_subshape_strict : world -> shape -> projection -> shape *)

(* val projection_subshape_inclusive : world -> shape -> projection -> shape *)

(* val shape_add_disjoint : world -> shape -> shape -> shape *)

(* val shape_add_precise : world -> shape -> shape -> shape *)

(* val shape_filter_strict_precise : world -> shape -> shape -> shape *)

(* val shape_filter_strict_loose : world -> shape -> shape -> shape *)

val group_desc : world -> Group.t -> desc

val print_group_id : Format.formatter -> Group.t -> unit

val print_group_desc : world -> Format.formatter -> desc -> unit

val print_projection : world -> Format.formatter -> Group.t -> unit

val print_shape : world -> Format.formatter -> shape -> unit

val print_world_desc : Format.formatter -> world -> unit

(* val domains_of_strings : world -> string list -> CaseSet.t DomainMap.t *)

(* val string_of_cases : world -> CaseSet.t -> string *)
