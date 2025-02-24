type t =
  | Internal of string
  | Parsing of { loc : Pos.t option; msg : string }
  | User of { locs : Pos.t list; msg : string }

val print_raw : Format.formatter -> t -> unit

(** This modules adds log related operations based on {!Logs}. *)

include module type of Logs with type 'a Tag.def = 'a Logs.Tag.def

(** {1 Types & Tags} *)

(** Additional message informations. *)
type info = private {
  kind : t;
}

(** Meta data tag for additionnal messages.loc

    @warning They are stored in reverse order. *)
val infos_tag : info Tag.def

val raise_internal_error :
  ('a, Format.formatter, unit, unit, unit, 'b) format6 -> 'a

val raise_parsing_error : ?loc:Pos.t -> string -> 'a

(** Initializes logs. Reporter uses
    standard output/error. For CLI use. *)
val cli_reporting_init : unit -> unit

(** {1. Legacy} *)

(** Legacy function to report errors.

  @deprecated *)

val raise_error :
  ?locs:Pos.t list -> ('a, Format.formatter, unit, unit, unit, 'b) format6 -> 'a
