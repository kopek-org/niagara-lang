type id_kind =
  | Any
  | Event
  | Partner

type error = private
  | Internal of string
  | Parsing of { loc : Pos.t option; msg : string }
  | Repartition of { pool : Variable.t; rep : R.t }
  | MissingDestination
  | UnknownIdentifier of { loc : Pos.t; id : string; kind : id_kind }
  | MultipleDefRep of { pool : Variable.t }
  | Typing of { loc : Pos.t }
  | User of { locs : Pos.t list; msg : string }

val print_error : ProgramInfo.t -> Format.formatter -> error -> unit

(** This modules adds log related operations based on {!Logs}. *)

include module type of Logs with type 'a Tag.def = 'a Logs.Tag.def

(** {1 Types & Tags} *)

(** Additional message informations. *)
type info = private {
  pinfo : ProgramInfo.t;
  kind : error;
}

(** Meta data tag for additionnal messages.loc

    @warning They are stored in reverse order. *)
val infos_tag : info Tag.def

val raise_internal_error :
  ('a, Format.formatter, unit, unit, unit, 'b) format6 -> 'a

val raise_parsing_error : ?loc:Pos.t -> string -> 'a

val raise_repartition_error : ProgramInfo.t -> Variable.t -> R.t -> 'a

val raise_multiple_def_rep_error : ProgramInfo.t -> Variable.t -> 'a

val raise_missing_dest_error : unit -> 'a

val raise_typing_error : ?loc:Pos.t -> unit -> 'a

val raise_unknown_id_error : ?loc:Pos.t -> string -> id_kind -> 'a

(** Initializes logs. Reporter uses
    standard output/error. For CLI use. *)
val cli_reporting_init : unit -> unit

(** {1. Legacy} *)

(** Legacy function to report errors.

  @deprecated *)

val raise_error :
  ?locs:Pos.t list -> ('a, Format.formatter, unit, unit, unit, 'b) format6 -> 'a
