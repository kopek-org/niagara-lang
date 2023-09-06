(** This modules adds log related operations based on {!Logs}. *)

include module type of Logs with type 'a Tag.def = 'a Logs.Tag.def

(** {1 Types & Tags} *)

(** Position meta data tag. *)
val loc_tag : Pos.t Tag.def

(** Kind of additional messages. *)
type kind =
  | Primary   (** Primary error source. *)
  | Secondary (** Secondary error source. *)
  | Note      (** An explanation. *)
  | Hint      (** A hint on how to resolve the problem. *)

(** Additional message informations. *)
type info = private {
  kind : kind;
  loc : Pos.t;
  msg : string;
}

(** Meta data tag for additionnal messages.loc
    
    @warning They are stored in reverse order. *)
val infos_tag : info list Tag.def

(** {1 Meta data combinators}*)

(** A convenient carrier type for meta data combinators. *)
type detail

(** Converts details into a tag set usable in logs. *)
val pack : detail list -> Tag.set

(** Syntactic shortcut for {!pack}. *)
val ( !! ) : detail list -> Tag.set

(** [loc p] creates a meta data with the given position. *)
val loc : Pos.t -> detail

(** [primary ~loc fmt] creates a primary additional message according to [fmt]
    and [loc]. *)
val primary : ?loc:Pos.t -> ('a, Format.formatter, unit, detail) format4 -> 'a

(** [secondary ~loc fmt] creates a primary additional message according to [fmt]
    and [loc]. *)
val secondary : ?loc:Pos.t -> ('a, Format.formatter, unit, detail) format4 -> 'a

(** [note ~loc fmt] creates a primary additional message according to [fmt]
    and [loc]. *)
    val note : ?loc:Pos.t -> ('a, Format.formatter, unit, detail) format4 -> 'a

(** [hint ~loc fmt] creates a primary additional message according to [fmt]
    and [loc]. *)
    val hint : ?loc:Pos.t -> ('a, Format.formatter, unit, detail) format4 -> 'a

(** Initializes logs with the given reporter, if any. Default reporter uses
    standard output/error. *)
val init : ?reporter:Logs.reporter -> unit -> unit

(** {1. Legacy} *)

(** Legacy function to report errors.
    
  @deprecated *)
val raise_error : 
  ?with_pos:Pos.t -> 
  ?span:string -> 
  ('a, Format.formatter, unit, unit, unit, 'b) format6 -> 'a

