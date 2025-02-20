

(** This module gives positions related oprations.

    Positions in the source code are tracked in order to return useful
    informations to user. *)

(** Position type with generalized payload *)
type t

(** A dummy position corresponding to unspecified position. *)
val dummy : t

module Text : sig

  (** Source code location *)
  type text = {
    start_line : int;
    start_column : int;
    stop_line : int;
    stop_column : int;
  }

  (** [make ~start ~stop] creates a new position according to
      [start] and [stop] {!Lexing.position}s. *)
  val make : start:Lexing.position -> stop:Lexing.position -> t

  (** [from_lexbuf lexbuf] returns a position from the
      given {!Sedlexing.lexbuf}. *)
  val from_lexbuf : Sedlexing.lexbuf -> t

  (** Sets the GNU conventions for printing positions. *)
  val set_gnu_style : bool -> unit

  (** Position pretty-printer. *)
  val pp : t Fmt.t

end
