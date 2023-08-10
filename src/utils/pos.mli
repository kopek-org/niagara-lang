

(** This module gives positions related oprations.  
    
    Positions in the source code are tracked in order to return useful
    informations to user. *)
type t

(** A dummy position corresponding to unspecified position. *)
val dummy : t

(** [make ~start ~stop] creates a new position according to
    [start] and [stop] {!Lexing.position}s. *)
val make : start:Lexing.position -> stop:Lexing.position -> t

(** [from_lexbuf lexbuf] returns a position from the
    given {!Sedlexing.lexbuf}. *)
val from_lexbuf : Sedlexing.lexbuf -> t

(** Position pretty-printer. *)
val pp : t Fmt.t