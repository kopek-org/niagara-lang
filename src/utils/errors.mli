include module type of Logs

val loc_tag : Pos.t Tag.def

type kind =
  | Primary
  | Secondary
  | Note
  | Hint

type info = private {
  kind : kind;
  loc : Pos.t;
  msg : string;
}

val info_tag : info Tag.def

type detail

val pack : detail list -> Tag.set
val ( !! ) : detail list -> Tag.set

val loc : Pos.t -> detail

val primary : ?loc:Pos.t -> ('a, Format.formatter, unit, detail) format4 -> 'a
val secondary : ?loc:Pos.t -> ('a, Format.formatter, unit, detail) format4 -> 'a
val note : ?loc:Pos.t -> ('a, Format.formatter, unit, detail) format4 -> 'a
val hint : ?loc:Pos.t -> ('a, Format.formatter, unit, detail) format4 -> 'a


val raise_error : 
  ?with_pos:Pos.t -> 
  ?span:string -> 
  ('a, Format.formatter, unit, unit, unit, 'b) format6 -> 'a

val init : ?reporter:Logs.reporter -> unit -> unit