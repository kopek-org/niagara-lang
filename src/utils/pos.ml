type t = Lexing.position * Lexing.position

let dummy = Lexing.dummy_pos, Lexing.dummy_pos

let print fmt ((s, e) as t) =
  if t = dummy then () else
    let open Lexing in
    Format.fprintf fmt "in file %s, line %d chars %d-%d" s.pos_fname
      s.pos_lnum
      (s.pos_cnum - s.pos_bol)
      (e.pos_cnum - s.pos_bol)
