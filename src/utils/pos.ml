

(* This is a simple implementation that will do the job for the time being. *)
type t =
  | Dummy
  | Loc of {
    start_line : int;
    start_column : int;
    stop_line : int;
    stop_column : int;
  } 

let dummy : t = Dummy

let make : start:Lexing.position -> stop:Lexing.position -> t =
  fun ~start ~stop ->
    Loc {
      start_line = start.pos_lnum;
      start_column = start.pos_cnum - start.pos_bol;
      stop_line = stop.pos_lnum;
      stop_column = stop.pos_cnum - stop.pos_bol;
    }

let from_lexbuf : Sedlexing.lexbuf -> t =
  fun lexbuf ->
    let start, stop = Sedlexing.lexing_positions lexbuf in
    make ~start ~stop

let pp : t Fmt.t = fun ppf pos ->
  match pos with
  | Dummy -> ()
  | Loc { start_line; start_column; stop_line; stop_column } ->
    if start_line = stop_line then
      Fmt.pf ppf "line@ %d,@ characters@ %d-%d" 
        start_line start_column stop_column
    else
      Fmt.pf ppf "line@ %d,@ character@ %d@ to@ line@ %d,@ character %d" 
        start_line start_column stop_line stop_column