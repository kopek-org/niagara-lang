
type t = ..

type t += Dummy

let dummy : t = Dummy

module Text = struct

(* This is a simple implementation that will do the job for the time being. *)
type text = Lexing.position * Lexing.position

type t += TextLoc of text

let make : start:Lexing.position -> stop:Lexing.position -> t =
  fun ~start ~stop ->
    TextLoc (start, stop)

let from_lexbuf : Sedlexing.lexbuf -> t =
  fun lexbuf ->
    let start, stop = Sedlexing.lexing_positions lexbuf in
    make ~start ~stop

let gnu_style, set_gnu_style =
  let flag = ref false in
  let gnu_style () = !flag in
  let set_gnu_style b = flag := b in
  gnu_style, set_gnu_style

let pp : t Fmt.t = fun ppf pos ->
  match pos with
  | Dummy -> ()
  | TextLoc (start, stop) ->
      let start_line = start.pos_lnum in
      let start_column = start.pos_cnum - start.pos_bol in
      let stop_line = stop.pos_lnum in
      let stop_column = stop.pos_cnum - stop.pos_bol in
    begin match gnu_style () with
    | true ->
      Fmt.text_loc ppf ((start_line, start_column), (stop_line, stop_column))
    | false ->
      if start_line = stop_line then
        Fmt.pf ppf "line@ %d,@ characters@ %d-%d"
          start_line start_column stop_column
      else
        Fmt.pf ppf "line@ %d,@ character@ %d@ to@ line@ %d,@ character %d"
          start_line start_column stop_line stop_column
    end
  | _ -> Fmt.pf ppf "unknown loc"

let to_lexing_positions t = match t with
  | TextLoc (start, stop) ->
    Some (start, stop)
  | _ -> None
end
