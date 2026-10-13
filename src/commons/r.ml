(** Extended rationnal module *)

let print_dec_approx fmt t =
  let precision = 5 in
  let int = Q.(to_bigint t) in
  let r = Z.(rem (abs t.num) t.den) in
  let s = Printf.sprintf "%.12g" Q.(to_float Q.(make Z.(t.den + r) t.den)) in
  match String.split_on_char '.' s with
  | [_; sdec] ->
    if String.length sdec > precision then
      Format.fprintf fmt "%a.%s..." Z.pp_print int (String.sub sdec 0 precision)
    else
      Format.fprintf fmt "%a.%s" Z.pp_print int sdec
  | _ -> Z.pp_print fmt int

include Q

let frac r = Z.(rem r.num r.den) /// r.den

let round r =
  let rr = (abs r) + 1//2 in
  (if r < zero then (~-) else (~+)) ~$$(to_bigint rr)

let ceil r =
  let remain = frac r in
  if remain = zero then r else
    let rr = r - remain in
    if r < zero then rr else rr + one

module Map = Map.Make(Q)
