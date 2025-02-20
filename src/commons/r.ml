(** Extended rationnal module *)

(** Decimal representation of rationnal numbers, with repeatend *)
type dec_repr = { intpart : string; fixdec : string; repeatend : string option }

(* tedious long division to compute repeatend *)
let to_dec_repr (rat : Q.t) =
  let substr_from n s = String.sub s n ((String.length s) - n) in
  let open Z in
  let open Z.Compare in
  let sign = if Q.(rat < zero) then "-" else "" in
  let Q.{ num = n; den = d } = Q.abs rat in
  let rec dec_list rest n =
    let q = n / d in
    let r = n mod d in
    let reach_loop = List.exists (fun (_, pr) -> r = pr) rest in
    let rest = (q,r)::rest in
    if r = zero || reach_loop then rest else
      dec_list rest (r * ~$10)
  in
  let intpart = sign ^ (to_string (n/d)) in
  let rdec = dec_list [] (abs (n mod d)) in
  match rdec with
  | [] -> { intpart; fixdec = ""; repeatend = None }
  | (q, fr)::rdec ->
    let fixdec, repeatend =
      List.fold_left (fun (fd, rp) (q, r) ->
          if fr = r then (to_string q, Some fd) else
            ((to_string q)^fd, rp))
        (to_string q, None) rdec
    in
    let fixdec = substr_from 1 fixdec in
    { intpart; fixdec; repeatend }

let print_dec_repr fmt dr =
  Format.fprintf fmt "%s.%s" dr.intpart dr.fixdec;
  Option.iter (Format.fprintf fmt "(%s)") dr.repeatend

let print_as_dec_repr fmt r = print_dec_repr fmt (to_dec_repr r)

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
