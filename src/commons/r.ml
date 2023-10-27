(** Extended rationnal module *)

(** Decimal representation of rationnal numbers, with repeatend *)
type dec_repr = { intpart : string; fixdec : string; repeatend : string option }

(* tedious long division to compute repeated *)
let to_dec_repr (rat : Q.t) =
  let substr_from n s = String.sub s n ((String.length s) - n) in
  let open Z in
  let open Z.Compare in
  let Q.{ num = n; den = d } = rat in
  let rec dec_list rest n =
    let q = n / d in
    let r = n mod d in
    let reach_loop = List.exists (fun (_, pr) -> r = pr) rest in
    let rest = (q,r)::rest in
    if r = zero || reach_loop then rest else
      dec_list rest (r * ~$10)
  in
  let intpart = to_string (n/d) in
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


include Q

let frac r = Z.(rem r.num r.den) /// r.den

let round r =
  let rr = (abs r) + 1//2 in
  (if r < zero then (~-) else (~+)) ~$(to_int rr)

let ceil r =
  let remain = frac r in
  if remain = zero then r else
    let rr = r - remain in
    if r < zero then rr else rr + one
