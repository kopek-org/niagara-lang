(** Extended rationnal module *)

include Q

let round r =
  let rr = (abs r) + 1//2 in
  (if r < zero then (~-) else (~+)) ~$(to_int rr)

let ceil r =
  let remain = Z.(rem r.num r.den) /// r.den in
  if remain = zero then r else
    let rr = r - remain in
    if r < zero then rr - one else rr + one
