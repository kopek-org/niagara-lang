type t =
  | LInteger of Z.t
  | LRational of R.t
  | LMoney of Z.t
  | LDate of Date.Date.t
  | LDuration of Date.Duration.t

let print_money_value fmt (m : Z.t) =
  Format.fprintf fmt "%a.%02d" Z.pp_print Z.(m / ~$100) Z.(to_int (m mod ~$100))

let print fmt (l : t) =
  match l with
  | LInteger i -> Z.pp_print fmt i
  | LRational f -> R.print_dec_approx fmt f
  | LMoney i -> Format.fprintf fmt "%a$" print_money_value i
  | LDate d -> CalendarLib.Printer.Date.fprint "%Y/%m/%d" fmt d
  | LDuration d ->
      let y, m, d = Date.Duration.ymd d in
      Format.fprintf fmt "%d year, %d month, %d day" y m d

let pp_int ?(sep = Fmt.any " ") () ppf i =
  let i, is_neg = Z.(abs i, i < zero) in
  let size = Float.(Z.to_float i |> log10 |> floor |> to_int |> ( + ) 1) in
  let rec aux n ppf i =
    match n with
    | 0 -> Fmt.int ppf (Z.to_int i)
    | _ ->
        Fmt.pf ppf "%a%a%03i"
          (aux (n - 1))
          Z.(i / ~$1000)
          sep ()
          Z.(to_int (i mod ~$1000))
  in
  let n = (size / 3) - if size mod 3 = 0 then 1 else 0 in
  if is_neg then Fmt.pf ppf "-";
  aux n ppf i

let print_rounded_typed_rational :
    ValueType.t -> Format.formatter -> R.t -> unit =
 fun typ fmt r ->
  let ndecimal = match typ with TMoney -> 2 | _ -> 5 in
  let ndec10 = Z.(pow ~$10 ndecimal) in
  let in_z_1 = R.(r >= zero && r <= one) in
  let rounded =
    match typ with
    | TInteger -> R.round r
    | TRational ->
        if in_z_1 then
          (* We print rationals between 0 and 1 like percentages *)
          R.(round (r * ~$$ndec10 * ~$100) / ~$$ndec10)
        else R.(round (r * ~$$ndec10) / ~$$ndec10)
    | TMoney -> R.(round r / ~$100)
    | _ -> assert false
  in
  let dec = R.(to_float (rounded - ~$$(to_bigint rounded))) in
  pp_int () fmt (R.to_bigint rounded);
  (if dec <> 0. then
     let zdecs = Fmt.str "%.*g" ndecimal dec in
     let offset = if R.(r < zero) then 3 else 2 in
     let decs = String.sub zdecs offset (String.length zdecs - offset) in
     let decs =
       if String.length decs = 1 && typ = TMoney then decs ^ "0" else decs
     in
     Fmt.pf fmt ",%s" decs);
  if typ = TMoney then Fmt.pf fmt "\u{00A0}€";
  if typ = TRational && in_z_1 then Fmt.pf fmt "\u{00A0}%%"

let pretty_print : Format.formatter -> t -> unit =
 fun fmt -> function
  | LInteger i -> pp_int () fmt i
  | LRational t -> print_rounded_typed_rational TRational fmt t
  | LMoney m -> print_rounded_typed_rational TMoney fmt R.(of_bigint m)
  | LDate d -> CalendarLib.Printer.Date.fprint "%Y-%m-%d" fmt d
  | LDuration d ->
      let y, m, d = Date.Duration.ymd d in
      Format.fprintf fmt "%d anné(es), %d mois, %d jour(s)" y m d

let is_zero (l : t) =
  match l with
  | LInteger i | LMoney i -> Z.(equal zero i)
  | LRational r -> R.(equal zero r)
  | LDate _ | LDuration _ -> assert false

let type_of (l : t) =
  match l with
  | LInteger _ -> ValueType.TInteger
  | LRational _ -> TRational
  | LMoney _ -> TMoney
  | LDate _ -> TDate
  | LDuration _ -> TDuration
