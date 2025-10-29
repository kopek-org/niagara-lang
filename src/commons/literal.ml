type t =
  | LInteger of Z.t
  | LRational of R.t
  | LMoney of Z.t
  | LDate of Date.Date.t
  | LDuration of Date.Duration.t

let print_money_value fmt (m : Z.t) =
  Format.fprintf fmt "%a.%02d"
    Z.pp_print Z.(m / ~$100) Z.(to_int (m mod ~$100))

let print fmt (l : t) =
  match l with
  | LInteger i -> Z.pp_print fmt i
  | LRational f ->  R.print_dec_approx fmt f
  | LMoney i -> Format.fprintf fmt "%a$" print_money_value i
  | LDate d -> CalendarLib.Printer.Date.fprint "%Y/%m/%d" fmt d
  | LDuration d ->
    let y,m,d = Date.Duration.ymd d in
    Format.fprintf fmt "%d year, %d month, %d day" y m d

let is_zero (l : t) =
  match l with
  | LInteger i
  | LMoney i -> Z.(equal zero i)
  | LRational r -> R.(equal zero r)
  | LDate _
  | LDuration _ -> assert false

let type_of (l : t) =
  match l with
  | LInteger _ -> ValueType.TInteger
  | LRational _ -> TRational
  | LMoney _ -> TMoney
  | LDate _ -> TDate
  | LDuration _ -> TDuration

let z_encoding = Json_encoding.(conv Z.to_string Z.of_string string)

let encoding =
  let open Json_encoding in
  union
    [
      case
        (obj2
           (req "type" (string_enum ["money",true;"int",false]))
           (req "value" z_encoding))
        (function LMoney m -> Some (true, m) | LInteger m -> Some (false, m) | _ -> None)
        (fun (b, s) -> if b then LMoney s else LInteger s);
      case
        (obj2 (req "type" (constant "rational")) (req "value" R.encoding))
        (function LRational q -> Some ((), q) | _ -> None)
        (fun ((), q) -> LRational q);
      case
        (obj2 (req "type" (constant "date")) (req "value" Date.Date.encoding))
        (function LDate d -> Some ((), d) | _ -> None)
        (fun ((), d) -> LDate d);
      case
        (obj2
           (req "type" (constant "duration"))
           (req "value" Date.Duration.encoding))
        (function LDuration d -> Some ((), d) | _ -> None)
        (fun ((), d) -> LDuration d);
    ]
