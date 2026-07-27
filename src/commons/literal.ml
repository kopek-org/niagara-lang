type t =
  | LInteger of Z.t
  | LRational of R.t
  | LMoney of R.t
  | LDate of Date.Date.t
  | LDuration of Date.Duration.t

let print fmt (l : t) =
  match l with
  | LInteger i -> Z.pp_print fmt i
  | LRational f ->  R.print_dec_approx fmt f
  | LMoney i -> Format.fprintf fmt "%a$" R.print_dec_approx i
  | LDate d -> CalendarLib.Printer.Date.fprint "%Y/%m/%d" fmt d
  | LDuration d ->
    let y,m,d = Date.Duration.ymd d in
    Format.fprintf fmt "%d year, %d month, %d day" y m d

let is_zero (l : t) =
  match l with
  | LInteger i -> Z.(equal zero i)
  | LMoney r
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
