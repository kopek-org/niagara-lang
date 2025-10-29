open CalendarLib

module Date = struct
  include Date

  let encoding =
    let open Json_encoding in
    conv
      (fun d -> (year d, int_of_month (month d), day_of_month d))
      (fun (y, m, d) -> make y m d)
      (obj3 (req "year" int) (req "month" int) (req "day" int))
end

module Duration = struct
  include Date.Period

  let encoding =
    let open Json_encoding in
    conv
      (fun d -> ymd d)
      (fun (y, m, d) -> make y m d)
      (obj3 (req "year" int) (req "month" int) (req "day" int))
end
