open CalendarLib

module Date = struct
  include Date

  let encoding =
    let open Json_encoding in
    conv
      (fun d ->
         Printf.sprintf
           "%04u-%02u-%02u" (year d) (int_of_month (month d)) (day_of_month d))
      (fun s ->
         match String.split_on_char '-' s with
         | [y;m;d] -> make (int_of_string y) (int_of_string m) (int_of_string d)
         | _ -> failwith ("Not an ISO 8601 date: "^s))
      string
end

module Duration = struct
  include Date.Period

  let encoding =
    let open Json_encoding in
    conv
      (fun d -> let y,m,d = ymd d in Printf.sprintf "P%04uY%02uM%02uD" y m d)
      (fun s -> Scanf.sscanf s "P%uY%uM%uD" make)
      string
end
