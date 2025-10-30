include Set.Make (String)

let encoding = Json_encoding.(conv elements of_list (list string))
