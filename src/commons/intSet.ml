include Set.Make (Int)

let encoding = Json_encoding.(conv elements of_list (list int))
