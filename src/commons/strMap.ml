include Map.Make (String)

let encoding encoding =
  Json_encoding.conv bindings
    (List.fold_left (fun a (k, v) -> add k v a) empty)
    Json_encoding.(assoc encoding)
