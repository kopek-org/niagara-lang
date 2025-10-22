include Map.Make (Int)

let tup_list_encoding enc =
  Json_encoding.conv bindings
    (List.fold_left (fun m (i, v) -> add i v m) empty)
    Json_encoding.(list (tup2 int enc))

let array_encoding enc =
  Json_encoding.conv
    (fun map ->
      match max_binding_opt map with
      | None -> [||]
      | Some (arity, _) ->
          let out = Array.make (succ arity) None in
          let () = iter (fun i v -> out.(i) <- Some v) map in
          out)
    (fun l ->
      snd
        (Array.fold_left
           (fun (i, acc) v ->
             (succ i, match v with Some v -> add i v acc | None -> acc))
           (0, empty) l))
    Json_encoding.(array (option enc))
