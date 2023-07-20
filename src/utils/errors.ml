
let raise_error ?(with_pos=Pos.dummy) ?span fmt =
  Format.kfprintf
    (fun fmt ->
       Format.fprintf fmt " %a:\n" Pos.print with_pos;
       (match span with
        | None -> ()
        | Some span -> Format.fprintf fmt "%s\n" span);
       failwith "error"(* exit 1 *))
    (Format.formatter_of_out_channel stderr)
    ("[error] " ^^ fmt ^^ "@.")
