
let raise_error ?(with_pos=Pos.dummy) ?span fmt =
  Printf.kfprintf
    (fun fmt ->
       Printf.fprintf fmt " %a:\n" Pos.print with_pos;
       (match span with
        | None -> ()
        | Some span -> Printf.fprintf fmt "%s\n" span);
       exit 1)
    stderr
    ("[error] "^^fmt)
