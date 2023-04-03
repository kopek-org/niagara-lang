
let raise_error ?(with_pos=Pos.dummy) ?span msg =
  Printf.eprintf "[error] %a %a:\n"
    Printf.fprintf msg
    Pos.print with_pos;
  (match span with
   | None -> ()
   | Some span -> Printf.eprintf "%s\n" span);
  exit 1

