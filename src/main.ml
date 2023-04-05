
let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Needs a file in argument\n";
    exit 1
  end;
  let file = Sys.argv.(1) in
  let _program = Frontend.ParserMain.parse_program file in
  Printf.printf "OK\n%!"
