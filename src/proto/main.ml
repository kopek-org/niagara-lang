

let parse_inputs input_string =
  let input_list = String.split_on_char ',' input_string in
  List.fold_left
    (fun inputs input_string ->
       match String.split_on_char '=' input_string with
       | [id;value] -> begin
           match inputs, int_of_string_opt (String.trim value) with
           | None, _
           | _, None -> None
           | Some inputs, Some v -> Some (Niagara.StrMap.add (String.trim id) (v*100) inputs)
         end
       | _ -> None
    )
    (Some Niagara.StrMap.empty)
    input_list

let _ =
  let file = Sys.argv.(1) in
  let fd = open_in file in
  let lexbuf = Lexing.from_channel fd in
  let fall =
    try Parser.program Lexer.main lexbuf
    with
    | e ->
      let open Lexing in
      let lnum = lexbuf.lex_start_p.pos_lnum in
      let csnum = lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol  in
      let cenum = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
      Printf.eprintf "Error during parsing: line %d, %d:%d\n" lnum csnum cenum;
      raise e
  in
  let rec eval_loop state =
    let output_diff state state' =
     Niagara.StrMap.merge
       (fun _ v v' ->
       match v, v' with
         | None, None -> None
         | None, Some v -> Some v
         | Some _, None -> assert false
         | Some v, Some v' -> Some (v'-v)
       )
       state.Niagara.outputed
       state'.Niagara.outputed
    in
    let print_output output =
      Niagara.StrMap.iter
        (fun id v ->
           Printf.printf "Emissaire %s = %d.%d\n"
             id (v / 100) (v mod 100))
        output
    in
    try
      match parse_inputs (read_line ()) with
      | None -> (Printf.eprintf "Input error\n%!"; eval_loop state)
      | Some inputs -> begin
          let new_state = Niagara.eval_with_state fall inputs state in
          print_output (output_diff state new_state);
          eval_loop new_state
        end
    with
    | End_of_file ->
      Printf.printf "Total:\n";
      print_output state.outputed;
      exit 0
  in
  eval_loop Niagara.init_state
