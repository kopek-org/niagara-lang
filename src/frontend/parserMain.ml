let print_pos fmt (s, e) =
  let open Lexing in
  Printf.fprintf fmt "in file %s, line %d chars %d-%d" s.pos_fname
    s.pos_lnum
    (s.pos_cnum - s.pos_bol)
    (e.pos_cnum - s.pos_bol)

module I = Parser.MenhirInterpreter

let accept _program = Printf.printf "OK\n%!"

let fail checkpoint =
  match checkpoint with
  | I.HandlingError env ->
    Printf.eprintf "Parsing error in %a:\n%s%!"
      print_pos (I.positions env)
      (ParserErrors.message (I.current_state_number env));
    exit 1
  | _ -> assert false

let parse_program filename =
  let file_chan = open_in filename in
  let lexbuf = Sedlexing.Utf8.from_channel file_chan in
  Sedlexing.set_filename lexbuf filename;
  let lexer =
    if String.equal (Filename.extension filename) ".nga"
    then Lexer.program_file ()
    else Lexer.text_and_code ()
  in
  let init_checkpoint =
    Parser.Incremental.program (fst @@ Sedlexing.lexing_positions lexbuf)
  in
  I.loop_handle accept fail (Sedlexing.with_tokenizer lexer lexbuf) init_checkpoint;
  close_in file_chan
