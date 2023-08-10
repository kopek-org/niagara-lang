module I = Parser.MenhirInterpreter

let accept program = program

let fail checkpoint =
  match checkpoint with
  | I.HandlingError env ->
    let start, stop = I.positions env in
    Errors.raise_error "Parsing error" ~with_pos:(Pos.make ~start ~stop)
      ~span:(ParserErrors.message (I.current_state_number env))
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
  let program =
    I.loop_handle accept fail (Sedlexing.with_tokenizer lexer lexbuf) init_checkpoint
  in
  close_in file_chan;
  program
