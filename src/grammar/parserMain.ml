open Surface

module I = Parser.MenhirInterpreter

let accept program = program

let fail checkpoint =
  match checkpoint with
  | I.HandlingError env ->
    let start, stop = I.positions env in
    Format.eprintf "On state %d@." (I.current_state_number env);
    Errors.raise_error "Parsing error" ~with_pos:(Pos.Text.make ~start ~stop)
      ~span:(ParserErrors.message (I.current_state_number env))
  | _ -> assert false

let parse_lexbuf :
  entry:(Lexing.position -> 'a I.checkpoint) -> ?literate:bool
  -> Sedlexing.lexbuf -> ('a -> 'b) -> 'b =
  fun ~entry ?(literate = false) lexbuf k ->
    let lexer = match literate with
      | true -> Lexer.text_and_code ()
      | false -> Lexer.program_file () in
    let init_checkpoint =
      entry (fst @@ Sedlexing.lexing_positions lexbuf) in
    let program = I.loop_handle
      accept fail (Sedlexing.with_tokenizer lexer lexbuf) init_checkpoint in
    k program

let parse_program : string -> Ast.source Ast.program = fun filename ->
  let file_chan = open_in filename in
  let lexbuf = Sedlexing.Utf8.from_channel file_chan in
  Sedlexing.set_filename lexbuf filename;
  let literate = not @@ String.equal (Filename.extension filename) ".nga" in
  try parse_lexbuf ~entry:Parser.Incremental.program ~literate lexbuf (fun program ->
    close_in file_chan;
    program
  ) with exn ->
    close_in file_chan;
    raise exn

let parse_string : entry:(Lexing.position -> 'a I.checkpoint) -> ?literate:bool
  -> string -> 'a =
  fun ~entry ?literate str ->
    let lexbuf = Sedlexing.Utf8.from_string str in
    parse_lexbuf ~entry ?literate lexbuf accept

let parse_gen : entry:(Lexing.position -> 'a I.checkpoint) -> ?literate:bool
  -> char Gen.t -> 'a =
  fun ~entry ?literate gen ->
    let lexbuf = Sedlexing.Utf8.from_gen gen in
    parse_lexbuf ~entry ?literate lexbuf (fun program -> program)
