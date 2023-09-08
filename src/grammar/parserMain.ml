open Surface

module I = Parser.MenhirInterpreter

let accept program = program

let fail checkpoint =
  match checkpoint with
  | I.HandlingError env ->
    let start, stop = I.positions env in
    Errors.raise_error "Parsing error" ~with_pos:(Pos.make ~start ~stop)
      ~span:(ParserErrors.message (I.current_state_number env))
  | _ -> assert false

let parse_lexbuf :
  ?literate:bool -> Sedlexing.lexbuf -> (Ast.source Ast.program -> 'a) -> 'a =
  fun ?(literate = false) lexbuf k ->
    let lexer = match literate with
      | true -> Lexer.text_and_code ()
      | false -> Lexer.program_file () in
    let init_checkpoint =
      Parser.Incremental.program (fst @@ Sedlexing.lexing_positions lexbuf) in
    let program = I.loop_handle
      accept fail (Sedlexing.with_tokenizer lexer lexbuf) init_checkpoint in
    k program

let parse_program : string -> Ast.source Ast.program = fun filename ->
  let file_chan = open_in filename in
  let lexbuf = Sedlexing.Utf8.from_channel file_chan in
  Sedlexing.set_filename lexbuf filename;
  let literate = not @@ String.equal (Filename.extension filename) ".nga" in
  try parse_lexbuf ~literate lexbuf (fun program ->
    close_in file_chan;
    program
  ) with exn ->
    close_in file_chan;
    raise exn

let parse_string : ?literate:bool -> string -> Ast.source Ast.program =
  fun ?literate str ->
    let lexbuf = Sedlexing.Utf8.from_string str in
    parse_lexbuf ?literate lexbuf accept

let parse_gen : ?literate:bool -> char Gen.t -> Ast.source Ast.program =
  fun ?literate gen ->
    let lexbuf = Sedlexing.Utf8.from_gen gen in
    parse_lexbuf ?literate lexbuf (fun program -> program)
