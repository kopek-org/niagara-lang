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

(** [parse_lexbuf ~literate lexbuf k] parses the lexbuf content representing
    a Niagara program [p] and returns [k p].

    @param literate If [lexbuf]'s content is composed with Mardown's blocks,
                    compiles only (and all) {v niagara v} code blocks, ignoring
                    other content.*)
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

(** [parse_program path] parses the file at [path] and returns the
    corresponding Niagara program. If the filename ends with {v .nga v}, it
    treated as a Niagara source file or a Markdown text file if not.

    @see {!parse_lexbuf} *)
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

(** [parse_string ~literate str] returns the Niagara program corresponding
    to [str]. [str] content is treated as Mardown if [literate] is [true]
    or as a Niagara source else.

    @see {!parse_lexbuf} *)
let parse_string : ?literate:bool -> string -> Ast.source Ast.program =
  fun ?literate str ->
    let lexbuf = Sedlexing.Utf8.from_string str in
    parse_lexbuf ?literate lexbuf accept


(** [parse_gen ~literate gen] returns the Niagara program read from the
    character generator [gen]. The resulting content is treated as Mardown if
    [literate] is [true] or as a Niagara source else.

    @see {!parse_lexbuf} *)
let parse_gen : ?literate:bool -> char Gen.t -> Ast.source Ast.program =
  fun ?literate gen ->
    let lexbuf = Sedlexing.Utf8.from_gen gen in
    parse_lexbuf ?literate lexbuf (fun program -> program)
