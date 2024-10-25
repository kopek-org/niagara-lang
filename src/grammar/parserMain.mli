open Surface

(** [parse_lexbuf ~entry ~literate lexbuf k] parses the lexbuf content
    representing a Niagara AST piece [p], produced by [entry], and
    returns [k p].

    @param literate If [lexbuf]'s content is composed with Mardown's blocks,
                    compiles only (and all) {v niagara v} code blocks, ignoring
                    other content.*)
val parse_lexbuf :
  entry:(Lexing.position -> 'a Parser.MenhirInterpreter.checkpoint) ->
  ?literate:bool
  -> Sedlexing.lexbuf
  -> ('a -> 'b)
  -> 'b

(** [parse_program path] parses the file at [path] and returns the
    corresponding Niagara program. If the filename ends with {v .nga v}, it
    treated as a Niagara source file or a Markdown text file if not.

    @see {!parse_lexbuf} *)
val parse_program : string -> Ast.source Ast.program

(** [parse_string ~entry ~literate str] returns the Niagara AST piece
    corresponding to the production of [str] through [entry]. [str]
    content is treated as Mardown if [literate] is [true] or as a
    Niagara source else.

    @see {!parse_lexbuf} *)
val parse_string :
  entry:(Lexing.position -> 'a Parser.MenhirInterpreter.checkpoint) ->
  ?literate:bool -> string -> 'a

(** [parse_gen ~entry ~literate gen] returns the Niagara AST piece
    read from the character generator [gen]. The resulting content is
    treated as Mardown if [literate] is [true] or as a Niagara source
    else.

    @see {!parse_lexbuf} *)
val parse_gen :
  entry:(Lexing.position -> 'a Parser.MenhirInterpreter.checkpoint) ->
  ?literate:bool -> char Gen.t -> 'a
