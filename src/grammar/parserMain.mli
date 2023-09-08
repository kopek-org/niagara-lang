open Surface

(** [parse_lexbuf ~literate lexbuf k] parses the lexbuf content representing
    a Niagara program [p] and returns [k p].

    @param literate If [lexbuf]'s content is composed with Mardown's blocks,
                    compiles only (and all) {v niagara v} code blocks, ignoring
                    other content.*)
val parse_lexbuf :
  ?literate:bool
  -> Sedlexing.lexbuf
  -> (Ast.source Ast.program -> 'a)
  -> 'a

(** [parse_program path] parses the file at [path] and returns the
    corresponding Niagara program. If the filename ends with {v .nga v}, it
    treated as a Niagara source file or a Markdown text file if not.

    @see {!parse_lexbuf} *)
val parse_program : string -> Ast.source Ast.program

(** [parse_string ~literate str] returns the Niagara program corresponding
    to [str]. [str] content is treated as Mardown if [literate] is [true]
    or as a Niagara source else.

    @see {!parse_lexbuf} *)
val parse_string : ?literate:bool -> string -> Ast.source Ast.program

(** [parse_gen ~literate gen] returns the Niagara program read from the
    character generator [gen]. The resulting content is treated as Mardown if
    [literate] is [true] or as a Niagara source else.

    @see {!parse_lexbuf} *)
val parse_gen : ?literate:bool -> char Gen.t -> Ast.source Ast.program
