(* Helpers *)
val parse_money_value : string -> int option

(** Lexer assuming code in Markdown file, enclosed in '```niagara' *)
val text_and_code : unit -> Sedlexing.lexbuf -> Parser.token

(** Lexer for code only content *)
val program_file : unit -> Sedlexing.lexbuf -> Parser.token
