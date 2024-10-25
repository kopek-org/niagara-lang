(* Helpers *)
(** convert to cent amount *)
val parse_money_value : string -> Z.t option
(** recognize only trailing '$' as money amont *)
val parse_money_amount_opt : string -> Z.t option

(** Lexer assuming code in Markdown file, enclosed in '```niagara' *)
val text_and_code : unit -> Sedlexing.lexbuf -> Parser.token

(** Lexer for code only content *)
val program_file : unit -> Sedlexing.lexbuf -> Parser.token
