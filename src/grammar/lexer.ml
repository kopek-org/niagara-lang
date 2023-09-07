open Sedlexing
open Parser

let unknown_token_error lexbuf =
  Errors.raise_error "Parsing error"
    ~with_pos:(Pos.from_lexbuf lexbuf)
    ~span:"unknown token"

let keywords =
  [
    "operation", OPERATION;
    "quotepart", QUOTEPART;
    "bonus", BONUS;
    "sur", SUR;
    "assiette", ASSIETTE;
    "ans", ANS;
    "mois", MOIS;
    "entree", ENTREE;
    "contextualisee", CONTEXTUALISEE;
    "par", PAR;
    "type", TYPE;
    "entier", ENTIER;
    "rationnel", RATIONNEL;
    "argent", ARGENT;
    "acteur", ACTEUR;
    "vers", VERS;
    "pour", POUR;
    "evenement", EVENEMENT;
    "atteint", ATTEINT;
    "et", ET;
    "ou", OU;
    "avant", AVANT;
    "apres", APRES;
    "quand", QUAND;
    "contexte", CONTEXTE;
    "tout", TOUT;
    "constante", CONSTANTE;
    "defaut", DEFAUT;
    "deficit", DEFICIT;
    "avance", AVANCE;
    "total", TOTAL;
    "courant", COURANT;
    "retrocession", RETROCESSION;
    "montant", MONTANT;
    (* "opposable", OPPOSABLE; *)
    (* "section", SECTION; *)
    (* "fin", FIN; *)
  ]

let digit = [%sedlex.regexp? '0' .. '9']
let integer = [%sedlex.regexp? (Plus digit)]
let decimal = [%sedlex.regexp? integer, '.', Opt integer | '.', integer]
let money = [%sedlex.regexp? integer, Opt('.', Rep (digit, 1 .. 2)), '$']
let lident = [%sedlex.regexp? lowercase, Star (digit | lowercase | uppercase | '_')]
let uident = [%sedlex.regexp? uppercase, Star (digit | lowercase | uppercase | '_')]
let date = [%sedlex.regexp? integer, '/', integer, '/', integer]
let comment = [%sedlex.regexp? '#', Star (Compl '\n'), '\n']
let label = [%sedlex.regexp? '\'', Plus (Compl ('\n' | '\r' | '\'')), '\'']

let parse_money_amount s =
  let invalid_arg = Invalid_argument "Lexer.parse_money_amount" in
  match String.split_on_char '$' s with
  | [s;""] -> begin
      match String.split_on_char '.' s with
      | [intpart] -> (int_of_string intpart) * 100
      | [intpart; decpart] -> int_of_string (intpart^decpart)
      | _ -> raise invalid_arg
    end
  | _ -> raise invalid_arg

let parse_percent s =
  match String.split_on_char '%' s with
  | [s;""] -> (float_of_string s) /. 100.
  | _ -> raise (Invalid_argument "Lexer.parse_percent")

let parse_date s =
  match String.split_on_char '/' s with
  | [y; m; d] ->
    Date.Date.make (int_of_string y) (int_of_string m) (int_of_string d)
  | _ -> raise (Invalid_argument "Lexer.parse_date")

let strip_enclosing_chars s =
  let s = String.trim s in
  String.sub s 1 (String.length s - 2)

let reading_code = ref false

let rec code ~is_in_text lexbuf =
  if not !reading_code then in_text lexbuf else
  match%sedlex lexbuf with
  | Plus white_space | comment -> code ~is_in_text lexbuf
  | date -> DATE (parse_date (Utf8.lexeme lexbuf))
  | (integer | decimal), '%' -> FLOAT (parse_percent (Utf8.lexeme lexbuf))
  | decimal -> FLOAT (float_of_string (Utf8.lexeme lexbuf))
  | integer -> INT (int_of_string (Utf8.lexeme lexbuf))
  | money -> MONEY (parse_money_amount (Utf8.lexeme lexbuf))
  | label -> LABEL (strip_enclosing_chars (Utf8.lexeme lexbuf))
  | '+' -> PLUS
  | '-' -> MINUS
  | '*' -> MULT
  | '/' -> DIV
  | '=' -> EQ
  | ':' -> COLON
  | '(' -> LPAR
  | ')' -> RPAR
  | '[' -> LBRA
  | ']' -> RBRA
  | ',' -> COMMA
  | uident -> UIDENT (Utf8.lexeme lexbuf)
  | lident ->
    let id = Utf8.lexeme lexbuf in
    begin match List.assoc_opt id keywords with
    | None -> LIDENT id
    | Some kw -> kw
    end
  | "```" ->
    if is_in_text
    then (reading_code := false; in_text lexbuf)
    else unknown_token_error lexbuf
  | eof ->
    if is_in_text
    then Errors.raise_error "Unclosed code section at end of file"
        ~with_pos:(Pos.from_lexbuf lexbuf)
    else EOF
  | _ -> unknown_token_error lexbuf

and in_text lexbuf =
  if !reading_code then code ~is_in_text:true lexbuf else
  match%sedlex lexbuf with
  | "```niagara" -> reading_code := true; code ~is_in_text:true lexbuf
  | any -> in_text lexbuf
  | eof -> EOF
  | _ -> assert false

let text_and_code () = reading_code := false; code ~is_in_text:true

let program_file () = reading_code := true; code ~is_in_text:false
