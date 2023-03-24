open Parser
open Sedlexing

let keywords =
  [
    "remuneration", REMUNERATION;
    "quotepart", QUOTEPART;
    "bonus", BONUS;
    "source", SOURCE;
    "totalise", TOTALISE;
    "ans", ANS;
    "entree", ENTREE;
    "calculable", CALCULABLE;
    "contextualisee", CONTEXTUALISEE;
    "par", PAR;
    "type", TYPE;
    "entier", ENTIER;
    "rationnel", RATIONNEL;
    "argent", ARGENT;
    "flux", FLUX;
    "sortie", SORTIE;
    "vers", VERS;
    "pour", POUR;
    "couloir", COULOIR;
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
    "section", SECTION;
    "fin", FIN;
  ]

let digit = [%sedlex.regexp? '0' .. '9']
let integer = [%sedlex.regexp? (Plus digit)]
let decimal = [%sedlex.regexp? integer, '.', Opt integer | '.', integer]
let money = [%sedlex.regexp? integer, Opt('.', Rep (digit, 1 .. 2)), '$']
let lident = [%sedlex.regexp? lowercase, Star (lowercase | uppercase | '_')]
let uident = [%sedlex.regexp? uppercase, Star (lowercase | uppercase | '_')]

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
  | _ -> raise(Invalid_argument "Lexer.parse_percent")

let rec main lexbuf =
  match%sedlex lexbuf with
  | Plus white_space -> main lexbuf
  | decimal, '%' -> FLOAT (parse_percent (Utf8.lexeme lexbuf))
  | decimal -> FLOAT (float_of_string (Utf8.lexeme lexbuf))
  | integer -> INT (int_of_string (Utf8.lexeme lexbuf))
  | money -> MONEY (parse_money_amount (Utf8.lexeme lexbuf))
  | '+' -> PLUS
  | '-' -> MINUS
  | '*' -> MULT
  | '/' -> DIV
  | '=' -> EQ
  | ':' -> COLON
  | '(' -> LPAR
  | ')' -> RPAR
  | uident -> UIDENT (Utf8.lexeme lexbuf)
  | lident ->
    let id = Utf8.lexeme lexbuf in
    begin match List.assoc_opt id keywords with
    | None -> LIDENT id
    | Some kw -> kw
    end
  | eof -> EOF
  | _ -> assert false
