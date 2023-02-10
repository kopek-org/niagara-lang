{
open Parser

let keywords = [
  "cessionnaires", CESSIONNAIRES;
  "cessionnaire", CESSIONNAIRE;
  "sources", SOURCES;
  "source", SOURCE;
  "section", SECTION;
  "taux", TAUX;
  "emet", EMET;
  "jusqua", JUSQUA;
  "puis", PUIS;
  "et", ET;
]

}

let id = ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9''_']*
let section_id = ['1'-'9']+('.'['0'-'9']+)*
let ratio = '0'?'.'['0'-'9']+
let thune = ['0'-'9']+('.'['0'-'9']['0'-'9']?)?
let space = [' ''\t''\r']+

rule main = parse
  | space { main lexbuf }
  | '\n' { Lexing.new_line lexbuf; main lexbuf }
  | ';' { SEMI }
  | ':' { COLON }
  | ratio as f { FLOAT (float_of_string f) }
  | '$'(thune as t) { THUNE (int_of_float (float_of_string t *. 100.)) }
  | id as x
    { match List.assoc_opt x keywords with
      | None -> ID x
      | Some kw -> kw
    }
  | section_id as x { SECTION_ID x }
  | eof { EOF }
  | _ { raise (Failure "syntax error") }

{

}
