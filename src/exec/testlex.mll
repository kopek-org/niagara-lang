{
  open Lexing

  let print_error_pos lexbuf =
    Format.eprintf "on line %d character %d :"
      lexbuf.lex_curr_p.pos_lnum
      (lexbuf.lex_curr_p.pos_cnum-lexbuf.lex_curr_p.pos_bol)
}

let space = [' ''\t''\r']+
let num = ['0'-'9']+

rule main lines = parse
  | '\n' { new_line lexbuf; main lines lexbuf }
  | space { main lines lexbuf }
  | (num as i) space* ':' ([^'\n']+ as input)
      "+=" space* ([^'\n']+ as amount)
      {
        let i = int_of_string i in
        new_line lexbuf;
        main (IntMap.add i (input, amount) lines) lexbuf
      }
  | eof { lines }
  | _ { raise (Failure "Syntax error") }

{
  let parse ic =
    let lexbuf = from_channel ic in
    lexbuf.lex_curr_p <- {
        pos_fname = "";
        pos_lnum = 1;
        pos_bol = 0;
        pos_cnum = 0;
      };
    try
      let inputs = main IntMap.empty lexbuf in
      close_in ic;
      inputs
    with e -> close_in ic; print_error_pos lexbuf; raise e
}
