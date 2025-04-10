{
  open Lexing
  open Grammar

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
      "+=" space* ([^'\n']+ as value)
      {
        let id = int_of_string i in
        let name, ctx =
            ParserMain.parse_string
             ~entry:Parser.Incremental.raw_pool input
        in
        let context =
         List.filter_map (fun cri -> match cri.Surface.Ast.cri_desc with
          | Surface.Ast.CFullDomain _ -> None
          | CCase c -> Some c)
          ctx
        in
        new_line lexbuf;
        main (Interpreter.Input.{id; name; context; value }::lines) lexbuf
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
      let inputs = main [] lexbuf in
      close_in ic;
      inputs
    with e -> close_in ic; print_error_pos lexbuf; raise e
}
