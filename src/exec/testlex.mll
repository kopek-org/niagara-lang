{
  open Lexing
  open Grammar

  let parse_init_line var for_opp value =
   let open Interpreter.Initialization in
   let name, spec =
     let Surface.Ast.{ named_desc; _ } =
       ParserMain.parse_string
        ~entry:Parser.Incremental.named_start var
     in
     match named_desc with
     | Name (n, ctx)
     | Holder { holder_desc = Pool (n, ctx); _ } ->
        let context =
          List.filter_map (fun cri -> match cri.Surface.Ast.cri_desc with
            | Surface.Ast.CFullDomain _ -> None
            | CCase c -> Some c)
            ctx
        in
        n, if context = [] then Bare else Ctx context
     | Holder { holder_desc = Actor {
                   actor_desc = PlainActor n; _
                  }; _
              } ->
         n, Bare
     | Holder { holder_desc = Actor {
                   actor_desc = LabeledActor (n, l); _
                  }; _
              } ->
         n, Label l
   in
   let for_opp =
     Option.map (fun target ->
       let Surface.Ast.{ named_desc; _ } =
         ParserMain.parse_string
          ~entry:Parser.Incremental.named_start target
       in
       match named_desc with
       | Name (n, []) -> n
       | _ -> raise (Failure "Not a opposition target"))
      for_opp
   in
   { name; spec; for_opp; value }

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
  | (num as i) space* ':'
      {
        let id = int_of_string i in
        input id lines lexbuf
      }
  | "init" { init lines lexbuf }
  | eof { lines }
  | _ { raise (Failure "Syntax error") }

and input id lines = parse
  | ([^'\n']+ as input) "+=" space* ([^'\n']+ as value)
      {
        let name, ctx =
            ParserMain.parse_string
             ~entry:Parser.Incremental.raw_pool_start input
        in
        let context =
         List.filter_map (fun cri -> match cri.Surface.Ast.cri_desc with
          | Surface.Ast.CFullDomain _ -> None
          | CCase c -> Some c)
          ctx
        in
        new_line lexbuf;
        let lines =
         fst lines,
         Interpreter.Input.{id; name; context; value }::(snd lines)
        in
        main lines lexbuf
      }
  | _ { raise (Failure "Syntax error") }

and init lines = parse
  | ([^'\n']+ as var) space+ "for" space+ ([^'\n']+ as opp) "=" space* ([^'\n']+ as value)
      {
        let init_line = parse_init_line var (Some opp) value in
        let lines =
         init_line::(fst lines),
         snd lines
        in
        main lines lexbuf
      }
  | ([^'\n']+ as var) "=" space* ([^'\n']+ as value)
      {
        let init_line = parse_init_line var None value in
        let lines =
         init_line::(fst lines),
         snd lines
        in
        main lines lexbuf
      }
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
      let inputs = main ([], []) lexbuf in
      close_in ic;
      inputs
    with e -> close_in ic; print_error_pos lexbuf; raise e
}
