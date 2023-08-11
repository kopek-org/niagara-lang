
let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Needs a file in argument\n";
    exit 1
  end;
  let file = Sys.argv.(1) in
  let outfmt = Format.formatter_of_out_channel stdout in
  let src_program = Frontend.ParserMain.parse_program file in
  Frontend.FormatAst.print_program outfmt src_program;
  Printf.printf "Parsing OK\n%!";
  let ctx_program = Frontend.Contextualize.program src_program in
  Frontend.FormatAst.print_program outfmt ctx_program;
  Printf.printf "Contextualization OK\n%!";
  let prog = Frontend.Ast_to_ir.translate_program ctx_program in
  Frontend.FormatIr.print_program outfmt prog;
  Printf.printf "First pass OK\n%!";

  let provs = Frontend.ConditionLifting.provenance_expressions prog in
  Format.fprintf outfmt "@[<hv 2>Direct provenances:@ %a@]@,"
    Frontend.ConditionLifting.print_provenances provs;
  let tprovs = Frontend.ConditionLifting.prov_transitivity provs in
  Format.fprintf outfmt "@[<hv 2>Transitive provenances:@ %a@]@,"
    Frontend.ConditionLifting.print_provenances tprovs;

  let eqs = Frontend.ConditionLifting.condition_equations prog tprovs in
  Format.fprintf outfmt "@[<hv 2>Equations:@ %a@]@,"
    Frontend.ConditionLifting.print_conditions eqs;

