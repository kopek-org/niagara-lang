(*****************************************************************************)
(*                                                                           *)
(*  Copyright (c) 2023 OCamlPro SAS                                          *)
(*                                                                           *)
(* All rights reserved.                                                      *)
(* This source code is licensed under the GNU Affero General Public License  *)
(* version 3 found in the LICENSE.md file in the root directory of this      *)
(* source tree.                                                              *)
(*                                                                           *)
(*****************************************************************************)

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Needs a file in argument\n";
    exit 1
  end;
  let file = Sys.argv.(1) in
  let outfmt = Format.formatter_of_out_channel stdout in
  let src_program = Niagara_ast.ParserMain.parse_program file in
  Niagara_ast.FormatAst.print_program outfmt src_program;
  Printf.printf "Parsing OK\n%!";
  let ctx_program = Niagara_ast.Contextualize.program src_program in
  Niagara_ast.FormatAst.print_program outfmt ctx_program;
  Printf.printf "Contextualization OK\n%!";
  let prog = Niagara_exec.Ast_to_ir.translate_program ctx_program in
  Niagara_exec.FormatIr.print_program outfmt prog;
  Printf.printf "First pass OK\n%!";
  let prog = Niagara_exec.ConditionLifting.compute_threshold_equations prog in
  Printf.printf "Events threshold OK\n%!";
  Niagara_exec.Test_interp.test prog
