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

let raise_error ?(with_pos=Pos.dummy) ?span fmt =
  Format.kfprintf
    (fun fmt ->
       Format.fprintf fmt " %a:\n" Pos.pp with_pos;
       (match span with
        | None -> ()
        | Some span -> Format.fprintf fmt "%s\n" span);
       failwith "error"(* exit 1 *))
    (Format.formatter_of_out_channel stderr)
    ("[error] " ^^ fmt ^^ "@.")
