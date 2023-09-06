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

open Ast

let print_type fmt (t : ValueType.t) =
  Format.fprintf fmt "%s"
  (match t with
  | ValueType.TInteger -> "entier"
  | ValueType.TRational -> "rationel"
  | ValueType.TMoney -> "argent"
  | ValueType.TEvent -> "evenement"
  | ValueType.TDate -> "date"
  | ValueType.TDuration -> "duree")

let print_context fmt (ctx : context) =
  match ctx with
  | Forall dom -> Format.fprintf fmt "tout %s" dom
  | Cases (dom, cases) ->
    Format.fprintf fmt "%s(%a)" dom
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',')
         Format.pp_print_string)
      cases

let print_input_context fmt (ctx : context list list) =
  if ctx = [] then () else begin
    Format.fprintf fmt "contextualisee par@ @[<v>";
    List.iter (fun ctx ->
      Format.fprintf fmt "@[<h>- %a@]@,"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         print_context) ctx)
      ctx;
    Format.fprintf fmt "@]"
  end

let print_context_refinement_item fmt (item : context_refinement_item) =
  match item.cri_desc with
  | CCase c -> Format.pp_print_string fmt c
  | CFullDomain d -> Format.fprintf fmt "tout %s" d

let print_context_refinement fmt (ctx : context_refinement) =
  if ctx <> [] then
    Format.fprintf fmt "@[<hv>(%a)@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
      print_context_refinement_item) ctx

let print_op_context fmt (ctx : context list) =
  List.iter (fun ctx ->
      Format.fprintf fmt "pour %a@;" print_context ctx)
    ctx

let print_actor fmt (a : actor) =
  match a.actor_desc with
  | PlainActor name ->
    Format.fprintf fmt "%s" name
  | LabeledActor (name, label) ->
    Format.fprintf fmt "%s[%s]" name label

let print_holder fmt (holder : holder) =
  match holder.holder_desc with
  | Pool (name, ctx) ->
    Format.fprintf fmt "assiette %s%a" name
      print_context_refinement ctx
  | Actor a -> print_actor fmt a

let print_op_source fmt (src : holder) =
  Format.fprintf fmt "%s %a@;"
    (match src.holder_desc with
     | Pool (_, _) -> "sur"
     | Actor _ -> "par")
    print_holder src

let print_destination fmt (dest : holder) =
  Format.fprintf fmt "vers %a" print_holder dest

let print_variable infos fmt (v : Variable.t) =
  let vinfos = Variable.Map.find v infos.var_info in
  Format.fprintf fmt "%s/%d" vinfos.var_name v

let print_ctx_variable infos fmt ((v, proj) : contextualized_variable) =
  Format.fprintf fmt "@[<hv 2>%a@,%a@]" (print_variable infos) v
    (Context.print_projection infos.contexts) proj

let print_literal fmt (lit : literal) =
  match lit with
  | LitInt i -> Format.fprintf fmt "%d" i
  | LitRational f -> Format.fprintf fmt "%f" f
  | LitMoney m -> Format.fprintf fmt "%d.%d$" (m/100) (m mod 100)
  | LitDuration d ->
    let y,m,d = Date.Duration.ymd d in
    Format.fprintf fmt "%d year, %d month, %d day" y m d
  | LitDate d -> CalendarLib.Printer.Date.fprint "%Y/%m/%d" fmt d

let print_named fmt (named : named) =
  match named.named_desc with
  | Name (name, ctx) -> Format.fprintf fmt "%s%a" name print_context_refinement ctx
  | Holder h -> print_holder fmt h

let rec print_formula : type a. program_infos -> Format.formatter -> a formula -> unit =
  fun infos fmt f ->
  match f.formula_desc with
  | Literal l -> print_literal fmt l
  | Named n -> print_named fmt n
  | Variable v -> print_ctx_variable infos fmt v
  | Binop (op, f1, f2) ->
      let op = match op with
      | Add -> "+"
      | Sub -> "-"
      | Mult -> "*"
      | Div -> "/"
      in
      Format.fprintf fmt "@[<hov 2>(%a@ %s %a)@]"
        (print_formula infos) f1 op
        (print_formula infos) f2
  | Total f -> Format.fprintf fmt "(%a) total" (print_formula infos) f
  | Instant f -> Format.fprintf fmt "(%a) courant" (print_formula infos) f

let rec print_event_expr : type a. program_infos -> Format.formatter -> a event_expr -> unit =
  fun infos fmt e ->
  match e.event_expr_desc with
  | EventId id -> Format.fprintf fmt "evenement %s" id
  | EventVar v -> Format.fprintf fmt "evenement %a" (print_variable infos) v
  | EventComp (Eq, f1, f2) ->
    Format.fprintf fmt "@[<hov 2>(%a@ %s %a)@]"
      (print_formula infos) f1 "="
      (print_formula infos) f2
  | EventConj (e1, e2) ->
    Format.fprintf fmt "@[<hov>(%a@ et %a)@]"
      (print_event_expr infos) e1
      (print_event_expr infos) e2
  | EventDisj (e1, e2) ->
    Format.fprintf fmt "@[<hov>(%a@ ou %a)@]"
      (print_event_expr infos) e1
      (print_event_expr infos) e2

let print_redist (type a) infos fmt (redist : a redistribution) =
  match redist.redistribution_desc with
  | Part f -> Format.fprintf fmt "quotepart %a" (print_formula infos) f
  | Flat f -> Format.fprintf fmt "bonus %a" (print_formula infos) f
  | Retrocession (f, p) ->
    Format.fprintf fmt "retrocession %a sur %a"
      (print_formula infos) f
      print_holder p

let print_redistrib_with_dest (type a) infos fmt (r : a redistrib_with_dest) =
  match r with
  | WithHolder (redist, dest) ->
    Format.fprintf fmt "%a %a"
      (print_redist infos) redist
      (Format.pp_print_option print_destination) dest
  | WithVar (redist, dest) ->
    Format.fprintf fmt "%a -> %a"
      (print_redist infos) redist
      (Format.pp_print_option (print_ctx_variable infos)) dest

let rec print_guarded_redistrib :
  type a. Ast.program_infos -> Format.formatter -> a guarded_redistrib -> unit =
  fun infos fmt g_redist ->
  match g_redist with
  | Redists rs ->
    Format.pp_print_list (print_redistrib_with_dest infos) fmt rs
  | Branches { befores; afters } ->
    Format.pp_print_list (fun fmt (c, r)->
        Format.fprintf fmt "@[<v 2>avant %a (@,%a)@]"
          (print_event_expr infos) c (print_guarded_redistrib infos) r)
      fmt befores;
    Format.pp_print_break fmt 0 0;
    Format.pp_print_list (fun fmt (c, r)->
        Format.fprintf fmt "@[<v 2>apres %a (@,%a)@]"
          (print_event_expr infos) c (print_guarded_redistrib infos) r)
      fmt afters
  | Whens gs ->
    Format.pp_print_list (fun fmt (c, r)->
        Format.fprintf fmt "@[<v 2>quand %a (@,%a)@]"
          (print_event_expr infos) c (print_guarded_redistrib infos) r)
      fmt gs

let print_declaration (type a) infos fmt (decl : a declaration) =
  match decl with
  | DContext ctx ->
    Format.fprintf fmt "contexte %s :@;@[<v>%a@]"
      ctx.context_type_name
      (Format.pp_print_list (fun fmt c -> Format.fprintf fmt"- %s" c))
      ctx.context_type_cases
  | DActor a ->
    Format.fprintf fmt "acteur %s" a.actor_decl_desc
  | DInput input ->
    Format.fprintf fmt "entree %s type %a %a"
      input.input_name
      print_type input.input_type
      print_input_context input.input_context
  | DHolderOperation op ->
    Format.fprintf fmt "@[<hv 2>operation '%s' %a@,%a%a%a@]"
      op.op_label
      (Format.pp_print_option print_destination) op.op_default_dest
      print_op_context op.op_context
      print_op_source op.op_source
      (print_guarded_redistrib infos) op.op_guarded_redistrib
  | DVarOperation op ->
    Format.fprintf fmt "@[<hv 2>operation '%s' -> %a@,sur %a@;%a@]"
      op.ctx_op_label
      (Format.pp_print_option (print_ctx_variable infos)) op.ctx_op_default_dest
      (print_ctx_variable infos) op.ctx_op_source
      (print_guarded_redistrib infos) op.ctx_op_guarded_redistrib
  | DHolderEvent e ->
    Format.fprintf fmt "@[<hov>evenement %s@ atteint quand %a"
      e.event_name (print_event_expr infos) e.event_expr
  | DVarEvent e ->
    Format.fprintf fmt "@[<hov>evenement %a@ atteint quand %a"
      (print_variable infos) e.ctx_event_var
      (print_event_expr infos) e.ctx_event_expr
  | DConstant c ->
    Format.fprintf fmt "constante %s : %a" c.const_name print_literal c.const_value
  | DHolderAdvance a ->
    Format.fprintf fmt "@[<hov>avance '%s' sur %a par %a@ montant %a@]"
      a.adv_label print_holder a.adv_output
      print_actor a.adv_provider
      (print_formula infos) a.adv_amount
  | DVarAdvance a ->
    Format.fprintf fmt "@[<hov>avance '%s' sur %a par %a@ montant %a@]"
      a.ctx_adv_label (print_ctx_variable infos) a.ctx_adv_output
      (print_ctx_variable infos) a.ctx_adv_provider
      (print_formula infos) a.ctx_adv_amount
  | DHolderDefault d ->
    Format.fprintf fmt "@[<hv>defaut sur %a@ vers %a@]"
      print_holder d.default_source print_holder d.default_dest
  | DVarDefault d ->
    Format.fprintf fmt "@[<hv>defaut sur %a@ vers %a@]"
      (print_ctx_variable infos) d.ctx_default_source
      (print_ctx_variable infos) d.ctx_default_dest
  | DHolderDeficit d ->
    Format.fprintf fmt "@[<hv>deficit sur %a@ par %a@]"
      print_holder d.deficit_pool print_holder d.deficit_provider
  | DVarDeficit d ->
    Format.fprintf fmt "@[<hv>deficit sur %a@ par %a@]"
      (print_ctx_variable infos) d.ctx_deficit_pool
      (print_ctx_variable infos) d.ctx_deficit_provider

let print_var_contexts infos fmt () =
  Variable.Map.iter (fun v shape ->
      Format.fprintf fmt "@[<hv 2>var %a@ %a@]@;"
        (print_ctx_variable infos) (v, Context.any_projection (infos.contexts))
        (Context.print_shape infos.contexts) shape
    )
    infos.var_shapes

let print_constants infos fmt () =
  Variable.Map.iter (fun v value ->
      Format.fprintf fmt "%a = %a@;"
        (print_variable infos) v
        print_literal value
    )
    infos.constants

let print_program (type a) fmt (p : a program) =
  Format.pp_open_vbox fmt 0;
  begin match p with
    | Source decls ->
      let dummy_infos = {
        var_info = Variable.Map.empty;
        var_shapes = Variable.Map.empty;
        contexts = Context.empty_world;
        inputs = Variable.Map.empty;
        actors = Variable.Map.empty;
        compounds = Variable.Map.empty;
        types = Variable.Map.empty;
        constants = Variable.Map.empty;
      }
      in
      Format.pp_print_list (print_declaration dummy_infos) fmt decls
    | Contextualized (infos, decls) ->
      print_var_contexts infos fmt ();
      print_constants infos fmt ();
      Format.pp_print_list (print_declaration infos) fmt decls
  end;
  Format.fprintf fmt "@."
