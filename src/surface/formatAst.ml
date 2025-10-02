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

let print_named fmt (named : named) =
  match named.named_desc with
  | Name (name, ctx) -> Format.fprintf fmt "%s%a" name print_context_refinement ctx
  | Holder h -> print_holder fmt h

let print_binop fmt (op : binop) =
  Format.pp_print_string fmt
    (match op with
     | Add -> "+"
     | Sub -> "-"
     | Mult -> "*"
     | Div -> "/")

let rec print_formula : type a. ProgramInfo.t -> Format.formatter -> a formula -> unit =
  fun infos fmt f ->
  match f.formula_desc with
  | Literal l -> Literal.print fmt l
  | Named n -> print_named fmt n
  | Variable v -> ProgramInfo.print_ctx_variable infos fmt v
  | Binop (op, f1, f2) ->
      Format.fprintf fmt "@[<hov 2>(%a@ %a %a)@]"
        (print_formula infos) f1
        print_binop op
        (print_formula infos) f2
  | Total f -> Format.fprintf fmt "(%a) total" (print_formula infos) f
  | Instant f -> Format.fprintf fmt "(%a) courant" (print_formula infos) f
  | Opposed (f, opp) -> Format.fprintf fmt "(%a) %a" (print_formula infos) f (print_opposable infos) opp

and print_opposable : type a. ProgramInfo.t -> Format.formatter -> a opposable -> unit =
  fun infos fmt opposable ->
  match opposable with
  | HolderOpp opposable ->
    Format.fprintf fmt "opposable %a @[<hv>envers %a@ par %a@]"
      (print_formula infos) opposable.opp_value
      print_actor opposable.opp_towards
      print_actor opposable.opp_provider
  | VarOpp opposable ->
    Format.fprintf fmt "opposable %a @[<hv>envers %a@ par %a@]"
      (print_formula infos) opposable.opp_value
      (ProgramInfo.print_variable infos) opposable.opp_towards
      (ProgramInfo.print_variable infos) opposable.opp_provider

let rec print_event_expr : type a.  ProgramInfo.t -> Format.formatter -> a event_expr -> unit =
  fun infos fmt e ->
  match e.event_expr_desc with
  | EventId id -> Format.fprintf fmt "evenement %s" id
  | EventVar v -> Format.fprintf fmt "evenement %a" (ProgramInfo.print_variable infos) v
  | EventComp (Eq, f1, f2) ->
    Format.fprintf fmt "@[<hov 2>%a@ %s %a@]"
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


let print_redist (type a) infos fmt (redist : a redistribution) ~dest =
  match redist.redistribution_desc with
  | Part (f, non_opp) ->
    Format.fprintf fmt "quotepart %a%s %t"
      (print_formula infos) f
      (if non_opp then " non opposable" else "")
      dest
  | Flat f -> Format.fprintf fmt "bonus %a %t" (print_formula infos) f dest
  | Retrocession (f, p) ->
    Format.fprintf fmt "retrocession %a sur %a %t"
      (print_formula infos) f
      print_holder p
      dest
  | Default -> Format.fprintf fmt "quotepart reste %t" dest

let print_redistrib_with_dest (type a) infos fmt (r : a redistrib_with_dest) =
  match r with
  | WithHolder (redist, dest) ->
    let dest = Format.dprintf "%a" (Format.pp_print_option print_destination) dest in
    Format.fprintf fmt "%a"
      (print_redist infos ~dest) redist
  | WithVar (redist, dest) ->
    Format.fprintf fmt "%a -> %a"
      (print_redist infos ~dest:(fun _ -> ())) redist
      (Format.pp_print_option (ProgramInfo.print_ctx_variable infos)) dest

let print_redistrib_list (type a) infos fmt (rs : a redistrib_with_dest list) =
  Format.pp_print_list (print_redistrib_with_dest infos) fmt rs

let rec print_guarded_obj :
  type a b. ProgramInfo.t -> (ProgramInfo.t -> Format.formatter -> b -> unit)
  -> Format.formatter -> (a, b) guarded_redistrib -> unit =
  fun infos print_atom fmt g_redist ->
  match g_redist with
  | Atom rs -> print_atom infos fmt rs
  | Branches { befores; afters } ->
    Format.pp_print_list (fun fmt (c, r)->
        Format.fprintf fmt "@[<v 2>avant %a (@,%a)@]"
          (print_event_expr infos) c (print_guarded_obj infos print_atom) r)
      fmt befores;
    Format.pp_print_break fmt 0 0;
    Format.pp_print_list (fun fmt (c, r)->
        Format.fprintf fmt "@[<v 2>apres %a (@,%a)@]"
          (print_event_expr infos) c (print_guarded_obj infos print_atom) r)
      fmt afters
  | Whens gs ->
    Format.pp_print_list (fun fmt (c, r)->
        Format.fprintf fmt "@[<v 2>quand %a (@,%a)@]"
          (print_event_expr infos) c (print_guarded_obj infos print_atom) r)
      fmt gs

let print_local_valuation (type a) infos fmt (f : a formula) =
  Format.fprintf fmt ": %a" (print_formula infos) f

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
    (match input.input_kind with
    | ReadOnly ->
      Format.fprintf fmt "entree %s type %a %a"
        input.input_name
        print_type input.input_type
        print_input_context input.input_context
    | Attributable ->
      Format.fprintf fmt "entree assiette %s %a"
        input.input_name
        print_input_context input.input_context)
  | DHolderOperation op ->
    Format.fprintf fmt "@[<hv 2>operation '%s' %a@,%a%a%a@]"
      op.op_label
      (Format.pp_print_option print_destination) op.op_default_dest
      print_op_context op.op_context
      print_op_source op.op_source
      (print_guarded_obj infos print_redistrib_list) op.op_guarded_redistrib
  | DVarOperation op ->
    Format.fprintf fmt "@[<hv 2>operation '%s' -> %a@,sur %a@;%a@]"
      op.ctx_op_label
      (Format.pp_print_option (ProgramInfo.print_ctx_variable infos)) op.ctx_op_default_dest
      (ProgramInfo.print_ctx_variable infos) op.ctx_op_source
      (print_guarded_obj infos print_redistrib_list) op.ctx_op_guarded_redistrib
  | DHolderEvent e ->
    Format.fprintf fmt "@[<hov>evenement %s@ atteint quand %a"
      e.event_name (print_event_expr infos) e.event_expr
  | DVarEvent e ->
    Format.fprintf fmt "@[<hov>evenement %a@ atteint quand %a"
      (ProgramInfo.print_variable infos) e.ctx_event_var
      (print_event_expr infos) e.ctx_event_expr
  | DHolderPool p ->
    Format.fprintf fmt "@[<hv 2>assiette calculee %s@,%a%a@]"
      p.comp_pool_name
      print_op_context p.comp_pool_context
      (print_guarded_obj infos print_local_valuation) p.comp_pool_guarded_value
  | DVarPool p ->
    Format.fprintf fmt "@[<hv 2>assiette calculee %a@,%a@]"
      (ProgramInfo.print_ctx_variable infos) p.ctx_comp_pool_var
      (print_guarded_obj infos print_local_valuation) p.ctx_comp_pool_guarded_value
  | DConstant c ->
    Format.fprintf fmt "constante %s : %a" c.const_name Literal.print c.const_value
  | DHolderValue v ->
    Format.fprintf fmt "@[<hov 2>valeur%s %s :@ %a@]"
      (if v.val_observable then " observable" else "")
      v.val_name
      (print_formula infos) v.val_formula
  | DVarValue v ->
    Format.fprintf fmt "@[<hov 2>valeur%s %a :@ %a@]"
      (if v.ctx_val_observable then " observable" else "")
      (ProgramInfo.print_ctx_variable infos) v.ctx_val_var
      (print_formula infos) v.ctx_val_formula
  | DHolderAdvance a ->
    Format.fprintf fmt "@[<hov>avance '%s' sur %a par %a@ montant %a@]"
      a.adv_label print_holder a.adv_output
      print_actor a.adv_provider
      (print_formula infos) a.adv_amount
  | DHolderDefault d ->
    Format.fprintf fmt "@[<hv>defaut sur %a@ vers %a@]"
      print_holder d.default_source print_holder d.default_dest
  | DVarDefault d ->
    Format.fprintf fmt "@[<hv>defaut sur %a@ vers %a@]"
      (ProgramInfo.print_ctx_variable infos) d.ctx_default_source
      (ProgramInfo.print_ctx_variable infos) d.ctx_default_dest
  | DHolderDeficit d ->
    Format.fprintf fmt "@[<hv>deficit sur %a@ par %a@]"
      print_holder d.deficit_pool print_holder d.deficit_provider
  | DVarDeficit d ->
    Format.fprintf fmt "@[<hv>deficit sur %a@ par %a@]"
      (ProgramInfo.print_ctx_variable infos) d.ctx_deficit_pool
      (ProgramInfo.print_ctx_variable infos) d.ctx_deficit_provider

let print_constants infos fmt () =
  Variable.Map.iter (fun v value ->
      Format.fprintf fmt "%a = %a@;"
        (ProgramInfo.print_variable infos) v
        Literal.print value
    )
    infos.constants

let print_program (type a) fmt (p : a program) =
  Format.pp_open_vbox fmt 0;
  begin match p with
    | Source decls ->
      Format.pp_print_list (print_declaration ProgramInfo.dummy) fmt decls
    | Contextualized (infos, decls) ->
      ProgramInfo.print_var_contexts infos fmt ();
      print_constants infos fmt ();
      Format.pp_print_list (print_declaration infos) fmt decls
  end;
  Format.fprintf fmt "@."
