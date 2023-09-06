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

open EzCompat
module StrMap = StringMap
open Ast



module Acc = struct

  type context_constraint = {
    from_vars : Context.Group.Set.t Variable.Map.t;
    projections : Context.Group.Set.t;
  }

  type actor_ref =
    | BaseActor of { downstream : Variable.t; upstream : Variable.t }
    | Label of Variable.t * stream_way

  type name_ref =
    | RefActor of actor_ref
    | RefPool of Variable.t
    | RefROInput of Variable.t
    | RefEvent of Variable.t
    | RefConst of Variable.t

  type t = {
    var_info : Variable.info Variable.Map.t;
    var_shapes : Context.shape Variable.Map.t;
    var_table : name_ref StringMap.t;
    contexts : Context.world;
    inputs : input_kind Variable.Map.t;
    actors : stream_way Variable.Map.t;
    compounds : Variable.Set.t Variable.Map.t;
    types : ValueType.t Variable.Map.t;
    constants : literal Variable.Map.t;
    constraints : context_constraint Variable.Map.t;
  }

  let empty = {
    var_info = Variable.Map.empty;
    var_shapes = Variable.Map.empty;
    var_table = StrMap.empty;
    contexts = Context.empty_world;
    inputs = Variable.Map.empty;
    actors = Variable.Map.empty;
    compounds = Variable.Map.empty;
    types = Variable.Map.empty;
    constants = Variable.Map.empty;
    constraints = Variable.Map.empty;
  }

  let contexts t = t.contexts

  let bind_var (name : string) (vref : name_ref) t =
    let var_table =
      StrMap.update name (function
          | None -> Some vref
          | Some _ -> Errors.raise_error "Name %s already registered" name)
        t.var_table
    in
    { t with var_table }

  let bind_name (v : Variable.t) (name : string) t =
    { t with
      var_info =
        Variable.Map.add v { Variable.var_name = name; } t.var_info
    }

  let bind_input (v : Variable.t) (kind : input_kind) t =
    { t with inputs = Variable.Map.add v kind t.inputs }

  let bind_type (v : Variable.t) (typ : ValueType.t) t =
    { t with types = Variable.Map.add v typ t.types }

  let bind_actor (v : Variable.t) (way : stream_way) t =
    { t with actors = Variable.Map.add v way t.actors }

  let bind_compound (v : Variable.t) (c : Variable.t) t =
    { t with
      compounds =
        Variable.Map.update c (function
            | None -> Some (Variable.Set.singleton v)
            | Some vs -> Some (Variable.Set.add v vs))
          t.compounds
    }

  let bind_const (v : Variable.t) (value : literal) t =
    { t with constants = Variable.Map.add v value t.constants }

  let register_pool t (name : string) =
    let v = Variable.new_var () in
    let t =
      bind_var name (RefPool v) t
      |> bind_name v name
      |> bind_type v ValueType.TMoney
      |> bind_input v Attributable
    in
    t, v

  let register_input t (name : string) (typ : ValueType.t) (kind : input_kind) =
    match kind with
    | Attributable ->
      if typ <> ValueType.TMoney then Errors.raise_error "Wrong type for pool";
      register_pool t name
    | ReadOnly ->
      let v = Variable.new_var () in
      let t =
        bind_var name (RefROInput v) t
        |> bind_name v name
        |> bind_type v typ
        |> bind_input v ReadOnly
      in
      t, v

  let register_actor t (name : string) =
    let uv = Variable.new_var () in
    let dv = Variable.new_var () in
    bind_var name (RefActor (BaseActor {upstream = uv; downstream = dv})) t
    |> bind_name uv name
    |> bind_type uv ValueType.TMoney
    |> bind_name dv name
    |> bind_type dv ValueType.TMoney
    |> bind_actor dv Downstream
    |> bind_actor uv Upstream
    |> bind_compound dv dv
    |> bind_compound uv uv

  let register_event t (name : string) =
    let v = Variable.new_var () in
    let t =
      bind_var name (RefEvent v) t
      |> bind_name v name
      |> bind_type v ValueType.TEvent
    in
    t, v

  let register_const t (name : string) (typ : ValueType.t) (value : literal) =
    let v = Variable.new_var () in
    bind_var name (RefConst v) t
    |> bind_name v name
    |> bind_type v typ
    |> bind_const v value

  let find_pool_opt t (name : string) =
    match StrMap.find_opt name t.var_table with
    | Some (RefPool v) -> Some v
    | None -> None
    | Some _ -> Errors.raise_error "Identifier %s is not a pool" name

  let find_actor ~(way : stream_way) t (name : string) =
    match StrMap.find_opt name t.var_table with
    | Some (RefActor ar) -> begin
        match ar with
        | Label (v, lway) ->
          if way <> lway then
            Errors.raise_error "Labeled actor has wrong steam way";
          v
        | BaseActor a ->
          (match way with Upstream -> a.upstream | Downstream -> a.downstream)
    end
    | Some _ -> Errors.raise_error "Identifier %s is not an actor" name
    | None -> Errors.raise_error "Unknown identifier %s" name

  let find_event t (name : string) =
    match StrMap.find_opt name t.var_table with
    | Some (RefEvent v) -> v
    | Some _ -> Errors.raise_error "Identifier %s is not an event" name
    | None -> Errors.raise_error "Unknown identifier %s" name

  let find_misc_var ~(way : stream_way) t (name : string) =
    match StrMap.find_opt name t.var_table with
    | Some (RefROInput v | RefEvent v | RefConst v) -> v
    | Some (RefActor (BaseActor a)) ->
      (match way with Upstream -> a.upstream | Downstream -> a.downstream)
    | Some (RefActor (Label _)) ->
      Errors.raise_error "(internal) actor name %s should not exists" name
    | Some (RefPool _) ->
      Errors.raise_error "Variable %s should be identified as pool" name
    | None -> Errors.raise_error "Unknown identifier %s" name

  let register_context_domain t (domain_name : string) (cases_names : string list) =
    { t with
      contexts = Context.add_domain t.contexts domain_name cases_names
    }

  let register_actor_label ~(way:stream_way) t (name : string) (label : string) =
    let base_actor =
      match StrMap.find_opt name t.var_table with
      | None -> Errors.raise_error "Unknown actor %s" name
      | Some (RefActor (BaseActor a)) ->
        (match way with Downstream -> a.downstream | Upstream -> a.upstream)
      | Some _ ->
        Errors.raise_error "(internal) %s should have been recognized as actor" name
    in
    let lname = name^"$"^label in
    match StrMap.find_opt lname t.var_table with
    | None ->
      let vl = Variable.new_var () in
      let t =
        bind_var lname (RefActor (Label (vl, way))) t
        |> bind_name vl lname
        |> bind_type vl ValueType.TMoney
        |> bind_actor vl way
        |> bind_compound vl base_actor
      in
      t, vl
    | Some (RefActor (Label (v, lway))) ->
      if way <> lway then
        Errors.raise_error "Cannot use label same label %s on both ways" label;
      t, v
    | Some _ ->
      Errors.raise_error "(internal) %s[%s] should have been recognized as a \
                          labeled actor"
        name label

  let add_var_constraint t (v : Variable.t)
      (from_var : Variable.t) (proj : Context.Group.t) =
    if Variable.Map.mem v t.actors then t else
      { t with
        constraints =
          Variable.Map.update v (function
              | None ->
                Some { from_vars =
                    Variable.Map.singleton
                      from_var
                      (Context.Group.Set.singleton proj);
                  projections = Context.Group.Set.empty;
                }
              | Some constrs ->
                Some { constrs with
                  from_vars =
                    Variable.Map.update from_var (function
                        | None -> Some (Context.Group.Set.singleton proj)
                        | Some projs -> Some (Context.Group.Set.add proj projs))
                      constrs.from_vars;
                })
            t.constraints
      }

  let add_proj_constraint t (v : Variable.t) (proj : Context.Group.t) =
    if Variable.Map.mem v t.actors then t else
      { t with
        constraints =
          Variable.Map.update v (function
              | None ->
                Some { from_vars = Variable.Map.empty;
                  projections = Context.Group.Set.singleton proj;
                }
              | Some constrs ->
                Some { constrs with
                  projections = Context.Group.Set.add proj constrs.projections;
                })
            t.constraints
      }

  let resolve_constraints t =
    let rec resolve_var v t =
      Format.printf "@[<v 2>resolve for %d@;" v;
      match Variable.Map.find_opt v t.var_shapes with
      | Some shape -> Format.printf "found !@]@;"; t, shape
      | None ->
        match Variable.Map.find_opt v t.constraints with
        | None ->
          Format.printf "no constraints @]@;@?";
          let everything = Context.shape_of_everything t.contexts in
          { t with
            var_shapes = Variable.Map.add v everything t.var_shapes;
          },
          everything
        | Some { from_vars; projections } ->
          let t, (pshapes, uclip) =
            Variable.Map.fold (fun v projs (t, (pshapes, uclip)) ->
                let t, var_shape = resolve_var v t in
                Format.printf "var_shape : %a@,(%a)@,%a@;"
                  (Context.print_shape t.contexts) var_shape
                  (Format.pp_print_list (Context.print_shape t.contexts)) pshapes
                  (Context.print_projection t.contexts) uclip;
                let vperi = Context.shape_perimeter var_shape in
                t, Context.Group.Set.fold (fun proj (pshapes, uclip) ->
                    Context.shape_cut_out var_shape proj :: pshapes,
                    Context.Group.inter uclip
                      (Context.Group.union vperi (Context.Group.not proj))
                  )
                  projs (pshapes, uclip))
              from_vars (t, ([], Context.any_projection t.contexts))
          in
          let shape_from_vars =
            match pshapes with
            | [] -> Context.shape_of_everything t.contexts
            | s::ss -> List.fold_left Context.shape_clip s ss
          in
          let clipped_shape = Context.shape_cut_out shape_from_vars uclip in
          let shape_with_projs =
            Context.Group.Set.fold (fun p s -> Context.shape_imprint_projection s p)
            projections clipped_shape
          in
          Format.printf "done!@]@;@?";
          { t with
            var_shapes = Variable.Map.add v shape_with_projs t.var_shapes;
          },
          shape_with_projs
    in
    Variable.Map.fold (fun v i t ->
        Format.printf "%s " i.Variable.var_name;
        fst @@ resolve_var v t)
      t.var_info t

end

let type_of_literal (lit : literal) =
  match lit with
  | LitInt _ -> ValueType.TInteger
  | LitRational _ -> ValueType.TRational
  | LitMoney _ -> ValueType.TMoney
  | LitDuration _ -> ValueType.TDuration
  | LitDate _ -> ValueType.TDate

let projection_of_context_selector acc (ctx : context list) =
  let contexts = Acc.contexts acc in
  let proj =
    List.fold_left (fun proj ctx ->
        match ctx with
        | Forall dom ->
          let dom = Context.find_domain contexts dom in
          Context.DomainMap.update dom (function
              | Some _ (* TODO warning *)
              | None -> Some Context.CaseSet.empty)
            proj
        | Cases (dom, cases) ->
          let domain = Context.find_domain contexts dom in
          let cases =
            List.fold_left (fun cases c ->
                let case = Context.find_case contexts c in
                if Context.case_is_in_domain contexts case domain
                then Context.CaseSet.add case cases
                else Errors.raise_error "Case %s do not belong in domain %s" c dom)
              Context.CaseSet.empty cases
          in
          Context.DomainMap.update domain (function
              | Some excases -> (* TODO warning *)
                Some (Context.CaseSet.union excases cases)
              | None -> Some cases)
            proj)
      Context.DomainMap.empty
      ctx
  in
  Context.group_of_selection contexts proj

let projection_of_context_refinement acc (ctx : context_refinement) =
  let contexts = Acc.contexts acc in
  let proj =
    List.fold_left (fun proj item ->
        match item.cri_desc with
        | CFullDomain dom ->
          let dom = Context.find_domain contexts dom in
          Context.DomainMap.update dom (function
              | Some _ (* TODO warning *)
              | None -> Some Context.CaseSet.empty)
            proj
        | CCase c ->
          let case = Context.find_case contexts c in
          let domain = Context.domain_of_case contexts case in
          Context.DomainMap.update domain (function
              | Some excases -> (* TODO warning *)
                Some (Context.CaseSet.add case excases)
              | None -> Some (Context.CaseSet.singleton case))
            proj)
      Context.DomainMap.empty
      ctx
  in
  Context.group_of_selection contexts proj

let find_holder0 ~(way : stream_way) acc (h : holder) =
  match h.holder_desc with
  | Pool (name, ctx) ->
    let proj = projection_of_context_refinement acc ctx in
    begin match Acc.find_pool_opt acc name with
    | Some v -> acc, (v, proj)
    | None ->
      let acc, v = Acc.register_pool acc name in
      acc, (v, proj)
    end
  | Actor {actor_desc = PlainActor name; _} ->
    acc, (Acc.find_actor ~way acc name, Context.any_projection (Acc.contexts acc))
  | Actor {actor_desc = LabeledActor (name, label); _} ->
    let acc, v = Acc.register_actor_label ~way acc name label in
    acc, (v, Context.any_projection (Acc.contexts acc))

let find_holder acc (h : holder) = find_holder0 ~way:Downstream acc h

let find_holder_as_source acc (h : holder) = find_holder0 ~way:Upstream acc h

let register_input acc (i : input_decl) =
  let acc, _v = Acc.register_input acc i.input_name i.input_type i.input_kind in
  if i.input_context = [] then acc else
    assert false

let destination acc (flow : holder) =
  let acc, (v, proj) = find_holder acc flow in
  if not @@ Context.is_any_projection (Acc.contexts acc) proj then
    Errors.raise_error "Forbidden context refinement on destination";
  acc, (v, proj)

let destination_opt acc (flow : holder option) =
  match flow with
  | Some dest ->
    let acc, dest = destination acc dest in
    acc, Some dest
  | None -> acc, None

let named acc (named : named) ~(on_proj : Context.Group.t) =
  let acc, (v, proj) =
    match named.named_desc with
    | Name (name, ctx) ->
      let v = Acc.find_misc_var ~way:Downstream acc name in
      let proj = projection_of_context_refinement acc ctx in
      acc, (v, proj)
    | Holder h -> find_holder acc h
  in
  let proj =
    if Context.is_any_projection (Acc.contexts acc) proj
    then on_proj else proj
  in
  let acc = Acc.add_proj_constraint acc v proj in
  acc, (v, proj)

let rec formula acc (f : source formula) ~(on_proj : Context.Group.t) =
  match f.formula_desc with
  | Literal l -> acc, {f with formula_desc = Literal l}
  | Named n ->
    let acc, v = named acc n ~on_proj in
    acc, {f with formula_desc = Variable v}
  | Binop (op, f1, f2) ->
    let acc, f1 = formula acc f1 ~on_proj in
    let acc, f2 = formula acc f2 ~on_proj in
    acc, {f with formula_desc = Binop (op, f1, f2)}
  | Total f ->
    let acc, f = formula acc f ~on_proj in
    acc, {f with formula_desc = Total f}
  | Instant f ->
    let acc, f = formula acc f ~on_proj in
    acc, {f with formula_desc = Instant f}

let rec event_expr acc (e : source event_expr) ~(on_proj : Context.Group.t) =
  match e.event_expr_desc with
  | EventId name ->
    let v = Acc.find_event acc name in
    acc, {e with event_expr_desc = EventVar v}
  | EventComp (op, f1, f2) ->
    let acc, f1 = formula acc f1 ~on_proj in
    let acc, f2 = formula acc f2 ~on_proj in
    acc, {e with event_expr_desc = EventComp (op, f1, f2)}
  | EventConj (e1, e2) ->
    let acc, e1 = event_expr acc e1 ~on_proj in
    let acc, e2 = event_expr acc e2 ~on_proj in
    acc, {e with event_expr_desc = EventConj (e1, e2)}
  | EventDisj (e1, e2) ->
    let acc, e1 = event_expr acc e1 ~on_proj in
    let acc, e2 = event_expr acc e2 ~on_proj in
    acc, {e with event_expr_desc = EventDisj (e1, e2)}

let redistribution acc (redist : source redistribution) ~(on_proj : Context.Group.t) =
  match redist.redistribution_desc with
  | Part f ->
    let acc, f = formula acc f ~on_proj in
    acc, {redist with redistribution_desc = Part f}
  | Flat f ->
    let acc, f = formula acc f ~on_proj in
    acc, {redist with redistribution_desc = Flat f}
  | Retrocession (f, p) ->
    (* syntactic sugar *)
    let left_operand = f in
    let right_operand = Ast.formula (Named (Ast.named (Holder p))) in
    let f = Ast.formula (Binop (Mult, left_operand, right_operand)) in
    let acc, f = formula acc f ~on_proj in
    acc, {redist with redistribution_desc = Flat f}

let redist_with_dest acc (WithHolder (redist, dest) : source redistrib_with_dest)
    ~(on_proj : Context.Group.t) =
  let acc, dest = destination_opt acc dest in
  let acc, redist = redistribution acc redist ~on_proj in
  let redist_wd = WithVar (redist, dest) in
  acc, redist_wd, Option.to_list dest

let rec guarded_redist acc (redist : source guarded_redistrib) ~(on_proj : Context.Group.t) =
  match redist with
  | Redists rs ->
    let (acc, dests), redists =
      List.fold_left_map (fun (acc, dests) r ->
          let acc, red_wd, ds = redist_with_dest acc r ~on_proj in
          (acc, ds @ dests), red_wd
        )
        (acc, []) rs
    in
    acc, Redists redists, dests
  | Branches { befores; afters } ->
    let (acc, dests), befores =
      List.fold_left_map (fun (acc, dests) (c, r) ->
          let acc, cond = event_expr acc c ~on_proj in
          let acc, g_redist, ds = guarded_redist acc r ~on_proj in
          (acc, ds@dests), (cond, g_redist)
        )
        (acc, []) befores
    in
    let (acc, dests), afters =
      List.fold_left_map (fun (acc, dests) (c, r) ->
          let acc, cond = event_expr acc c ~on_proj in
          let acc, g_redist, ds = guarded_redist acc r ~on_proj in
          (acc, ds@dests), (cond, g_redist)
        )
        (acc, dests) afters
    in
    acc, Branches { befores; afters }, dests
  | Whens gs ->
    let (acc, dests), redists =
      List.fold_left_map (fun (acc, dests) (c, r) ->
          let acc, cond = event_expr acc c ~on_proj in
          let acc, g_redist, ds = guarded_redist acc r ~on_proj in
          (acc, ds@dests), (cond, g_redist)
        )
        (acc, []) gs
    in
    acc, Whens redists, dests

let operation acc (op : operation_decl) =
  let acc, (src, src_proj) = find_holder_as_source acc op.op_source in
  if not @@ Context.is_any_projection (Acc.contexts acc) src_proj then
    Errors.raise_error "Forbidden refinment on source of operation";
  let on_proj =
    projection_of_context_selector acc op.op_context
  in
  let acc, default_dest = destination_opt acc op.op_default_dest in
  let acc, g_redist, dests = guarded_redist acc op.op_guarded_redistrib ~on_proj in
  let dests = dests @ (Option.to_list default_dest) in
  let acc =
    List.fold_left (fun acc (dest,_) ->
        Acc.add_var_constraint acc src dest on_proj)
      acc dests
  in
  acc,
  {
    ctx_op_label = op.op_label;
    ctx_op_default_dest = default_dest;
    ctx_op_source = src, on_proj;
    ctx_op_guarded_redistrib = g_redist;
  }

let event_decl acc (e : event_decl) =
  let acc, expr =
    event_expr acc e.event_expr
      ~on_proj:(Context.any_projection (Acc.contexts acc))
  in
  let acc, v = Acc.register_event acc e.event_name in
  acc,
  {
    ctx_event_var = v;
    ctx_event_expr = expr;
  }

let constant acc (c : const_decl) =
  let t = type_of_literal c.const_value in
  Acc.register_const acc c.const_name t c.const_value

let advance acc (a : advance_decl) =
  let acc, output = find_holder acc a.adv_output in
  let acc, provider =
    let holder = holder ~loc:a.adv_provider.actor_loc (Actor a.adv_provider) in
    find_holder acc holder in
  let acc, amount =
    formula acc a.adv_amount
      ~on_proj:(Context.any_projection (Acc.contexts acc))
  in
  acc,
  {
    ctx_adv_label = a.adv_label;
    ctx_adv_output = output;
    ctx_adv_provider = provider;
    ctx_adv_amount = amount;
  }

let declaration acc (decl : source declaration) =
  match decl with
  | DInput i -> register_input acc i, None
  | DActor a -> Acc.register_actor acc a.actor_decl_desc, None
  | DContext c ->
    Acc.register_context_domain acc c.context_type_name c.context_type_cases, None
  | DHolderOperation o ->
    let acc, op = operation acc o in
    acc, Some (DVarOperation op)
  | DHolderEvent e ->
    let acc, e = event_decl acc e in
    acc, Some (DVarEvent e)
  | DConstant c -> constant acc c, None
  | DHolderDefault d ->
    let acc, (src, src_proj as ctx_default_source) =
      find_holder acc d.default_source
    in
    let acc, ctx_default_dest =
      destination acc d.default_dest
    in
    let acc =
      Acc.add_var_constraint acc src (fst ctx_default_dest) src_proj
    in
    acc, Some (DVarDefault { ctx_default_source; ctx_default_dest })
  | DHolderDeficit d ->
    let acc, ctx_deficit_pool = find_holder acc d.deficit_pool in
    let acc, ctx_deficit_provider = find_holder acc d.deficit_provider in
    acc, Some (DVarDeficit { ctx_deficit_pool; ctx_deficit_provider})
  | DHolderAdvance a ->
    let acc, a = advance acc a in
    acc, Some (DVarAdvance a)

let program (Source prog : source program) : contextualized program =
  let acc = Acc.empty in
  let acc, prog =
    List.fold_left_map (fun acc decl ->
        declaration acc decl)
      acc prog
  in
  let prog = List.filter_map (fun x -> x) prog in
  let acc = Acc.resolve_constraints acc in
  let program_infos =
    { var_info = acc.var_info;
      var_shapes = acc.var_shapes;
      contexts = acc.contexts;
      inputs = acc.inputs;
      actors = acc.actors;
      compounds = acc.compounds;
      types = acc.types;
      constants = acc.constants;
    }
  in
  Contextualized (program_infos, prog)
