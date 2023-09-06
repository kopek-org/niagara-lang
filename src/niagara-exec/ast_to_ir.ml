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

open Niagara_ast
open Ir

module Acc = struct

  type t = {
    infos : Ast.program_infos;
    used_variables : Variable.Set.t;
    ctx_derivations : Variable.t Context.Group.Map.t Variable.Map.t;
    trees : RedistTree.t Variable.Map.t;
    events : event Variable.Map.t;
  }

  type derivation_mode = Strict | Inclusive

  let contexts t = t.infos.contexts

  let infos t = t.infos

  let trees t = t.trees

  let events t = t.events

  let make (infos : Ast.program_infos) =
    { infos;
      used_variables = Variable.Set.empty;
      ctx_derivations = Variable.Map.empty;
      trees = Variable.Map.empty;
      events = Variable.Map.empty;
    }

  let flag_variable_usage t (v : Variable.t) =
    { t with used_variables = Variable.Set.add v t.used_variables }

  let var_shape t (v : Variable.t) =
    match Variable.Map.find_opt v t.infos.var_shapes with
    | Some shape -> shape
    | None -> Errors.raise_error "No shape for var %d" v

  let type_of t v =
    match Variable.Map.find_opt v t.infos.types with
    | Some typ -> typ
    | None -> Errors.raise_error "(internal) Cannot find type of variable"

  let is_actor t (v : Variable.t) =
    Variable.Map.mem v t.infos.actors

  let anon_var_name =
    let c = ref 0 in
    fun name ->
      let i = !c in incr c;
      "anon_" ^ name ^ "_" ^ string_of_int i

  let find_const_opt t (v : Variable.t) =
    Variable.Map.find_opt v t.infos.Ast.constants

  let find_compound_vars t (v : Variable.t) =
    match Variable.Map.find_opt v t.infos.compounds with
    | None -> [v]
    | Some vs -> Variable.Set.elements vs

  let find_derivation_opt t (v : Variable.t) (ctx : Context.Group.t) =
    match Variable.Map.find_opt v t.ctx_derivations with
    | None -> None
    | Some cgm ->
      Context.Group.Map.find_opt ctx cgm

  let get_derivative_var t (v : Variable.t) (ctx : Context.Group.t) =
    match find_derivation_opt t v ctx with
    | Some v -> t, v
    | None ->
      let dv = Variable.new_var () in
      let { Variable.var_name } = Variable.Map.find v t.infos.var_info in
      let typ = type_of t v in
      let t = flag_variable_usage t dv in
      let t = {
        t with
        infos = {
          t.infos with
          var_info = Variable.Map.add dv { Variable.var_name } t.infos.var_info;
          types =  Variable.Map.add dv typ t.infos.types;
          var_shapes = Variable.Map.add dv
              (Context.shape_of_groups [ctx]) t.infos.var_shapes;
        };
        ctx_derivations =
          Variable.Map.update v (function
              | None -> Some (Context.Group.Map.singleton ctx dv)
              | Some groups -> Some (Context.Group.Map.add ctx dv groups)
            )
            t.ctx_derivations;
      }
      in
      let t =
        match Variable.Map.find_opt v t.infos.inputs with
        | None -> t
        | Some kind ->
          { t with infos = { t.infos with inputs = Variable.Map.add dv kind t.infos.inputs }}
      in
      let t =
        match Variable.Map.find_opt v t.infos.actors with
        | None -> t
        | Some way ->
          { t with infos = { t.infos with actors = Variable.Map.add dv way t.infos.actors }}
      in
      t, dv

  type compound_derivation =
    | ActorComp of { base : Variable.t; compound : Variable.t list }
    | ContextVar of Variable.t list

  let derive_ctx_variables ~mode t (v : Variable.t) (ctx : Context.Group.t) =
    if is_actor t v then
      let compound = find_compound_vars t v in
      let t = List.fold_left flag_variable_usage t compound in
      t, ActorComp { base = v; compound; }
    else
      let shape = var_shape t v in
      let subshape =
        match mode with
        | Strict -> Context.shape_project shape ctx
        | Inclusive -> Context.shape_overlap_subshape shape ctx
      in
      let t, vars =
        Context.shape_fold (fun (t, vars) group ->
            let t, v = get_derivative_var t v group in
            t, v::vars)
          (t, []) subshape
      in
      t, ContextVar vars

  let register_event t (v : Variable.t) (event : event) =
    let t = flag_variable_usage t v in
    { t with
      events = Variable.Map.add v event t.events
    }

  let lift_event t (event : event) =
    let has_equal =
      Variable.Map.fold (fun v evt -> function
          | Some v -> Some v
          | None -> if event = evt then Some v else None)
        t.events None
    in
    match has_equal with
    | Some evt -> t, evt
    | None ->
      let var_name = anon_var_name "event" in
      let v = Variable.new_var () in
      let t = flag_variable_usage t v in
      let t =
        { t with
          infos =
            { t.infos with
              var_info = Variable.Map.add v { Variable.var_name } t.infos.var_info;
              types = Variable.Map.add v ValueType.TEvent t.infos.types;
            };
        }
      in
      let t = register_event t v event in
      t, v

  let add_tree t ~(source : Variable.t) (tree : RedistTree.kind_tree) =
    let trees =
      Variable.Map.update source (function
          | None -> Some (RedistTree.of_tree tree)
          | Some existing ->
            Some (RedistTree.add_tree tree existing))
        t.trees
    in
    { t with trees }

  let add_default t ~(source : Variable.t) (d : Variable.t) =
    let trees =
      Variable.Map.update source (function
          | None -> Some (RedistTree.of_remainder d)
          | Some existing ->
            Some (RedistTree.add_remainder d existing))
        t.trees
    in
    { t with trees }

  let filter_usage t =
    let filter map =
      Variable.Map.filter (fun v _ -> Variable.Set.mem v t.used_variables) map
    in
    { t with
      infos = {
        t.infos with
        var_info = filter t.infos.var_info;
        var_shapes = filter t.infos.var_shapes;
        inputs = filter t.infos.inputs;
        actors = filter t.infos.actors;
        compounds = filter t.infos.compounds;
        types = filter t.infos.types;
        constants = filter t.infos.constants;
      }
    }

end

let shape_of_ctx_var acc (v : Ast.contextualized_variable) =
  let v, proj = v in
  let vshape = Acc.var_shape acc v in
  Context.shape_project vshape proj

let resolve_projection_context acc ~context ~refinement =
  if Context.is_any_projection (Acc.contexts acc) refinement then context else refinement

let rec reduce_formula (f : formula) =
  match f with
  | Literal _
  | Variable _ -> f
  | RCast f ->
    begin match reduce_formula f with
      | Literal (LInteger i) -> Literal (LRational (float_of_int i))
      | Literal _ -> assert false
      | _ -> f
    end
  | Binop (IAdd, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LInteger i1), Literal (LInteger i2) -> Literal (LInteger (i1 + i2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (RAdd, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LRational f1), Literal (LRational f2) -> Literal (LRational (f1 +. f2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (MAdd, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LMoney m1), Literal (LMoney m2) -> Literal (LMoney (m1 + m2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DAdd, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LDate d), Literal (LDuration dr) ->
        Literal (LDate (Date.Date.add d dr))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DrAdd, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LDuration dr1), Literal (LDuration dr2) ->
        Literal (LDuration (Date.Duration.add dr1 dr2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (ISub, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LInteger i1), Literal (LInteger i2) -> Literal (LInteger (i1 - i2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (RSub, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LRational f1), Literal (LRational f2) -> Literal (LRational (f1 -. f2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (MSub, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LMoney m1), Literal (LMoney m2) -> Literal (LMoney (m1 - m2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DSub, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LDate d), Literal (LDuration dr) ->
        Literal (LDate (Date.Date.rem d dr))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DrSub, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LDuration dr1), Literal (LDuration dr2) ->
        Literal (LDuration (Date.Duration.sub dr1 dr2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (IMult, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LInteger i1), Literal (LInteger i2) -> Literal (LInteger (i1 * i2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (RMult, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LRational f1), Literal (LRational f2) -> Literal (LRational (f1 *. f2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (MMult, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LMoney m), Literal (LRational f) ->
        Literal (LMoney (int_of_float ((float_of_int m) *. f)))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DrMult, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LDuration dr), Literal (LRational f) ->
        let ddr = Date.Duration.nb_days dr in
        Literal (LDuration (Date.Duration.day (int_of_float (float_of_int ddr *. f +. 0.5))))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (IDiv, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LInteger i1), Literal (LInteger i2) -> Literal (LInteger (i1 / i2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (RDiv, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LRational f1), Literal (LRational f2) -> Literal (LRational (f1 /. f2))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (MDiv, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LMoney m), Literal (LRational f) ->
        Literal (LMoney (int_of_float ((float_of_int m) /. f)))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DrDiv, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LDuration dr), Literal (LRational f) ->
        let ddr = Date.Duration.nb_days dr in
        Literal (LDuration (Date.Duration.day (int_of_float (float_of_int ddr /. f +. 0.5))))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end

let translate_literal (l : Ast.literal) =
  match l with
  | LitInt i -> LInteger i, ValueType.TInteger
  | LitRational r -> LRational r, ValueType.TRational
  | LitMoney c -> LMoney c, ValueType.TMoney
  | LitDate d -> LDate d, ValueType.TDate
  | LitDuration d -> LDuration d, ValueType.TDuration

let translate_binop (op : Ast.binop)
    (f1, t1 : formula * ValueType.t)
    (f2, t2 : formula * ValueType.t) =
  match op, t1, t2 with
  | Add, TInteger, TInteger -> Binop (IAdd, f1, f2), ValueType.TInteger
  | Add, TInteger, TRational -> Binop (RAdd, RCast f1, f2), ValueType.TRational
  | Add, TRational, TInteger -> Binop (RAdd, f1, RCast f2), ValueType.TRational
  | Add, TRational, TRational -> Binop (RAdd, f1, f2), ValueType.TRational
  | Add, TMoney, TMoney -> Binop (MAdd, f1, f2), ValueType.TMoney
  | Add, TDate, TDuration -> Binop (DAdd, f1, f2), ValueType.TDate
  | Add, TDuration, TDate -> Binop (DAdd, f2, f1), ValueType.TDate
  | Add, TDuration, TDuration -> Binop (DrAdd, f1, f2), ValueType.TDuration
  | Sub, TInteger, TInteger -> Binop (ISub, f1, f2), ValueType.TInteger
  | Sub, TInteger, TRational -> Binop (RSub, RCast f1, f2), ValueType.TRational
  | Sub, TRational, TInteger -> Binop (RSub, f1, RCast f2), ValueType.TRational
  | Sub, TRational, TRational -> Binop (RSub, f1, f2), ValueType.TRational
  | Sub, TMoney, TMoney -> Binop (MSub, f1, f2), ValueType.TMoney
  | Sub, TDate, TDuration -> Binop (DSub, f1, f2), ValueType.TDate
  | Sub, TDuration, TDuration -> Binop (DrSub, f1, f2), ValueType.TDuration
  | Mult, TInteger, TInteger -> Binop (IMult, f1, f2), ValueType.TInteger
  | Mult, TInteger, TRational -> Binop (RMult, RCast f1, f2), ValueType.TRational
  | Mult, TRational, TInteger -> Binop (RMult, f1, RCast f2), ValueType.TRational
  | Mult, TRational, TRational -> Binop(RMult, f1, f2), ValueType.TRational
  | Mult, TMoney, TInteger -> Binop (MMult, f1, RCast f2), ValueType.TMoney
  | Mult, TMoney, TRational -> Binop (MMult, f1, f2), ValueType.TMoney
  | Mult, TInteger, TMoney -> Binop (MMult, f2, RCast f1), ValueType.TMoney
  | Mult, TRational, TMoney -> Binop (MMult, f2, f1), ValueType.TMoney
  | Mult, TDuration, TInteger -> Binop (DrMult, f1, RCast f2), ValueType.TDuration
  | Mult, TDuration, TRational -> Binop (DrMult, f1, f2), ValueType.TDuration
  | Mult, TInteger, TDuration -> Binop (DrMult, f2, RCast f1), ValueType.TDuration
  | Mult, TRational, TDuration -> Binop (DrMult, f2, f1), ValueType.TDuration
  | Div, TInteger, TInteger -> Binop (IDiv, f1, f2), ValueType.TInteger
  | Div, TInteger, TRational -> Binop (RDiv, RCast f1, f2), ValueType.TRational
  | Div, TRational, TInteger -> Binop (RDiv, f1, RCast f2), ValueType.TRational
  | Div, TRational, TRational -> Binop (RDiv, f1, f2), ValueType.TRational
  | Div, TMoney, TInteger -> Binop (MDiv, f1, RCast f2), ValueType.TMoney
  | Div, TMoney, TRational -> Binop (MDiv, f1, f2), ValueType.TMoney
  | Div, TDuration, TInteger -> Binop (DrDiv, f1, RCast f2), ValueType.TDuration
  | Div, TDuration, TRational -> Binop (DrDiv, f1, f2), ValueType.TDuration
  | _ -> Errors.raise_error "Mismatching types for binop"

let aggregate_vars ~view (typ : ValueType.t) (vars : Variable.t list) =
  let op =
    match typ with
    | ValueType.TInteger -> IAdd
    | ValueType.TRational -> RAdd
    | ValueType.TMoney -> MAdd
    | ValueType.TEvent
    | ValueType.TDate
    | ValueType.TDuration ->
      Errors.raise_error
        "(internal) there should not exist multiple derivatives for \
         variable of type %a"
        FormatAst.print_type typ
  in
  match vars with
  | [] -> Errors.raise_error "(internal) should have found derivative vars"
  | v::vs ->
    List.fold_left (fun f v ->
        (Binop (op, f, Variable (v, view))))
      (Variable (v, view)) vs

let rec translate_formula ~(ctx : Context.Group.t) acc ~(view : flow_view)
    (f : Ast.contextualized Ast.formula) =
  match f.formula_desc with
  | Literal l ->
    let l, t = translate_literal l in
    acc, (Literal l, t)
  | Variable (v, proj) ->
    begin match Acc.find_const_opt acc v with
    | Some l ->
      let l, t = translate_literal l in
      acc, (Literal l, t)
    | None ->
      let t = Acc.type_of acc v in
      let proj = resolve_projection_context acc ~context:ctx ~refinement:proj in
      let acc, vs = Acc.derive_ctx_variables ~mode:Strict acc v proj in
      let vs = match vs with
        | ActorComp c -> c.compound
        | ContextVar vs -> vs
      in
      let f = aggregate_vars ~view t vs in
      acc, (f, t)
    end
  | Binop (op, f1, f2) ->
    let acc, f1 = translate_formula ~ctx acc ~view f1 in
    let acc, f2 = translate_formula ~ctx acc ~view f2 in
    acc, (translate_binop op f1 f2)
  | Instant f -> translate_formula ~ctx acc ~view:AtInstant f
  | Total f -> translate_formula ~ctx acc ~view:Cumulated f

let translate_redist ~(ctx : Context.Group.t) acc ~(dest : Ast.contextualized_variable)
    (redist : Ast.contextualized Ast.redistribution) =
  let proj = resolve_projection_context acc ~context:ctx ~refinement:(snd dest) in
  let acc, dest = Acc.derive_ctx_variables ~mode:Inclusive acc (fst dest) proj in
  let dest =
    match dest with
    | ActorComp c -> c.base
    | ContextVar [v] -> v
    | _ -> Errors.raise_error "(internal) Destination context inapplicable"
  in
  match redist.redistribution_desc with
  | Part f ->
    let acc, (f, ft) = translate_formula ~ctx acc ~view:AtInstant f in
    acc, RedistTree.share dest (reduce_formula f, ft)
  | Flat f ->
    let acc, f = translate_formula ~ctx acc ~view:AtInstant f in
    acc, RedistTree.flat dest f

let translate_comp (comp : Ast.comp) =
  match comp with Eq -> Eq

let rec translate_event acc (event : Ast.contextualized Ast.event_expr) =
  match event.event_expr_desc with
  | EventVar v -> acc, EvtVar v
  | EventConj (e1, e2) ->
    let acc, e1 = translate_event acc e1 in
    let acc, e2 = translate_event acc e2 in
    acc, EvtAnd(e1,e2)
  | EventDisj (e1, e2) ->
    let acc, e1 = translate_event acc e1 in
    let acc, e2 = translate_event acc e2 in
    acc, EvtOr(e1,e2)
  | EventComp (comp, f1, f2) ->
    let ctx = Context.any_projection (Acc.contexts acc) in
    let acc, (f1, t1) = translate_formula ~ctx acc ~view:Cumulated f1 in
    let acc, (f2, t2) = translate_formula ~ctx acc ~view:Cumulated f2 in
    let c = translate_comp comp in
    if t1 <> t2 then
      Errors.raise_error "Mismatched type for comparison";
    acc, EvtComp (c, f1, f2)

let lift_event ~(on_raise : bool) acc (event : Ast.contextualized Ast.event_expr) =
  let acc, evt = translate_event acc event in
  let acc, evt =
    match evt with
    | EvtVar v -> acc, v
    | _ -> Acc.lift_event acc evt
  in
  if on_raise
  then Acc.lift_event acc (EvtOnRaise evt)
  else acc, evt

let translate_redist_with_dest ~(ctx : Context.Group.t) acc
    ~(default_dest : Ast.contextualized_variable option)
    (WithVar (redist, dest) : Ast.contextualized Ast.redistrib_with_dest) =
  let dest =
    match default_dest, dest with
    | Some _, Some dest (* TODO warning *)
    | None, Some dest -> dest
    | Some default, None -> default
    | None, None -> Errors.raise_error "No destination for repartition"
  in
  let acc, redist = translate_redist ~ctx acc ~dest redist in
  acc, redist

let rec translate_guarded_redist ~(ctx : Context.Group.t) acc
    ~(default_dest : Ast.contextualized_variable option)
    (redist : Ast.contextualized Ast.guarded_redistrib) =
  match redist with
  | Redists rs ->
    let acc, redists =
      List.fold_left_map (fun acc r ->
          translate_redist_with_dest ~ctx acc ~default_dest r)
        acc rs
    in
    begin match redists with
      | [] -> assert false
      | r::rs -> acc, RedistTree.tredist (List.fold_left RedistTree.merge_redist r rs)
    end
  | Whens gs ->
    let acc, gs = translate_condition_group ~on_raise:true ~ctx acc ~default_dest gs in
    acc, RedistTree.twhen gs
  | Branches { befores; afters } ->
    let acc, befores =
      translate_condition_group ~on_raise:false ~ctx acc ~default_dest befores
    in
    let acc, afters =
      translate_condition_group ~on_raise:false ~ctx acc ~default_dest afters
    in
    let btree =
      List.fold_right (fun (evt, before) after ->
          RedistTree.tbranch evt before after)
        befores RedistTree.NothingTree
    in
    let tree =
      List.fold_left (fun before (evt, after) ->
          RedistTree.tbranch evt before after)
        btree afters
    in
    acc, tree

and translate_condition_group ~on_raise ~ctx acc ~default_dest
    (group : Ast.contextualized Ast.conditional_redistrib list)
  =
  List.fold_left_map (fun acc (cond, g_redist) ->
      let acc, evt = lift_event ~on_raise acc cond in
      let acc, tree = translate_guarded_redist ~ctx acc ~default_dest g_redist in
      acc, (evt, tree)
    )
    acc group

let translate_operation acc (o : Ast.ctx_operation_decl) =
  Format.printf "translate op '%s'@." o.ctx_op_label;
  let source_local_shape = shape_of_ctx_var acc o.ctx_op_source in
  Context.shape_fold (fun acc ctx ->
      let acc, source = Acc.get_derivative_var acc (fst o.ctx_op_source) ctx in
      let acc, tree =
        translate_guarded_redist ~ctx acc ~default_dest:o.ctx_op_default_dest
          o.ctx_op_guarded_redistrib
      in
      Acc.add_tree acc ~source tree)
    acc source_local_shape

let translate_default acc (d : Ast.ctx_default_decl) =
  let source_local_shape = shape_of_ctx_var acc d.ctx_default_source in
  Context.shape_fold (fun acc ctx ->
      let acc, source = Acc.get_derivative_var acc (fst d.ctx_default_source) ctx in
      let acc, dest =
        Acc.derive_ctx_variables ~mode:Inclusive acc (fst d.ctx_default_dest) ctx
      in
      match dest with
      | ActorComp c -> Acc.add_default acc ~source c.base
      | ContextVar [dest] -> Acc.add_default acc ~source dest
      | _ -> Errors.raise_error "destination derivation should have been unique")
    acc source_local_shape

let translate_declaration acc (decl : Ast.contextualized Ast.declaration) =
  match decl with
  | DVarOperation o -> translate_operation acc o
  | DVarEvent e ->
    let acc, evt_formula = translate_event acc e.ctx_event_expr in
    Acc.register_event acc e.ctx_event_var evt_formula
  | DVarDefault d -> translate_default acc d
  | DVarAdvance _
  | DVarDeficit _ -> assert false

let level_fractional_attribution (t : RedistTree.t) =
  match t with
  | Flat _ -> t
  | Fractions { base_shares; default; branches } ->
    let redist_part (r : RedistTree.frac RedistTree.redist) =
      match r with
      | NoInfo -> 0.
      | Shares sh -> Variable.Map.fold (fun _v -> (+.)) sh 0.
    in
    let rec branch_part known default (tree : RedistTree.frac RedistTree.tree) =
      match tree with
      | Nothing -> default
      | Redist r ->
        Variable.BDT.map_action known (fun _k -> function
            | None -> assert false
            | Some part -> Action (part -. redist_part r))
          default
      | Branch { evt; before; after } ->
        let default = Variable.BDT.add_decision evt default in
        let default = branch_part (Variable.Map.add evt true known) default before in
        branch_part (Variable.Map.add evt false known) default after
    in
    let base_part = redist_part base_shares in
    let default_redist = Variable.BDT.Action (1. -. base_part) in
    let branches_parts =
      List.fold_left (branch_part Variable.Map.empty) default_redist branches
    in
    let make_default_redist =
      match default with
      | NoDefault -> fun _path _ -> RedistTree.Nothing (* TODO warning default needed *)
      | DefaultTree _ -> Errors.raise_error "(internal) Default tree already computed"
      | DefaultVariable d -> fun _ part ->
        RedistTree.(Redist (Shares (Variable.Map.singleton d part)))
    in
    let default_tree =
      Variable.BDT.fold branches_parts
        ~noaction:(fun _k -> RedistTree.Nothing)
        ~action:(fun k part ->
            if part > 0.
            then make_default_redist k part
            else RedistTree.Nothing)
        ~decision:(fun _k evt before after ->
            match before, after with
            | Nothing, Nothing -> RedistTree.Nothing
            | _, _ -> Branch { evt; before; after })
    in
    Fractions { base_shares; branches; default = DefaultTree default_tree }

let level_attributions (acc : Acc.t) =
  let trees = Variable.Map.map level_fractional_attribution acc.trees in
  { acc with trees }

let dependancy_graph (acc : Acc.t) =
  let rec dep_formula graph src (f : formula) =
    match f with
    | Literal _ -> graph
    | Variable (v, _) -> Variable.Graph.add_edge graph v src
    | Binop (_, f1, f2) ->
      let graph = dep_formula graph src f1 in
      dep_formula graph src f2
    | RCast f -> dep_formula graph src f
  in
  let dep_redist (type a) graph src (r : a RedistTree.redist) =
    match r with
    | RedistTree.NoInfo -> graph
    | RedistTree.Shares sh ->
      Variable.Map.fold (fun dest _ graph -> Variable.Graph.add_edge graph src dest)
        sh graph
    | RedistTree.Flats fs ->
      Variable.Map.fold (fun dest formula graph ->
          let graph = Variable.Graph.add_edge graph src dest in
          dep_formula graph src formula)
        fs graph
  in
  let rec dep_tree :
    type a. Variable.Graph.t -> Variable.t -> a RedistTree.tree -> Variable.Graph.t =
    fun graph src tree ->
    match tree with
    | RedistTree.Nothing -> graph
    | RedistTree.Redist r -> dep_redist graph src r
    | RedistTree.When ws ->
      List.fold_left (fun graph (_,tree) -> dep_tree graph src tree) graph ws
    | RedistTree.Branch { before; after; _ } ->
      let graph = dep_tree graph src before in
      dep_tree graph src after
  in
  Variable.Map.fold (fun src trees graph ->
      match trees with
      | RedistTree.Fractions { base_shares; default; branches } ->
        let graph = dep_redist graph src base_shares in
        let graph =
          match default with
          | NoDefault -> graph
          | DefaultVariable d -> Variable.Graph.add_edge graph src d
          | DefaultTree tree -> dep_tree graph src tree
        in
        List.fold_left (fun graph tree -> dep_tree graph src tree)
          graph branches
      | RedistTree.Flat fs ->
        List.fold_left (fun graph tree -> dep_tree graph src tree) graph fs)
    acc.trees Variable.Graph.empty

let evaluation_order (acc : Acc.t) =
  let graph = dependancy_graph acc in
  let module SCC = Graph.Components.Make(Variable.Graph) in
  let scc = SCC.scc_list graph in
  List.fold_left (fun order comp ->
      match comp with
      | [] -> assert false
      | [v] -> if Variable.Map.mem v acc.trees then v::order else order
      | _ -> Errors.raise_error "Cyclic dependancy")
    [] scc

let translate_program (Contextualized (infos, prog) : Ast.contextualized Ast.program) =
  let acc = Acc.make infos in
  let acc =
    List.fold_left
      (fun acc decl ->
         translate_declaration acc decl)
      acc prog
  in
  let acc = Acc.filter_usage acc in
  let acc = level_attributions acc in
  let eval_order = evaluation_order acc in
  {
    infos = Acc.infos acc;
    trees = Acc.trees acc;
    events = Acc.events acc;
    eval_order;
  }
