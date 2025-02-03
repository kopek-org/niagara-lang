open Surface
open Dataflow
open Equ

type flow_view = AtInstant | Cumulated

type artefact_event =
  | Generic
  | PreviousState of Variable.t
  | RaiseTest of Variable.t

type result = {
  infos : ProgramInfo.t;
  aggr_eqs : Equ.aggregate_eqs;
  event_eqs : Equ.expr Variable.Map.t;
}

module Acc = struct

type flat_op = {
  flat_label : string option;
  flat_expr : expr;
  flat_src : Variable.t;
  flat_cond : Condition.t;
}

type t = {
  pinfos : ProgramInfo.t;
  used_variables : Variable.Set.t;
  ctx_derivations : Variable.t Context.Group.Map.t Variable.Map.t;
  event_eqs : expr Variable.Map.t;
  repartitions : Repartition.eqs; (* src key *)
  flat_eqs : flat_op list Variable.Map.t; (* dest key *)
  value_eqs : aggregate_eqs; (* dest key *)
  cumulation_vars : Variable.t Variable.Map.t;
  deficits_vars : (Variable.t * Condition.t) list Variable.Map.t;
  oppositions : Opposition.user_substitutions Variable.Map.t (* target -> og variable -> subst expr *)
}

let make (pinfos : ProgramInfo.t) = {
  pinfos;
  used_variables = Variable.Set.empty;
  ctx_derivations = Variable.Map.empty;
  event_eqs = Variable.Map.empty;
  repartitions = Variable.Map.empty;
  flat_eqs = Variable.Map.empty;
  value_eqs = Variable.Map.empty;
  cumulation_vars = Variable.Map.empty;
  deficits_vars = Variable.Map.empty;
  oppositions = Variable.Map.empty;
}

let find_vinfo t (v : Variable.t) =
  match Variable.Map.find_opt v t.pinfos.var_info with
  | None -> Errors.raise_error "(internal) variable %d info not found" (Variable.uid v)
  | Some i -> i

let bind_vinfo t (v : Variable.t) (info : VarInfo.t) =
  { t with
    pinfos = {
      t.pinfos with
      var_info = Variable.Map.add v info t.pinfos.var_info
    }
  }

let create_var_from t ?(shadowing=false) (ov : Variable.t)
    (build : VarInfo.t -> VarInfo.t) =
  let set_shadowing i shadow =
    VarInfo.{ i with
      kind =
        match i.kind with
        | ParameterInput _ -> ParameterInput { shadow }
        | PoolInput _ -> PoolInput { shadow }
        | k -> k
    }
  in
  let v = Variable.create () in
  let oi = find_vinfo t ov in
  let oi =
    if shadowing then set_shadowing oi false else oi
  in
  let i = build oi in
  let t = bind_vinfo t v i in
  if shadowing then
    bind_vinfo t ov (set_shadowing oi true), v
  else
    t, v

let contexts t = t.pinfos.contexts

let type_of t v =
  match Variable.Map.find_opt v t.pinfos.var_info with
  | Some vinfo -> vinfo.typ
  | None -> Errors.raise_error "(internal) Cannot find type of variable"

let is_actor t (v : Variable.t) =
  match Variable.Map.find_opt v t.pinfos.var_info with
  | None -> false
  | Some i -> VarInfo.is_partner i

let find_const_opt t (v : Variable.t) =
  Variable.Map.find_opt v t.pinfos.constants

let create_cumulation t v =
  let t, cv =
    create_var_from t v (fun i ->
      { i with
        origin = Cumulative v;
        kind = Intermediary;
      })
  in
  let cumulation_vars =
    Variable.Map.add v cv t.cumulation_vars
  in
  { t with cumulation_vars }, cv

let get_cumulation_var t (v : Variable.t) =
  match Variable.Map.find_opt v t.cumulation_vars with
  | Some v -> t, v
  | None -> create_cumulation t v

let ensure_cumulation t (v : Variable.t) =
  fst @@ get_cumulation_var t v

let find_compound_vars t (v : Variable.t) =
  match Variable.Map.find_opt v t.pinfos.compounds with
  | None -> [v]
  | Some vs -> Variable.Set.elements vs

(** Explicit flagging of program objects to remove clutter generated in
    previous pass *)
let flag_variable_usage t (v : Variable.t) =
  { t with used_variables = Variable.Set.add v t.used_variables }

let var_shape t (v : Variable.t) =
  match Variable.Map.find_opt v t.pinfos.var_shapes with
  | Some shape -> shape
  | None -> Errors.raise_error "No shape for var %d" (Variable.uid v)

let register_event t (v : Variable.t) (e : expr) =
  { t with event_eqs = Variable.Map.add v e t.event_eqs }

let lift_event t (event : expr) (kind : artefact_event) =
  let has_equal =
    match event with
    | EVar v -> Some v
    | _ ->
      Variable.Map.fold (fun v evt -> function
          | Some v -> Some v
          | None -> if event = evt then Some v else None)
        t.event_eqs None
  in
  match has_equal with
  | Some evt -> t, evt
  | None ->
    let v = Variable.create () in
    let info = VarInfo.{
        origin =
          (match kind with
          | Generic -> AnonEvent
          | PreviousState v -> Peeking v
          | RaiseTest v -> RisingEvent v);
        typ = ValueType.TEvent;
        kind = Event;
      }
    in
    let t = bind_vinfo t v info in
    let t = flag_variable_usage t v in
    let t = register_event t v event in
    t, v

let register_part t ~(label : string option) ~(act : Condition.t) ~(src : Variable.t)
    ~(dest : Variable.t) (part : Repartition.part_or_def) =
  let share = Repartition.{ label; dest; part; condition = act } in
  let repartitions =
    Variable.Map.update src (function
        | None -> Some [ share ]
        | Some rs -> Some (share::rs))
      t.repartitions
  in
  { t with repartitions }

let register_redist t ~(act : Condition.t) ~(src : Variable.t)
    ~(dest : Variable.t) (part : R.t) (opps : (Variable.t * R.t) list) =
  let opps =
    List.map (fun (opp_target, opp_value) ->
      Repartition.{ opp_target; opp_value }) opps
  in
  register_part t ~act ~src ~dest (Part (part, opps))

let register_default t ~(act : Condition.t) ~(src : Variable.t)
    ~(dest : Variable.t) =
  register_part t ~act ~src ~dest Default

let register_deficit t ~(act : Condition.t) ~(provider : Variable.t)
    ~(pool : Variable.t) =
  register_part t ~act ~src:pool ~dest:provider Deficit

let register_flat ~(label : string option) t ~(act : Condition.t) ~(src : Variable.t)
    ~(dest : Variable.t) (expr : expr) =
  let flat = {
    flat_label = label;
    flat_src = src;
    flat_expr = expr;
    flat_cond = act;
  }
  in
  let flat_eqs =
    Variable.Map.update dest (function
        | None -> Some [ flat ]
        | Some rs -> Some (flat::rs))
      t.flat_eqs
  in
  { t with flat_eqs; }

let register_aggregation t ~(act : Condition.t) ~(dest : Variable.t) (v : Variable.t) =
  let value_eqs =
    Variable.Map.update dest (function
        | None -> Some (More [v, act])
        | Some (More vars) -> Some (More ((v, act)::vars))
        | Some (One _) ->
          Errors.raise_error "(internal) Cannot aggregate on valuation expression")
      t.value_eqs
  in
  { t with value_eqs }

let register_value t ~(act : Condition.t) ~(dest : Variable.t) (expr : expr) =
  let ge = { eq_act = act; eq_expr = expr } in
  let value_eqs =
    Variable.Map.update dest (function
        | None -> Some (One ge)
        | Some ag ->
          Variable.Map.iter (fun v _ ->
              Format.eprintf "%a@\n" (FormatEqu.print_var_with_info t.pinfos) v)
            t.pinfos.var_info;
          Format.eprintf "var %d: %a (%s)@."
                    (Variable.uid dest)
                    FormatEqu.print_expr expr
                    (match ag with One _ -> "one" | More _ -> "more");
          Errors.raise_error "(internal) Cannot register valuation on aggregation")
      t.value_eqs
  in
  { t with value_eqs; }

let register_opposition t ~(on : Variable.t) ~(target : Variable.t)
    (subst : Opposition.user_substitution) =
  let oppositions =
    Variable.Map.update target (function
        | None -> Some (Variable.Map.singleton on subst)
        | Some opps -> Some (Variable.Map.add on subst opps))
      t.oppositions
  in
  { t with oppositions }

let add_deficit_var t ~(act : Condition.t) ~(provider : Variable.t) (def : Variable.t) =
  { t with
    deficits_vars =
      Variable.Map.update provider (function
          | None -> Some [def,act]
          | Some vs -> Some ((def, act)::vs))
        t.deficits_vars
  }

let find_derivation_opt t (v : Variable.t) (ctx : Context.Group.t) =
  match Variable.Map.find_opt v t.ctx_derivations with
  | None -> None
  | Some cgm ->
    Context.Group.Map.find_opt ctx cgm

(* Fetch for the one variable to this exact context group *)
let get_derivative_var t (v : Variable.t) (ctx : Context.Group.t) =
  match find_derivation_opt t v ctx with
  | Some v -> t, v
  | None ->
    match Variable.Map.find_opt v t.pinfos.var_shapes with
    | None -> ensure_cumulation t v, v
    | Some s ->
      if Context.is_whole_shape s then ensure_cumulation t v, v else
        (* Fragile: Generating new variable for the given group without checking
           whever there is already one whose group overlap. This should not be an
           issue as the analysis should not produce such cases, but vulnerable
           nonetheless. *)
        let t, dv =
          create_var_from ~shadowing:true t v
            (fun i -> { i with origin = ContextSpecialized { origin = v; context = ctx } })
        in
        let t = flag_variable_usage t dv in
        let t = ensure_cumulation t dv in
        let t = {
          t with
          pinfos = {
            t.pinfos with
            var_shapes = Variable.Map.add dv
                (Context.shape_of_groups [ctx]) t.pinfos.var_shapes;
          };
          ctx_derivations =
            Variable.Map.update v (function
                | None -> Some (Context.Group.Map.singleton ctx dv)
                | Some groups -> Some (Context.Group.Map.add ctx dv groups)
              )
              t.ctx_derivations;
        }
        in
        t, dv

(** When fetching variables in a specific context, we need the groups to
      either be completely included in the given context, to simply overlap with
      it, depending on the intended use of the variables. *)
type derivation_mode = Strict | Inclusive

(* Actors do not have context distinctions, but have labels. This type makes
   clear what is found when requesting variables through abstract reference. *)
type compound_derivation =
  | ActorComp of { base : Variable.t; compound : Variable.t list }
  | ContextVar of Variable.t list

(* Fetch all variables included (according to the given mode) in the given
   context *)
let derive_ctx_variables ~mode t (v : Variable.t) (ctx : Context.Group.t) =
  if is_actor t v then
    let compound = find_compound_vars t v in
    let t =
      List.fold_left (fun t v ->
          flag_variable_usage (ensure_cumulation t v) v)
        t compound
    in
    t, ActorComp { base = v; compound; }
  else
    let shape = var_shape t v in
    let subshape =
      match mode with
      | Strict -> Context.shape_filter_projection shape ctx
      | Inclusive -> Context.shape_overlap_subshape shape ctx
    in
    let t, vars =
      Context.shape_fold (fun (t, vars) group ->
          let t, v = get_derivative_var t v group in
          t, v::vars)
        (t, []) subshape
    in
    t, ContextVar vars

let group_productions ~produce ~aggr_var t ops =
  let t, pvars = produce t ops in
  match pvars with
  | [v] -> t, v
  | _ ->
    let t, agv = aggr_var t in
    let t, cond =
      List.fold_left (fun (t, cond) (v, act) ->
          register_aggregation t ~act ~dest:agv v,
          Condition.disj cond act
        )
        (t, Condition.never) pvars
    in
    t, (agv, cond)

let convert_repartitions t =
  let conv_shares t src shares =
    let grouped_shares =
      List.fold_left (fun gs (share : _ Repartition.share) ->
          Variable.Map.update share.dest (function
              | None -> Some [share]
              | Some sh -> Some (share::sh))
            gs)
        Variable.Map.empty shares
    in
    let t, (pvar, _pcond) =
      group_productions
        ~produce:(fun t ops ->
            Variable.Map.fold (fun dest shares (t, vars) ->
                let t, (pvar, pcond) =
                  group_productions
                    ~produce:(List.fold_left_map (fun t ({ label; condition; part; dest = _ }
                                                         : Repartition.opposable_part Repartition.share) ->
                        let part, opposed = part in
                        let t, ov = create_var_from t dest (fun i ->
                            { i with
                              origin = OperationDetail
                                  { label; op_kind = Quotepart part; source = src; target = dest }
                            })
                        in
                        let t =
                          List.fold_left (fun t Repartition.{ opp_value; opp_target } ->
                              let delta = R.(opp_value - part) in
                              let expr = EMult (EConst (LRational opp_value), EVar src) in
                              let subst = Opposition.{ expr; delta; condition; source = src } in
                              register_opposition t ~on:ov ~target:opp_target subst)
                            t opposed
                        in
                        let expr = EMult (EConst (LRational part), EVar src) in
                        let t = register_value t ~act:condition ~dest:ov expr in
                        t, (ov, condition)))
                    ~aggr_var:(fun t -> create_var_from t dest (fun i ->
                        { i with
                          origin = OperationSum { source = src; target = dest }
                        }))
                    t shares
                in
                let t = register_aggregation t ~act:pcond ~dest pvar in
                t, (pvar, pcond)::vars)
              ops (t,[]))
        ~aggr_var:(fun t ->
            let agv = Variable.create () in
            let t =
              bind_vinfo t agv {
                origin = RepartitionSum src;
                typ = TMoney;
                kind = Intermediary;
              }
            in
            t, agv)
        t grouped_shares
    in
    t, pvar
  in
  let conv_defaults t src reps_var def_shares =
    List.fold_left
      (fun t ({ label; dest; condition; part }
              : Repartition.unified_parts Repartition.share) ->
        let t, ov = create_var_from t dest (fun i ->
            { i with
              origin = OperationDetail {
                  label;
                  op_kind = Default part;
                  source = src;
                  target = dest }
            })
        in
        let expr = EAdd (EVar src, ENeg (EVar reps_var)) in
        let t = register_value t ~act:condition ~dest:ov expr in
        register_aggregation t ~act:condition ~dest ov)
      t def_shares
  in
  let conv_deficit t src reps_var
      (def_share : Repartition.unified_parts Repartition.share option) =
    match def_share with
    | None -> t
    | Some { label; dest; condition; part } ->
      let t, ov = create_var_from t dest (fun i ->
          { i with
            origin = OperationDetail {
                label;
                op_kind = Deficit part;
                source = dest;
                target = src }
          })
      in
      let expr = EAdd (EVar reps_var, ENeg (EVar src)) in
      let t = register_value t ~act:condition ~dest:ov expr in
      add_deficit_var t ~act:condition ~provider:dest ov
  in
  Variable.Map.fold (fun src rep t ->
      let fullrep = Repartition.resolve_fullness rep in
      let t, direct_rep = conv_shares t src fullrep.parts in
      let t = conv_defaults t src direct_rep fullrep.defaults in
      conv_deficit t src direct_rep fullrep.deficits)
    t.repartitions t

let convert_flats t =
  Variable.Map.fold (fun dest flats t ->
      let grouped_flats =
        List.fold_left (fun gs flat ->
            Variable.Map.update flat.flat_src (function
                | None -> Some [flat]
                | Some fs -> Some (flat::fs))
              gs)
          Variable.Map.empty flats
      in
      Variable.Map.fold (fun src flats t ->
          let t, (pvar, pcond) =
            group_productions
              ~produce:(List.fold_left_map (fun t flat ->
                  let t, ldest =
                    create_var_from t dest (fun i ->
                        { i with
                          origin = OperationDetail {
                              label = flat.flat_label;
                              op_kind = Bonus;
                              source = src;
                              target = dest };
                          kind = Intermediary
                        })
                  in
                  let t = register_value t ~act:flat.flat_cond ~dest:ldest flat.flat_expr in
                  t, (ldest, flat.flat_cond)))
              ~aggr_var:(fun t -> create_var_from t dest (fun i ->
                  { i with
                    origin = OperationSum { source = src; target = dest }
                  }))
              t flats
          in
          let t = register_aggregation t ~act:pcond ~dest pvar in
          let t = register_aggregation t ~act:pcond ~dest:src pvar in
          t)
        grouped_flats t)
    t.flat_eqs t

let aggregate_derivations t =
  Variable.Map.fold (fun dest ctxv t ->
      let t = ensure_cumulation t dest in
      Context.Group.Map.fold (fun _ctx deriv t ->
          register_aggregation t ~act:Condition.always ~dest deriv)
        ctxv t)
    t.ctx_derivations t

let aggregate_compounds t =
  Variable.Map.fold (fun dest ctxv t ->
      let t = ensure_cumulation t dest in
      Variable.Set.fold (fun c t ->
          if Variable.equal dest c then t else
            register_aggregation t ~act:Condition.always ~dest c)
        ctxv t)
    t.pinfos.compounds t

let convert_cumulations t =
  Variable.Map.fold (fun instant cumul t ->
      let expr = EAdd (EPre cumul, EVar instant) in
      register_value t ~act:Condition.always ~dest:cumul expr)
    t.cumulation_vars t

let convert_deficits t =
  Variable.Map.fold (fun provider dvars t ->
      let t, (sumdef, act) =
        match dvars with
      | [v] -> t, v
      | _ ->
        group_productions
          ~produce:(fun t defs -> t, defs)
          ~aggr_var:(fun t ->
              create_var_from t provider (fun i ->
              { i with
                origin = DeficitSum provider
              }))
          t dvars
      in
      register_aggregation t ~act ~dest:provider sumdef)
    t.deficits_vars t

let resolve_oppositions (t : t) =
  let Opposition.{ opp_var_info; opp_value_eqs; opp_event_eqs; opp_relevance_sets } =
    Opposition.resolve t.pinfos.var_info t.value_eqs t.event_eqs
      t.oppositions t.cumulation_vars
  in
  { t with
    pinfos =
      { t.pinfos with
        var_info = opp_var_info;
        relevance_sets = opp_relevance_sets
      };
    value_eqs = opp_value_eqs;
    event_eqs = opp_event_eqs;
  }

let produce_aggregated_eqs t =
  let t = convert_repartitions t in
  let t = convert_flats t in
  let t = aggregate_derivations t in
  let t = aggregate_compounds t in
  let t = convert_cumulations t in
  let t = convert_deficits t in
  let t = resolve_oppositions t in
  { infos = t.pinfos;
    aggr_eqs = t.value_eqs;
    event_eqs = t.event_eqs;
  }

end

let shape_of_ctx_var acc (v : Ast.contextualized_variable) =
  let v, proj = v in
  let vshape = Acc.var_shape acc v in
  Context.shape_filter_projection vshape proj

let resolve_projection_context acc ~context ~refinement =
  (* Refinement has priority over operation context *)
  if Context.is_any_projection (Acc.contexts acc) refinement then context else refinement


(* Used only to compute constant quoteparts *)
let rec reduce_to_r (e : expr) : R.t option =
  let to_rational = function
    | Literal.LInteger f -> Some R.(~$$f)
    | LRational f -> Some f
    | _ -> None
  in
  let r_of_binop op e1 e2 =
    let e1 = reduce_to_r e1 in
    let e2 = reduce_to_r e2 in
    match e1, e2 with
    | Some e1, Some e2 -> Some (op e1 e2)
    | _ -> None
  in
  match e with
  | EConst l -> to_rational l
  | EVar _ | EPre _ | EMerge _ | ENot _ | EAnd _ | EGe _ -> None
  | EAdd (e1, e2) -> r_of_binop R.add e1 e2
  | EMult (e1, e2) -> r_of_binop R.mul e1 e2
  | ENeg e -> Option.map R.neg (reduce_to_r e)
  | EInv e -> Option.map R.inv (reduce_to_r e)

let var_view acc (view : flow_view) (v : Variable.t) =
  match view with
  | AtInstant -> acc, v
  | Cumulated -> Acc.get_cumulation_var acc v

(* When refering a named variable, it may actually refers to several variables
   according to the context. In this case, it refers to their sum. *)
let aggregate_vars acc ~view (vars : Variable.t list) =
  match vars with
  | [] -> Errors.raise_error "(internal) should have found derivative vars"
  | v::vs ->
    let acc, v = var_view acc view v in
    List.fold_left (fun (acc, e) v ->
        let acc, v = var_view acc view v in
        acc, EAdd (e, EVar v))
      (acc, EVar v) vs

let translate_binop (op : Ast.binop)
    (e1, t1 : expr * ValueType.t)
    (e2, t2 : expr * ValueType.t) =
  let typ =
    match op, t1, t2 with
    | Add, TInteger, TInteger
    | Sub, TInteger, TInteger
    | Mult, TInteger, TInteger
    | Div, TInteger, TInteger -> ValueType.TInteger
    | Add, TInteger, TRational
    | Add, TRational, TInteger
    | Add, TRational, TRational
    | Sub, TInteger, TRational
    | Sub, TRational, TInteger
    | Sub, TRational, TRational
    | Mult, TInteger, TRational
    | Mult, TRational, TInteger
    | Mult, TRational, TRational
    | Div, TInteger, TRational
    | Div, TRational, TInteger
    | Div, TRational, TRational -> ValueType.TRational
    | Add, TMoney, TMoney
    | Sub, TMoney, TMoney
    | Mult, TMoney, TInteger
    | Mult, TMoney, TRational
    | Mult, TInteger, TMoney
    | Mult, TRational, TMoney
    | Div, TMoney, TInteger
    | Div, TMoney, TRational -> ValueType.TMoney
    | Add, TDate, TDuration
    | Add, TDuration, TDate
    | Sub, TDate, TDuration -> ValueType.TDate
    | Add, TDuration, TDuration
    | Sub, TDuration, TDuration
    | Mult, TDuration, TInteger
    | Mult, TDuration, TRational
    | Mult, TInteger, TDuration
    | Mult, TRational, TDuration
    | Div, TDuration, TInteger
    | Div, TDuration, TRational -> ValueType.TDuration
    | _ -> Errors.raise_error "Mismatching types for binop"
  in
  let expr =
    match op with
    | Add -> EAdd (e1, e2)
    | Sub -> EAdd (e1, ENeg e2)
    | Mult -> EMult (e1, e2)
    | Div -> EMult (e1, EInv e2)
  in
  expr, typ

let rec translate_formula acc ~(ctx : Context.Group.t) ~(view : flow_view)
    (f : Ast.contextualized Ast.formula) =
  match f.formula_desc with
  | Literal l -> acc, (EConst l, Literal.type_of l)
  | Variable (v, proj) ->
    begin match Acc.find_const_opt acc v with
    | Some l ->
      acc, (EConst l, Literal.type_of l)
    | None ->
      let t = Acc.type_of acc v in
      let proj = resolve_projection_context acc ~context:ctx ~refinement:proj in
      let acc, vs = Acc.derive_ctx_variables ~mode:Strict acc v proj in
      let vs = match vs with
        | ActorComp c -> [c.base]
        | ContextVar vs -> vs
      in
      let acc, f = aggregate_vars acc ~view vs in
      acc, (f, t)
    end
  | Binop (op, f1, f2) ->
    let acc, e1 = translate_formula ~ctx acc ~view f1 in
    let acc, e2 = translate_formula ~ctx acc ~view f2 in
    acc, (translate_binop op e1 e2)
  | Total f -> translate_formula ~ctx acc ~view:Cumulated f
  | Instant f -> translate_formula ~ctx acc ~view:AtInstant f

let rec translate_event acc (eexpr : Ast.contextualized Ast.event_expr) =
  match eexpr.event_expr_desc with
  | EventVar v -> acc, EVar v
  | EventComp (Eq, f1, f2) ->
    let ctx = Context.any_projection (Acc.contexts acc) in
    let acc, (e1, _) = translate_formula ~ctx ~view:Cumulated acc f1 in
    let acc, (e2, _) = translate_formula ~ctx ~view:Cumulated acc f2 in
    acc, EGe (e1, e2)
  | EventConj (e1, e2) ->
    let acc, e1 = translate_event acc e1 in
    let acc, e2 = translate_event acc e2 in
    acc, EAnd(e1, e2)
  | EventDisj _ -> assert false

let translate_redistribution ~(label : string option) acc ~(ctx : Context.Group.t) ~(act : Condition.t)
    ~(src : Variable.t) ~(dest : Ast.contextualized_variable)
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
  | Part (f, opp) ->
    let acc, (partf, _) = translate_formula ~ctx ~view:AtInstant acc f in
    let part =
      match reduce_to_r partf with
      | Some p -> p
      | None -> Errors.raise_error "Non-constant quotepart"
    in
    let acc, opps =
      List.fold_left_map (fun acc (Ast.VarOpp { opp_towards; opp_value; _ }) ->
          let acc, (partf, _) = translate_formula ~ctx ~view:AtInstant acc opp_value in
          let opp_part =
            match reduce_to_r partf with
            | Some p -> p
            | None -> Errors.raise_error "Non-constant quotepart"
          in
          acc, (opp_towards, opp_part))
        acc opp
    in
    Acc.register_redist ~label acc ~act ~src ~dest part opps
  | Flat f ->
    let acc, (e, _) = translate_formula ~ctx ~view:AtInstant acc f in
    Acc.register_flat ~label acc ~act ~src ~dest e
  | Default -> Acc.register_default ~label acc ~act ~src ~dest

let translate_redist_w_dest acc ~(ctx : Context.Group.t) ~(act : Condition.t)
    ~(src : Variable.t)
    ~(def_dest : Ast.contextualized_variable option)
    (WithVar (redist, dest) : Ast.contextualized Ast.redistrib_with_dest)
  =
  let dest =
    match def_dest, dest with
    | Some _, Some dest (* TODO warning *)
    | None, Some dest -> dest
    | Some default, None -> default
    | None, None -> Errors.raise_error "No destination for repartition"
  in
  translate_redistribution ~ctx ~act ~src ~dest acc redist

let rec translate_guarded_redist ~(label : string option) acc ~(ctx : Context.Group.t)
    ~(act : Condition.t) ~(src : Variable.t) ~(def_dest : Ast.contextualized_variable option)
    (gr : _ Ast.guarded_redistrib) =
  match gr with
  | Redists rs ->
    List.fold_left (translate_redist_w_dest ~label ~ctx ~act ~src ~def_dest) acc rs
  | Whens gs ->
    List.fold_left (fun acc (cond, gr) ->
        let acc, condt, _ =
          translate_condition ~act ~on_raise:true acc cond
        in
        translate_guarded_redist ~label acc ~ctx ~act:condt ~src ~def_dest gr)
      acc gs
  | Branches { befores; afters } ->
    let acc, act =
      List.fold_right (fun (cond, gr) (acc, act) ->
          let acc, condt, condf =
            translate_condition ~act ~on_raise:false acc cond
          in
          translate_guarded_redist ~label acc ~ctx ~act:condt ~src ~def_dest gr,
          condf)
        afters (acc, act)
    in
    fst @@
    List.fold_left (fun (acc, act) (cond, gr) ->
        let acc, condt, condf =
          translate_condition ~act ~on_raise:false acc cond
        in
        translate_guarded_redist ~label acc ~ctx ~act:condf ~src ~def_dest gr,
        condt)
      (acc, act) befores

and translate_condition acc ~on_raise ~act
    (cond : Ast.contextualized Ast.event_expr) =
  let acc, eexpr = translate_event acc cond in
  let acc, evt =
    if on_raise then
      let acc, base_evt = Acc.lift_event acc eexpr Generic in
      let acc, prev_evt =
        Acc.lift_event acc (EPre base_evt) (PreviousState base_evt)
      in
      Acc.lift_event acc (EAnd (ENot (EVar prev_evt), EVar base_evt))
        (RaiseTest base_evt)
    else
      Acc.lift_event acc eexpr Generic
  in
  let condt = Condition.(conj (of_event evt true) act) in
  let condf = Condition.(conj (of_event evt false) act) in
  acc, condt, condf

let translate_operation acc (o : Ast.ctx_operation_decl) =
  let source_local_shape = shape_of_ctx_var acc o.ctx_op_source in
  Context.shape_fold (fun acc ctx ->
      let acc, src = Acc.get_derivative_var acc (fst o.ctx_op_source) ctx in
      translate_guarded_redist ~label:(Some o.ctx_op_label) acc ~ctx ~act:Condition.always
        ~src ~def_dest:o.ctx_op_default_dest o.ctx_op_guarded_redistrib)
    acc source_local_shape

let translate_default acc (d : Ast.ctx_default_decl) =
  let source_local_shape = shape_of_ctx_var acc d.ctx_default_source in
  Context.shape_fold (fun acc ctx ->
      let acc, src = Acc.get_derivative_var acc (fst d.ctx_default_source) ctx in
      let acc, dest =
        Acc.derive_ctx_variables ~mode:Inclusive acc (fst d.ctx_default_dest) ctx
      in
      let dest = match dest with
        | ActorComp c -> c.base
        | ContextVar [dest] -> dest
        | _ -> Errors.raise_error "destination derivation should have been unique"
      in
      Acc.register_default ~label:None acc ~act:Condition.always ~src ~dest)
    acc source_local_shape

let translate_deficit acc (d : Ast.ctx_deficit_decl) =
  let acc, provider =
    let prov, prov_ctx = d.ctx_deficit_provider in
    let acc, prov = Acc.derive_ctx_variables ~mode:Strict acc prov prov_ctx in
    acc, match prov with
    | ContextVar _ -> Errors.raise_error "Deficit handler can only be an actor"
    | ActorComp { base; _ } -> base
  in
  let pool_local_shape = shape_of_ctx_var acc d.ctx_deficit_pool in
  Context.shape_fold (fun acc ctx ->
      let acc, pool = Acc.get_derivative_var acc (fst d.ctx_deficit_pool) ctx in
      Acc.register_deficit ~label:None acc ~act:Condition.always ~provider ~pool)
    acc pool_local_shape

let translate_declaration acc (decl : Ast.contextualized Ast.declaration) =
  match decl with
  | DVarOperation o -> translate_operation acc o
  | DVarEvent e ->
    let acc, evt_expr = translate_event acc e.ctx_event_expr in
    Acc.register_event acc e.ctx_event_var evt_expr
  | DVarDefault d -> translate_default acc d
  | DVarDeficit d -> translate_deficit acc d

let translate_program (Contextualized (infos, prog) : Ast.contextualized Ast.program) =
  let acc = Acc.make infos in
  let acc = List.fold_left translate_declaration acc prog in
  Acc.produce_aggregated_eqs acc
