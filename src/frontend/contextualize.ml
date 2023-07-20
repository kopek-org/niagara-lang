open Ast

type constr =
  | ShapeOfVar of contextualized_variable * Context.projection
  | Shape of Context.shape * Context.projection
  | Projection of Context.projection

type constr_bound =
  | AtLeast of constr
  | AtMost of constr

module Acc = struct

  type name_ref =
    | RefActor of { downstream : Variable.t; upstream : Variable.t }
    | RefPool of Variable.t
    | RefROInput of Variable.t
    | RefEvent of Variable.t
    | RefConst of Variable.t

  type t = {
    var_info : Variable.info Variable.Map.t;
    var_shapes : Context.shape Variable.Map.t;
    var_table : name_ref StrMap.t;
    contexts : Context.world;
    inputs : input_kind Variable.Map.t;
    actors : stream_way Variable.Map.t;
    types : ValueType.t Variable.Map.t;
    constants : literal Variable.Map.t;
    constraints : (constr_bound list) Variable.Map.t;
  }

  let empty = {
    var_info = Variable.Map.empty;
    var_shapes = Variable.Map.empty;
    var_table = StrMap.empty;
    contexts = Context.empty_world;
    inputs = Variable.Map.empty;
    actors = Variable.Map.empty;
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
    bind_var name (RefActor {upstream = uv; downstream = dv}) t
    |> bind_name uv name
    |> bind_type uv ValueType.TMoney
    |> bind_name dv name
    |> bind_type dv ValueType.TMoney
    |> bind_actor dv Downstream
    |> bind_actor uv Upstream

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

  let is_actor t (v : Variable.t) =
    Variable.Map.mem v t.actors

  let find_pool_opt t (name : string) =
    match StrMap.find_opt name t.var_table with
    | Some (RefPool v) -> Some v
    | None -> None
    | Some _ -> Errors.raise_error "Identifier %s is not a pool" name

  let find_actor ~(way : stream_way) t (name : string) =
    match StrMap.find_opt name t.var_table with
    | Some (RefActor a) ->
      (match way with Upstream -> a.upstream | Downstream -> a.downstream)
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
    | Some (RefActor a) ->
      (match way with Upstream -> a.upstream | Downstream -> a.downstream)
    | Some (RefPool _) ->
      Errors.raise_error "Variable %s should be identified as pool" name
    | None -> Errors.raise_error "Unknown identifier %s" name

  let register_context_domain t (domain_name : string) (cases_names : string list) =
    { t with
      contexts = Context.add_domain t.contexts domain_name cases_names
    }

  let add_constraint0 t (v : Variable.t) (constr : constr_bound) =
    { t with
      constraints =
        Variable.Map.update v (function
            | None -> Some [constr]
            | Some ctrs -> Some (constr::ctrs)
          )
          t.constraints
    }

  let register_actor_label ~(way:stream_way) t (name : string) (label : string) =
    let contexts, dom, case = Context.extend_domain t.contexts name label in
    let v = find_actor ~way t name in
    let proj = Context.projection_of contexts dom [case] in
    let t = add_constraint0 t v (AtLeast (Projection proj)) in
    { t with contexts; }, v, proj

  let add_constraint t (v : Variable.t) (constr : constr_bound) =
    if Variable.Map.mem v t.actors then t else add_constraint0 t v constr

  let resolve_constraints t =
    let rec resolve_var v t =
      Format.printf "@[<v 2>resolve for %d@;" v;
      match Variable.Map.find_opt v t.var_shapes with
      | Some shape -> Format.printf "found !@]@;"; t, shape
      | None ->
        match Variable.Map.find_opt v t.constraints with
        | None ->
          let everything = Context.shape_of_everything t.contexts in
          { t with
            var_shapes = Variable.Map.add v everything t.var_shapes;
          },
          everything
        | Some constrs ->
          let most, least =
            List.fold_left (fun (m, l) ->
                function AtMost c -> c::m, l | AtLeast c -> m, c::l)
              ([], []) constrs
          in
          let t, least_shape =
            List.fold_left (fun (t, shape) constr ->
                match constr with
                | Projection p ->
                  Format.printf "constr proj: %a@;" (Context.print_projection t.contexts) p;
                  let pshape = Context.shape_of_projection t.contexts p in
                  Format.printf "proj shape: %a@;" (Context.print_shape t.contexts) pshape;
                  let shape = Context.shape_add_precise t.contexts shape pshape in
                  (* add and slice *)
                  t, shape
                | Shape (s, p) ->
                  Format.printf "constr shape: %a _ %a@;"
                    (Context.print_shape t.contexts) s
                    (Context.print_projection t.contexts) p;
                  let pshape = Context.shape_of_projection t.contexts p in
                  let sshape = Context.shape_filter_strict_precise t.contexts s pshape in
                  (* filterMustExist and slice *)
                  let shape = Context.shape_add_precise t.contexts shape sshape in
                  (* add and slice *)
                  t, shape
                | ShapeOfVar ((v, vp), p) ->
                  let t, vshape = resolve_var v t in
                  Format.printf "constr varShape: %a _ %a _ %a@;"
                    (Context.print_shape t.contexts) vshape
                    (Context.print_projection t.contexts) vp
                    (Context.print_projection t.contexts) p;
                  (* let vpshape = Context.shape_of_projection t.contexts vp in *)
                  (* Format.printf "shape of proj: %a@;" *)
                  (*   (Context.print_shape t.contexts) vpshape; *)
                  let vshape =
                    Context.projection_subshape_strict t.contexts vshape vp
                  in
                  (* filterMustExist and morePrecise *)
                  Format.printf "shape of projd var: %a@;"
                    (Context.print_shape t.contexts) vshape;
                  let pshape = Context.fit_projection_to_shape p vshape in
                  Format.printf "shape of proj: %a@;"
                    (Context.print_shape t.contexts) pshape;
                  let sshape =
                    Context.shape_filter_strict_precise t.contexts vshape pshape
                  in
                  (* filterMustExist and slice *)
                  let shape = Context.shape_add_precise t.contexts shape sshape in
                  (* add and slice *)
                  t, shape
              )
              (t, Context.empty_shape) least
          in
          List.iter (function
              (* | Projection p -> *)
              (*   let pshape = Context.shape_of_projection t.contexts p in *)
              (*   Context.match_shape t.contexts least_shape pshape *)
              (*   (\* filterMayNotExist and morePrecise *\) *)
              (* | Shape (s, p) -> *)
              (*   let pshape = Context.shape_of_projection t.contexts p in *)
              (*   let rshape = *)
              (*     Context.shape_filter_strict_loose t.contexts least_shape pshape *)
              (*   in *)
              (*   (\* filterMustExist and lessPrecise *\) *)
              (*   Context.match_shape t.contexts rshape s *)
              (*   (\* filterMayNotExist and morePrecise *\) *)
              (* | ShapeOfVar ((v, vp), p) -> *)
              (*   let t, vshape = resolve_var v t in *)
              (*   let vpshape = Context.shape_of_projection t.contexts vp in *)
              (*   let vshape = Context.refine_shape t.contexts vshape vpshape in *)
              (*   (\* filterMustExist and lessPrecise *\) *)
              (*   let pshape = Context.shape_of_projection t.contexts p in *)
              (*   let rshape = Context.refine_shape t.contexts least_shape pshape in *)
              (*   (\* filterMustExist and lessPrecise *\) *)
              (*   Context.match_shape t.contexts rshape vshape *)
              (*   (\* filterMayNotExist and morePrecise *\) *)
              | _ -> Errors.raise_error "Upstream context constraints not implemented"
            )
            most;
          Format.printf "done!@]@;@?";
          { t with
            var_shapes = Variable.Map.add v least_shape t.var_shapes;
          },
          least_shape
    in
    Variable.Map.fold (fun v _ t ->
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
  let is_redundant dom proj =
    if Context.projection_includes_domain proj dom then
      (* TODO warning *)
      true
    else false
  in
  let contexts = Acc.contexts acc in
  List.fold_left (fun proj ctx ->
      match ctx with
      | Forall dom ->
        let dom = Context.find_domain contexts dom in
        if is_redundant dom proj then proj else
          Context.projection_union proj (Context.projection_of contexts dom [])
      | Cases (dom, cases) ->
        let dom = Context.find_domain (Acc.contexts acc) dom in
        if is_redundant dom proj then proj else
          let cases = List.map (Context.find_case (Acc.contexts acc)) cases in
          Context.projection_union proj (Context.projection_of contexts dom cases)
    )
    Context.any_projection
    ctx

let projection_of_context_refinement acc (ctx : context_refinement) =
  let is_redundant dom case proj =
    match case with
      | None ->
        if Context.projection_includes_domain proj dom then
          (* TODO warning *)
          true
        else false
      | Some case ->
        if Context.projection_includes_case proj dom case then
          (* TODO warning *)
          true
        else false
  in
  let contexts = Acc.contexts acc in
  List.fold_left (fun proj item ->
      match item with
      | CFullDomain dom ->
        let dom = Context.find_domain contexts dom in
        if is_redundant dom None proj then proj else
          Context.projection_union proj (Context.projection_of contexts dom [])
      | CCase case ->
        let case = Context.find_case contexts case in
        let dom = Context.domain_of_case contexts case in
        if is_redundant dom (Some case) proj then proj else
          Context.projection_union proj (Context.projection_of contexts dom [case])
    )
    Context.any_projection
    ctx

let find_holder_opt_with_name ~(way:stream_way) acc (flow : holder) =
  (* TODO check var category *)
  match flow with
  | Pool (name, ctx) ->
    let proj = projection_of_context_refinement acc ctx in
    acc, Acc.find_pool_opt acc name, proj, name
  | Actor (PlainActor name) ->
    acc, Some (Acc.find_actor ~way acc name), Context.any_projection, name
  | Actor (LabeledActor (name, label)) ->
    let acc, v, proj = Acc.register_actor_label ~way acc name label in
    acc, Some v, proj, name

let find_holder0 ~(way : stream_way) acc (h : holder) =
  match find_holder_opt_with_name ~way acc h with
  | acc, Some v, proj, _name -> acc, (v, proj)
  | _, None, _, name ->
    Errors.raise_error "Unknown identifier" ~span:("Unknown holder "^name)

let find_holder acc (h : holder) = find_holder0 ~way:Downstream acc h

let find_holder_as_source acc (h : holder) = find_holder0 ~way:Upstream acc h

let register_input acc (i : input_decl) =
  let acc, v = Acc.register_input acc i.input_name i.input_type i.input_kind in
  if i.input_context = [] then acc else
    let shape =
      let enum_shapes =
        List.map (fun ctx ->
            Context.shape_of_projection (Acc.contexts acc)
              (projection_of_context_selector acc ctx))
          i.input_context
      in
      List.fold_left (Context.shape_add_disjoint acc.contexts)
        Context.empty_shape enum_shapes
    in
    Acc.add_constraint acc v (AtMost (Shape (shape, Context.any_projection)))

let destination acc (flow : holder) ~(on_proj : Context.projection) =
  let acc, v, proj =
    match find_holder_opt_with_name ~way:Downstream acc flow with
    | acc, Some v, proj, _name ->
      acc, v, proj
    | acc, None, proj, name ->
      let acc, v = Acc.register_pool acc name in
      acc, v, proj
  in
  (* TODO warning on context refine *)
  let acc = Acc.add_constraint acc v (AtLeast (Projection on_proj)) in
  acc, (v, proj)

let destination_opt acc (flow : holder option) ~(on_proj : Context.projection) =
  match flow with
  | Some dest ->
    let acc, dest = destination acc dest ~on_proj in
    acc, Some dest
  | None -> acc, None

let named acc (named : named) ~(on_proj : Context.projection) =
  let acc, (v, proj) =
    match named with
    | Name (name, ctx) ->
      let v = Acc.find_misc_var ~way:Downstream acc name in
      let proj = projection_of_context_refinement acc ctx in
      acc, (v, proj)
    | Holder h -> find_holder acc h
  in
  let proj = if Context.is_any_projection proj then on_proj else proj in
  let acc = Acc.add_constraint acc v (AtLeast (Projection proj)) in
  acc, (v, proj)

let rec formula acc (f : source formula) ~(on_proj : Context.projection) =
  match f with
  | Literal l -> acc, Literal l
  | Named n ->
    let acc, v = named acc n ~on_proj in
    acc, Variable v
  | Binop (op, f1, f2) ->
    let acc, f1 = formula acc f1 ~on_proj in
    let acc, f2 = formula acc f2 ~on_proj in
    acc, Binop (op, f1, f2)
  | Comp (op, f1, f2) ->
    let acc, f1 = formula acc f1 ~on_proj in
    let acc, f2 = formula acc f2 ~on_proj in
    acc, Comp (op, f1, f2)
  | Total f ->
    let acc, f = formula acc f ~on_proj in
    acc, Total f
  | Instant f ->
    let acc, f = formula acc f ~on_proj in
    acc, Instant f

let rec event_expr acc (e : source event_expr) ~(on_proj : Context.projection) =
  match e with
  | EventId name ->
    let v = Acc.find_event acc name in
    acc, EventVar v
  | EventFormula f ->
    let acc, f = formula acc f ~on_proj in
    acc, EventFormula f
  | EventConj (e1, e2) ->
    let acc, e1 = event_expr acc e1 ~on_proj in
    let acc, e2 = event_expr acc e2 ~on_proj in
    acc, EventConj (e1, e2)
  | EventDisj (e1, e2) ->
    let acc, e1 = event_expr acc e1 ~on_proj in
    let acc, e2 = event_expr acc e2 ~on_proj in
    acc, EventDisj (e1, e2)

let guard acc (guard : source guard) ~(on_proj : Context.projection) =
  match guard with
  | Before e ->
    let acc, e = event_expr acc e ~on_proj in
    acc, Before e
  | After e ->
    let acc, e = event_expr acc e ~on_proj in
    acc, After e
  | When e ->
    let acc, e = event_expr acc e ~on_proj in
    acc, When e

let redistribution acc (redist : source redistribution) ~(on_proj : Context.projection) =
  match redist with
  | Part f ->
    let acc, f = formula acc f ~on_proj in
    acc, Part f
  | Flat f ->
    let acc, f = formula acc f ~on_proj in
    acc, Flat f
  | Retrocession (f, p) ->
    (* syntactic sugar *)
    let f = Binop (Mult, f, Named (Holder p)) in
    let acc, f = formula acc f ~on_proj in
    acc, Flat f

let rec guarded_redist acc (redist : source guarded_redistrib) ~(on_proj : Context.projection) =
  match redist with
  | Redist (WithHolder (redist, dest)) ->
    let acc, dest = destination_opt acc dest ~on_proj in
    let acc, redist = redistribution acc redist ~on_proj in
    let g_redist = Redist (WithVar (redist, dest)) in
    acc, g_redist, Option.to_list dest
  | Guarded (guard_expr, redist) ->
    let acc, guard_expr = guard acc guard_expr ~on_proj in
    let acc, redist, dests = guarded_redist acc redist ~on_proj in
    let g_redist = Guarded (guard_expr, redist) in
    acc, g_redist, dests
  | Seq redists ->
    let (acc, dests), redists =
      List.fold_left_map (fun (acc, dests) redist ->
          let acc, redist, ds = guarded_redist acc redist ~on_proj in
          (acc, ds @ dests), redist
        )
        (acc, []) redists
    in
    acc, Seq redists, dests

let operation acc (op : operation_decl) =
  let acc, (src, src_proj) = find_holder_as_source acc op.op_source in
  let on_proj =
    projection_of_context_selector acc op.op_context
  in
  let on_proj =
    if Context.is_any_projection src_proj
    then on_proj
    else if Acc.is_actor acc src
    then src_proj
    else Errors.raise_error "Forbidden refinment on source of operation"
  in
  (* TODO warning on source context refinment *)
  let acc, default_dest = destination_opt acc op.op_default_dest ~on_proj in
  let acc, g_redist, dests = guarded_redist acc op.op_guarded_redistrib ~on_proj in
  let dests = dests @ (Option.to_list default_dest) in
  let acc =
    List.fold_left (fun acc dest ->
        Acc.add_constraint acc src (AtLeast (ShapeOfVar (dest, src_proj))))
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
  let acc, expr = event_expr acc e.event_expr ~on_proj:Context.any_projection in
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
  let acc, provider = find_holder acc (Actor a.adv_provider) in
  let acc, amount = formula acc a.adv_amount ~on_proj:Context.any_projection in
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
  | DActor a -> Acc.register_actor acc a, None
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
      destination acc d.default_dest ~on_proj:Context.any_projection
    in
    let acc =
      Acc.add_constraint acc src
        (AtLeast (ShapeOfVar (ctx_default_dest, src_proj)))
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
      types = acc.types;
      constants = acc.constants;
    }
  in
  Contextualized (program_infos, prog)
