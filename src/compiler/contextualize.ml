open Surface.Ast

module Acc : sig

  type t

  val empty : t

  val contexts : t -> Context.world

  val add_program_decl : t -> contextualized declaration -> t

  val register_input : t -> string -> ValueType.t -> input_kind -> t * Variable.t

  val register_pool : t -> string -> t * Variable.t

  val register_actor : t -> string -> t

  val register_actor_label :
    way:stream_way ->
    t ->
    string ->
    string ->
    t * Variable.t

  val register_event : t -> string -> t * Variable.t

  val register_const : t -> string -> ValueType.t -> Literal.t -> t

  val register_context_domain : t -> string -> string list -> t

  val find_pool_opt : t -> string -> Variable.t option

  val find_actor : way:stream_way -> t -> string -> Variable.t

  val find_event : t -> string -> Variable.t

  val find_misc_var : way:stream_way -> t -> string -> Variable.t

  val add_proj_constraint : t -> Variable.t -> Context.Group.t -> t

  val add_var_constraint : t -> Variable.t -> Variable.t -> Context.Group.t -> t

  val add_deps_from : t -> Variable.t -> Variable.t list -> t

  val to_contextualized_program : t -> contextualized program

end = struct

  type context_constraint = {
    from_vars : Context.Group.Set.t Variable.Map.t;
    (* Constraints related to downstream variables, projections restricted *)
    projections : Context.Group.Set.t;
    (* User projection constraints *)
  }

  (* Reference of names *)

  type actor_ref =
    | BaseActor of { downstream : Variable.t; upstream : Variable.t }
    | Label of Variable.t * stream_way

  type name_ref =
    | RefActor of actor_ref (* providers or receivers, with or without labels *)
    | RefPool of Variable.t (* redistributable pools of money *)
    | RefROInput of Variable.t (* Non redistributable inputs (variable parameters) *)
    | RefEvent of Variable.t
    | RefConst of Variable.t

  type t = {
    program_decls : contextualized declaration list; (* the program in reverse order *)
    var_info : VarInfo.collection;
    var_table : name_ref StrMap.t;
    contexts : Context.world;
    actors : stream_way Variable.Map.t;
    compounds : Variable.Set.t Variable.Map.t; (* Aggregations of actor labels *)
    constants : Literal.t Variable.Map.t;
    constraints : context_constraint Variable.Map.t;
    deps : Variable.Set.t Variable.Map.t; (* maps src -> dests, for cycle detections *)
  }

  let empty = {
    program_decls = [];
    var_info = Variable.Map.empty;
    var_table = StrMap.empty;
    contexts = Context.empty_world;
    actors = Variable.Map.empty;
    compounds = Variable.Map.empty;
    constants = Variable.Map.empty;
    constraints = Variable.Map.empty;
    deps = Variable.Map.empty;
  }

  let contexts t = t.contexts

  let is_actor v t =
    match Variable.Map.find_opt v t.var_info with
    | None -> false
    | Some i -> VarInfo.is_partner i

  let add_program_decl t (decl : contextualized declaration) =
    { t with program_decls = decl::t.program_decls }

  let bind_var (name : string) (vref : name_ref) t =
    let var_table =
      StrMap.update name (function
          | None -> Some vref
          | Some _ -> Errors.raise_error "Name %s already registered" name)
        t.var_table
    in
    { t with var_table }

  let bind_compound (v : Variable.t) (c : Variable.t) t =
    { t with
      compounds =
        Variable.Map.update c (function
            | None -> Some (Variable.Set.singleton v)
            | Some vs -> Some (Variable.Set.add v vs))
          t.compounds
    }

  let bind_const (v : Variable.t) (value : Literal.t) t =
    { t with constants = Variable.Map.add v value t.constants }

  let bind_vinfo (v : Variable.t) (info : VarInfo.t) t =
    { t with var_info = Variable.Map.add v info t.var_info }

  let register_pool t (name : string) =
    let v = Variable.create () in
    let info = VarInfo.{
        origin = Named name;
        typ = TMoney;
        kind = Intermediary;
      }
    in
    let t = bind_vinfo v info t in
    let t = bind_var name (RefPool v) t in
    t, v

  let register_input t (name : string) (typ : ValueType.t) (kind : input_kind) =
    match kind with
    | Attributable ->
      if typ <> ValueType.TMoney then Errors.raise_error "(internal) Wrong type for pool";
      let info = VarInfo.{
        origin = Named name;
        typ;
        kind = PoolInput { shadow = false };
      }
      in
      let t, v = register_pool t name in
      let t = bind_vinfo v info t in
      t, v
    | ReadOnly ->
      let v = Variable.create () in
      let info = VarInfo.{
        origin = Named name;
        typ;
        kind = ParameterInput { shadow = false };
      }
      in
      let t = bind_var name (RefROInput v) t in
      let t = bind_vinfo v info t in
      t, v

  let register_actor t (name : string) =
    let uv = Variable.create () in
    let dv = Variable.create () in
    let uinfo = VarInfo.{
        origin = Named name;
        typ = TMoney;
        kind = ProvidingPartner;
      }
    in
    let dinfo = VarInfo.{
        origin = Named name;
        typ = TMoney;
        kind = ReceivingPartner;
      }
    in
    let t = bind_vinfo uv uinfo t in
    let t = bind_vinfo dv dinfo t in
    bind_var name (RefActor (BaseActor {upstream = uv; downstream = dv})) t
    |> bind_compound dv dv
    |> bind_compound uv uv

  let register_event t (name : string) =
    let v = Variable.create () in
    let info = VarInfo.{
        origin = Named name;
        typ = ValueType.TEvent;
        kind = Event;
      }
    in
    let t = bind_vinfo v info t in
    let t = bind_var name (RefEvent v) t in
    t, v

  let register_const t (name : string) (typ : ValueType.t) (value : Literal.t) =
    let v = Variable.create () in
    let info = VarInfo.{
        origin = Named name;
        typ;
        kind = Constant;
      }
    in
    let t = bind_vinfo v info t in
    bind_var name (RefConst v) t
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
      let vl = Variable.create () in
      let info = VarInfo.{
          origin = LabelOfPartner { partner = base_actor; label };
          typ = ValueType.TMoney;
          kind =
            match way with
            | Upstream -> ProvidingPartner
            | Downstream -> ReceivingPartner;
        }
      in
      let t = bind_vinfo vl info t in
      let t =
        bind_var lname (RefActor (Label (vl, way))) t
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
    if is_actor v t then t else
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
    if is_actor v t then t else
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
    let rec resolve_var v shapes =
      (* Recursive memoized calls to process in topological order. Vulnerable to
         cyclicity *)
      match Variable.Map.find_opt v shapes with
      | Some shape -> shapes, shape
      | None ->
        match Variable.Map.find_opt v t.constraints with
        | None ->
          let everything = Context.shape_of_everything t.contexts in
          Variable.Map.add v everything shapes,
          everything
        | Some { from_vars; projections } ->
          (* Core of the inference algorithm

             Propagating shapes upward means having the minimum distinction of
             groups that can properly pour downstream (each group pours in one
             and only one group of the downstream variable).

             The first part is to aggregate all the groups of each constraining
             variables, restricted to the associated projections. Then, merge
             them together keeping all the distinctions made for each (group
             clipping).

             The second is to filter out any part of the result that does not
             exists in one of downstream variables but is included in the
             projections. This is to avoid a result with groups that can be
             poured in some cases. *)
          let shapes, (pshapes, forbidden) =
            Variable.Map.fold (fun v projs (shapes, (pshapes, forbidden)) ->
                let shapes, var_shape = resolve_var v shapes in
                let vperi = Context.shape_perimeter var_shape in
                shapes, Context.Group.Set.fold (fun proj (pshapes, forbidden) ->
                    Context.shape_cut_out var_shape proj :: pshapes,
                    Context.Group.union (Context.Group.inter forbidden vperi)
                      (Context.Group.diff forbidden proj)
                  )
                  projs (pshapes, forbidden))
              from_vars (shapes, ([], Context.any_projection t.contexts))
          in
          let shape_from_vars =
            match pshapes with
            | [] -> Context.shape_of_everything t.contexts
            | s::ss -> List.fold_left Context.shape_clip s ss
          in
          let filtered_shape = Context.shape_cut_out shape_from_vars forbidden in
          (* Additionally, for simple projection constraints, we just need to
             add distinction of what exists in the projection from what exists
             ouside *)
          let shape_with_projs =
            Context.Group.Set.fold (fun p s -> Context.shape_imprint_projection s p)
            projections filtered_shape
          in
          Variable.Map.add v shape_with_projs shapes,
          shape_with_projs
    in
    Variable.Map.fold (fun v _ shapes ->
        fst @@ resolve_var v shapes)
      t.var_info Variable.Map.empty

  let add_deps_from t (src : Variable.t) (dests : Variable.t list) =
    let tdeps =
      List.fold_left (fun tdeps dest ->
          match Variable.Map.find_opt dest t.deps with
          | None -> Variable.Set.add dest tdeps
          | Some tds -> Variable.Set.union (Variable.Set.add dest tds) tdeps)
        Variable.Set.empty dests
    in
    if Variable.Set.mem src tdeps then
      Errors.raise_error "Cyclic repartition";
    let deps =
      Variable.Map.update src (function
          | None -> Some tdeps
          | Some tds -> Some (Variable.Set.union tds tdeps))
        t.deps
    in
    { t with deps }

  let to_contextualized_program t =
    let infos : ProgramInfo.t = {
      var_info = t.var_info;
      var_shapes = resolve_constraints t;
      contexts = t.contexts;
      compounds = t.compounds;
      constants = t.constants;
      relevance_sets = Variable.Map.empty;
    }
    in
    Contextualized (infos, List.rev t.program_decls)
end

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

let projection_of_context_refinement contexts (ctx : context_refinement) =
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
    let proj = projection_of_context_refinement (Acc.contexts acc) ctx in
    begin match Acc.find_pool_opt acc name with
    | Some v ->
      Acc.add_proj_constraint acc v proj, (v, proj)
    | None ->
      let acc, v = Acc.register_pool acc name in
      Acc.add_proj_constraint acc v proj, (v, proj)
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
    (* TODO downward shape constraints *)
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
      let proj = projection_of_context_refinement (Acc.contexts acc) ctx in
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

let opposable acc ~(on_proj : Context.Group.t)
    (HolderOpp { opp_value; opp_provider; opp_towards } : source opposable) =
  let acc, opp_value = formula acc opp_value ~on_proj in
  let acc, opp_provider =
    find_holder_as_source acc
      (holder ~loc:opp_provider.actor_loc (Actor opp_provider))
  in
  let opp_towards =
    match opp_towards.actor_desc with
    | LabeledActor _ -> Errors.raise_error "Opposition target must be a partner without label"
    | PlainActor s -> Acc.find_actor acc ~way:Downstream s
  in
  acc, VarOpp { opp_value; opp_provider; opp_towards }

let redistribution acc (redist : source redistribution) ~(on_proj : Context.Group.t) =
  match redist.redistribution_desc with
  | Part (f, opposables) ->
    let acc, f = formula acc f ~on_proj in
    let acc, opposables = List.fold_left_map (opposable ~on_proj) acc opposables in
    acc, {redist with redistribution_desc = Part (f, opposables)}
  | Flat f ->
    let acc, f = formula acc f ~on_proj in
    acc, {redist with redistribution_desc = Flat f}
  | Retrocession (f, p) ->
    (* syntactic sugar *)
    let left_operand = f in
    let right_operand = Surface.Ast.formula (Named (Surface.Ast.named (Holder p))) in
    let f = Surface.Ast.formula (Binop (Mult, left_operand, right_operand)) in
    let acc, f = formula acc f ~on_proj in
    acc, {redist with redistribution_desc = Flat f}
  | Default -> acc, { redist with redistribution_desc = Default }

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
  let acc = Acc.add_deps_from acc src (List.map fst dests) in
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
  let t = Literal.type_of c.const_value in
  Acc.register_const acc c.const_name t c.const_value

let advance _acc (_a : advance_decl) = Errors.raise_error "no more advance"

let declaration acc (decl : source declaration) =
  match decl with
  | DInput i -> register_input acc i
  | DActor a -> Acc.register_actor acc a.actor_decl_desc
  | DContext c ->
    Acc.register_context_domain acc c.context_type_name c.context_type_cases
  | DHolderOperation o ->
    let acc, op = operation acc o in
    Acc.add_program_decl acc (DVarOperation op)
  | DHolderEvent e ->
    let acc, e = event_decl acc e in
    Acc.add_program_decl acc (DVarEvent e)
  | DConstant c -> constant acc c
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
    let acc = Acc.add_deps_from acc (fst ctx_default_source) [fst ctx_default_dest] in
    Acc.add_program_decl acc (DVarDefault { ctx_default_source; ctx_default_dest })
  | DHolderDeficit d ->
    let acc, ctx_deficit_pool = find_holder acc d.deficit_pool in
    let acc, ctx_deficit_provider = find_holder_as_source acc d.deficit_provider in
    let acc = Acc.add_deps_from acc (fst ctx_deficit_provider) [fst ctx_deficit_pool] in
    Acc.add_program_decl acc (DVarDeficit { ctx_deficit_pool; ctx_deficit_provider})
  | DHolderAdvance a -> advance acc a

let program (Source prog : source program) : contextualized program =
  let acc = Acc.empty in
  let acc = List.fold_left declaration acc prog in
  Acc.to_contextualized_program acc
