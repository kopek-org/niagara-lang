open Surface.Ast

module Acc : sig

  type t

  val empty : t

  val contexts : t -> Context.world

  val is_partner : Variable.t -> t -> bool

  val is_linear : Variable.t -> t -> bool

  val type_of : Variable.t -> t -> ValueType.t

  val add_program_decl : t -> contextualized declaration -> t

  val register_input : t -> string -> ValueType.t -> input_kind -> t * Variable.t

  val register_pool : t -> string -> computed:bool -> t * Variable.t

  val register_value :
    t ->
    string ->
    obs:bool ->
    linear:bool ->
    typ:ValueType.t ->
    t * Variable.t

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

  val project_var : t -> Variable.t -> Context.Group.t -> contextualized_variable

  val find_pool_opt : t -> string -> Variable.t option

  val find_actor : way:stream_way -> t -> string -> Variable.t

  val find_event : t -> string -> Variable.t

  val find_misc_var : t -> string -> Variable.t

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
    | BaseActor of Variable.t
    | Label of Variable.t * stream_way

  type name_ref =
    | RefActor of actor_ref (* providers or receivers, with or without labels *)
    | RefPool of Variable.t (* redistributable pools of money *)
    | RefValue of Variable.t
    | RefROInput of Variable.t (* Non redistributable inputs (variable parameters) *)
    | RefEvent of Variable.t
    | RefConst of Variable.t

  type t = {
    program_decls : contextualized declaration list; (* the program in reverse order *)
    var_info : VarInfo.collection;
    var_table : name_ref StrMap.t;
    contexts : Context.world;
    compounds : Variable.Set.t Variable.Map.t; (* Aggregations of actor labels *)
    constants : Literal.t Variable.Map.t;
    constraints : context_constraint Variable.Map.t;
    linearity : bool Variable.Map.t;
    deps : Variable.Set.t Variable.Map.t; (* maps src -> dests, for cycle detections *)
  }

  let empty = {
    program_decls = [];
    var_info = Variable.Map.empty;
    var_table = StrMap.empty;
    contexts = Context.empty_world;
    compounds = Variable.Map.empty;
    constants = Variable.Map.empty;
    constraints = Variable.Map.empty;
    linearity = Variable.Map.empty;
    deps = Variable.Map.empty;
  }

  let contexts t = t.contexts

  let is_partner v t =
    match Variable.Map.find_opt v t.var_info with
    | None -> false
    | Some i -> VarInfo.is_partner i

  let is_constant v t = Variable.Map.mem v t.constants

  let is_linear v t =
    match Variable.Map.find_opt v t.linearity with
    | None | Some false -> false
    | Some true -> true

  let type_of v t =
    match Variable.Map.find_opt v t.var_info with
    | None ->
      Report.raise_internal_error "No info for variable %d" (Variable.uid v)
    | Some { typ; _ } -> typ

  let add_program_decl t (decl : contextualized declaration) =
    { t with program_decls = decl::t.program_decls }

  let bind_var (name : string) (vref : name_ref) t =
    let var_table =
      StrMap.update name (function
          | None -> Some vref
          | Some _ -> Report.raise_error "Name %s already registered" name)
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

  let bind_linearity (v : Variable.t) (linear : bool) t =
    { t with linearity = Variable.Map.add v linear t.linearity }

  let register_pool t (name : string) ~(computed : bool) =
    let v = Variable.create () in
    let info = VarInfo.{
        origin = Named name;
        typ = TMoney;
        kind = if computed then Computed else Intermediary;
      }
    in
    let t = bind_vinfo v info t in
    let t = bind_var name (RefPool v) t in
    let t = bind_linearity v true t in
    t, v

  let register_value t (name : string) ~(obs : bool)
      ~(linear : bool) ~(typ : ValueType.t) =
    let v = Variable.create () in
    let info = VarInfo.{
        origin = Named name;
        typ;
        kind = Value { observable = obs; cumulative = not linear };
      }
    in
    let t = bind_vinfo v info t in
    let t = bind_var name (RefValue v) t in
    let t = bind_linearity v linear t in
    t, v

  let register_input t (name : string) (typ : ValueType.t) (kind : input_kind) =
    match kind with
    | Attributable ->
      if typ <> ValueType.TMoney then Report.raise_internal_error "Wrong type for pool";
      let info = VarInfo.{
        origin = Named name;
        typ;
        kind = PoolInput { shadow = false };
      }
      in
      let t, v = register_pool t name ~computed:false in
      let t = bind_vinfo v info t in
      let t = bind_linearity v true t in
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
      let t = bind_linearity v true t in
      t, v

  let register_actor t (name : string) =
    let pv = Variable.create () in
    let info = VarInfo.{
        origin = Named name;
        typ = TMoney;
        kind = Partner;
      }
    in
    let t = bind_vinfo pv info t in
    let t = bind_linearity pv true t in
    bind_var name (RefActor (BaseActor pv)) t
    |> bind_compound pv pv

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
    let t = bind_linearity v false t in
    bind_var name (RefConst v) t
    |> bind_const v value

  let project_var t (v : Variable.t) (proj : Context.Group.t) =
    if is_partner v t || is_constant v t
    then v, Context.any_projection t.contexts
    else v, proj

  let find_pool_opt t (name : string) =
    match StrMap.find_opt name t.var_table with
    | Some (RefPool v) -> Some v
    | None -> None
    | Some _ -> Report.raise_error "Identifier %s is not a pool" name

  let find_actor ~(way : stream_way) t (name : string) =
    match StrMap.find_opt name t.var_table with
    | Some (RefActor ar) -> begin
        match ar with
        | Label (v, lway) ->
          if way <> lway then
            Report.raise_error "Labeled actor has wrong steam way";
          v
        | BaseActor a ->
          if way = Upstream then
            Report.raise_error "Upstream partner must be under a label";
          a
    end
    | Some _ -> Report.raise_error "Identifier %s is not an actor" name
    | None -> Report.(raise_unknown_id_error name Partner)

  let find_event t (name : string) =
    match StrMap.find_opt name t.var_table with
    | Some (RefEvent v) -> v
    | Some _ -> Report.raise_error "Identifier %s is not an event" name
    | None -> Report.(raise_unknown_id_error name Event)

  let find_misc_var t (name : string) =
    match StrMap.find_opt name t.var_table with
    | Some (RefROInput v | RefEvent v | RefValue v | RefConst v) -> v
    | Some (RefActor (BaseActor a)) -> a
    | Some (RefActor (Label _)) ->
      Report.raise_internal_error "Actor name %s should not exists" name
    | Some (RefPool _) ->
      Report.raise_error "Variable %s should be identified as pool" name
    | None -> Report.(raise_unknown_id_error name Any)

  let register_context_domain t (domain_name : string) (cases_names : string list) =
    { t with
      contexts = Context.add_domain t.contexts domain_name cases_names
    }

  let register_actor_label ~(way:stream_way) t (name : string) (label : string) =
    let base_actor =
      match StrMap.find_opt name t.var_table with
      | None -> Report.raise_error "Unknown actor %s" name
      | Some (RefActor (BaseActor a)) -> a
      | Some _ ->
        Report.raise_internal_error "'%s' should have been recognized as actor" name
    in
    let lname = name^"$"^label in
    match StrMap.find_opt lname t.var_table with
    | None ->
      let vl = Variable.create () in
      let info = VarInfo.{
          origin = LabelOfPartner { partner = base_actor; label };
          typ = ValueType.TMoney;
          kind = Partner;
        }
      in
      let t = bind_vinfo vl info t in
      let t = bind_var lname (RefActor (Label (vl, way))) t in
      let t =
        if way = Upstream then t else
          (* upstream labels are not part of partner total *)
          bind_compound vl base_actor t
      in
      t, vl
    | Some (RefActor (Label (v, lway))) ->
      if way <> lway then
        Report.raise_error "Cannot use same label %s on both ways" label;
      t, v
    | Some _ ->
      Report.raise_internal_error "'%s[%s]' should have been recognized as a \
                                   labeled actor"
        name label

  let add_var_constraint t (v : Variable.t)
      (from_var : Variable.t) (proj : Context.Group.t) =
    if is_partner v t || is_constant v t then t else
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
    if is_partner v t || is_constant v t then t else
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
    if VarInfo.is_partner (Variable.Map.find src t.var_info) then
      (* providing value, not a real dependency *)
      t
    else
      let tdeps =
        List.fold_left (fun tdeps dest ->
            match Variable.Map.find_opt dest t.deps with
            | None -> Variable.Set.add dest tdeps
            | Some tds -> Variable.Set.union (Variable.Set.add dest tds) tdeps)
          Variable.Set.empty dests
      in
      if Variable.Set.mem src tdeps then
        Report.raise_error "Cyclic repartition";
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
      dep_graph = Variable.Graph.empty;
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
                else Report.raise_error "Case %s do not belong in domain %s" c dom)
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

let find_actor ~(way : stream_way) acc (a : actor) =
  match a.actor_desc with
  | PlainActor name -> acc, Acc.find_actor ~way acc name
  | LabeledActor (name, label) ->
    Acc.register_actor_label ~way acc name label

let find_holder0 ~(way : stream_way) acc (h : holder) =
  match h.holder_desc with
  | Pool (name, ctx) ->
    let proj = projection_of_context_refinement (Acc.contexts acc) ctx in
    begin match Acc.find_pool_opt acc name with
    | Some v ->
      Acc.add_proj_constraint acc v proj, (v, proj)
    | None ->
      let acc, v = Acc.register_pool acc name ~computed:false in
      Acc.add_proj_constraint acc v proj, (v, proj)
    end
  | Actor a ->
    let acc, v = find_actor ~way acc a in
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
    Report.raise_error "Forbidden context refinement on destination";
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
      let v = Acc.find_misc_var acc name in
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

type formula_res = {
  acc : Acc.t;
  formula : contextualized formula;
  typ : ValueType.t;
  is_input_linear : bool;
}

let binop_typ (op : Surface.Ast.binop) (t1 : ValueType.t) (t2 : ValueType.t) =
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
  | _ -> Report.raise_typing_error ()

let comp_typ (t1 : ValueType.t) (t2 : ValueType.t) =
  match t1, t2 with
  | TInteger, TInteger
  | TInteger, TRational
  | TRational, TInteger
  | TRational, TRational
  | TMoney, TMoney
  | TDate, TDate
  | TDuration, TDuration -> ()
  | _ -> Report.raise_typing_error ()

let rec formula acc ~(for_ : Variable.t option) (f : source formula)
    ~(on_proj : Context.Group.t) : formula_res =
  let register_for acc (v,_) =
    match for_ with
    | None -> acc
    | Some for_ ->
      let acc = Acc.add_deps_from acc v [for_] in
      Acc.add_var_constraint acc v for_ on_proj
  in
  match f.formula_desc with
  | Literal l -> {
      acc;
      formula = { f with formula_desc = Literal l };
      is_input_linear = false;
      typ = Literal.type_of l;
    }
  | Named n ->
    let acc, v = named acc n ~on_proj in
    let acc = register_for acc v in
    { acc;
      formula = { f with formula_desc = Variable v };
      is_input_linear = Acc.is_linear (fst v) acc;
      typ = Acc.type_of (fst v) acc;
    }
  | Binop (op, f1, f2) ->
    let { acc; formula = f1; is_input_linear = l1; typ = t1 } =
      formula acc ~for_ f1 ~on_proj
    in
    let { acc; formula = f2; is_input_linear = l2; typ = t2 } =
      formula acc ~for_ f2 ~on_proj
    in
    let typ = binop_typ op t1 t2 in
    { acc; typ;
      formula = { f with formula_desc = Binop (op, f1, f2) };
      is_input_linear =
        match op with
        | Add | Sub -> l1 && l2
        | Mult | Div -> l1 || l2;
    }
  | Total f ->
    let { acc; formula; is_input_linear = _; typ } =
      formula acc ~for_ f ~on_proj
    in
    { acc; typ;
      formula = { formula with formula_desc = Total formula };
      is_input_linear = false;
    }
  | Instant f ->
    let { acc; formula; is_input_linear; typ } = formula acc ~for_ f ~on_proj in
    { acc; typ;
      formula = { formula with formula_desc = Instant formula };
      is_input_linear;
    }
  | Opposed (f, opp) ->
    let { acc; formula; is_input_linear; typ } = formula acc ~for_ f ~on_proj in
    let acc, opp = opposable acc ~on_proj opp in
    { acc; typ;
      formula = { formula with formula_desc = Opposed (formula, opp) };
      is_input_linear;
    }

and opposable acc ~(on_proj : Context.Group.t)
    (HolderOpp { opp_value; opp_provider; opp_towards } : source opposable) =
  let { acc; formula = opp_value; _ } =
    formula acc ~for_:None opp_value ~on_proj
  in
  let acc, opp_provider = find_actor ~way:Upstream acc opp_provider in
  let opp_towards =
    match opp_towards.actor_desc with
    | LabeledActor _ -> Report.raise_error "Opposition target must be a partner without label"
    | PlainActor s -> Acc.find_actor acc ~way:Downstream s
  in
  acc, VarOpp { opp_value; opp_provider; opp_towards }

let rec event_expr acc (e : source event_expr) ~(on_proj : Context.Group.t) =
  match e.event_expr_desc with
  | EventId name ->
    let v = Acc.find_event acc name in
    acc, {e with event_expr_desc = EventVar v}
  | EventComp (op, f1, f2) ->
    let { acc; formula = f1; typ = t1; _ } =
      formula acc ~for_:None f1 ~on_proj
    in
    let { acc; formula = f2; typ = t2; _ } =
      formula acc ~for_:None f2 ~on_proj
    in
    comp_typ t1 t2;
    acc, {e with event_expr_desc = EventComp (op, f1, f2)}
  | EventConj (e1, e2) ->
    let acc, e1 = event_expr acc e1 ~on_proj in
    let acc, e2 = event_expr acc e2 ~on_proj in
    acc, {e with event_expr_desc = EventConj (e1, e2)}
  | EventDisj (e1, e2) ->
    let acc, e1 = event_expr acc e1 ~on_proj in
    let acc, e2 = event_expr acc e2 ~on_proj in
    acc, {e with event_expr_desc = EventDisj (e1, e2)}

let redistribution acc ~(for_ : Variable.t option) (redist : source redistribution)
    ~(on_proj : Context.Group.t) ~(when_guarded : bool) =
  match redist.redistribution_desc with
  | Part (f, non_opp) ->
    let { acc; formula; _ } = formula acc ~for_ f ~on_proj in
    acc, { redist with redistribution_desc = Part (formula, non_opp) }
  | Flat f ->
    let { acc; formula; is_input_linear; typ } = formula acc ~for_ f ~on_proj in
    if typ <> ValueType.TMoney then Report.raise_typing_error ();
    if not (when_guarded || is_input_linear) then
      Report.raise_nonlinear_error ~loc:formula.formula_loc ();
    acc, { redist with redistribution_desc = Flat formula }
  | Retrocession (f, p) ->
    (* syntactic sugar *)
    let left_operand = f in
    let right_operand = Surface.Ast.formula (Named (Surface.Ast.named (Holder p))) in
    let f = Surface.Ast.formula (Binop (Mult, left_operand, right_operand)) in
    let { acc; formula; _ } = formula acc ~for_ f ~on_proj in
    acc, { redist with redistribution_desc = Flat formula }
  | Default -> acc, { redist with redistribution_desc = Default }

let redist_with_dest ~(src : Variable.t) ~(default_dest : Variable.t option)
    acc (WithHolder (redist, dest) : source redistrib_with_dest)
    ~(on_proj : Context.Group.t) ~(when_guarded : bool) =
  let acc, dest = destination_opt acc dest in
  let acc =
    match dest with
    | None -> acc
    | Some (dest,_) ->
      let acc = Acc.add_deps_from acc src [dest] in
      Acc.add_var_constraint acc src dest on_proj
  in
  let for_ =
    let o =
      Option.fold ~none:default_dest ~some:(fun (d,_) -> Some d) dest
    in
    Option.bind o (fun d -> if Acc.is_partner d acc then None else Some d)
  in
  let acc, redist = redistribution ~for_ acc redist ~on_proj ~when_guarded in
  let redist_wd = WithVar (redist, dest) in
  acc, redist_wd

let rec guarded_obj :
  type src_obj ctx_obj. Acc.t
  -> (Acc.t -> src_obj -> on_proj:Context.Group.t -> when_guarded:bool
      -> Acc.t * ctx_obj)
  -> (source, src_obj) guarded_redistrib -> on_proj:Context.Group.t
  -> when_guarded:bool
  -> Acc.t * (contextualized, ctx_obj) guarded_redistrib =
  fun acc obj_process obj ~on_proj ~when_guarded ->
  match obj with
  | Atom a ->
    let acc, ctx_obj = obj_process acc a ~on_proj ~when_guarded in
    acc, Atom ctx_obj
  | Branches { befores; afters } ->
    let acc, befores =
      List.fold_left_map (fun acc (c, r) ->
          let acc, cond = event_expr acc c ~on_proj in
          let acc, g_redist = guarded_obj acc obj_process r ~on_proj ~when_guarded in
          acc, (cond, g_redist)
        )
        acc befores
    in
    let acc, afters =
      List.fold_left_map (fun acc (c, r) ->
          let acc, cond = event_expr acc c ~on_proj in
          let acc, g_redist = guarded_obj acc obj_process r ~on_proj ~when_guarded in
          acc, (cond, g_redist)
        )
        acc afters
    in
    acc, Branches { befores; afters }
  | Whens gs ->
    let acc, redists =
      List.fold_left_map (fun acc (c, r) ->
          let acc, cond = event_expr acc c ~on_proj in
          let acc, g_redist =
            guarded_obj acc obj_process r ~on_proj ~when_guarded:true
          in
          acc, (cond, g_redist)
        )
        acc gs
    in
    acc, Whens redists

let operation acc (op : operation_decl) =
  let acc, (src, src_proj) = find_holder_as_source acc op.op_source in
  if not @@ Context.is_any_projection (Acc.contexts acc) src_proj then
    Report.raise_error "Forbidden refinment on source of operation";
  let on_proj =
    projection_of_context_selector acc op.op_context
  in
  let acc, default_dest = destination_opt acc op.op_default_dest in
  let redist_process acc rs ~on_proj ~when_guarded =
    List.fold_left_map (fun acc r ->
        let acc, red_wd =
          redist_with_dest ~src ~default_dest:(Option.map fst default_dest)
            acc r ~on_proj ~when_guarded
        in
        acc, red_wd
      )
      acc rs
  in
  let source = Acc.project_var acc src on_proj in
  let acc, g_redist =
    guarded_obj acc redist_process op.op_guarded_redistrib ~on_proj ~when_guarded:false
  in
  let acc =
    match default_dest with
    | None -> acc
    | Some (d,_) ->
      let acc = Acc.add_deps_from acc src [d] in
      Acc.add_var_constraint acc src d on_proj
  in
  acc,
  {
    ctx_op_label = op.op_label;
    ctx_op_default_dest = default_dest;
    ctx_op_source = source;
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

let advance _acc (_a : advance_decl) = Report.raise_error "no more advance"

let comp_pool acc (p : comp_pool_decl) =
  let acc, pool =
    match Acc.find_pool_opt acc p.comp_pool_name with
    | Some v -> acc, v
    | None -> Acc.register_pool acc p.comp_pool_name ~computed:true
  in
  let on_proj =
    projection_of_context_selector acc p.comp_pool_context
  in
  let acc = Acc.add_proj_constraint acc pool on_proj in
  let acc, g_redist =
    guarded_obj acc (fun acc f ~on_proj ~when_guarded ->
        let { acc; formula; is_input_linear; typ } =
          formula acc ~for_:(Some pool) f ~on_proj
        in
        if typ <> ValueType.TMoney then Report.raise_typing_error ();
        if not ( when_guarded || is_input_linear) then
          Report.raise_nonlinear_error ~loc:formula.formula_loc ();
        acc, formula)
      p.comp_pool_guarded_value ~on_proj ~when_guarded:false
  in
  acc,
  {
    ctx_comp_pool_var = (pool, on_proj);
    ctx_comp_pool_guarded_value = g_redist;
  }

let value acc (v : val_decl) =
  let obs = v.val_observable in
  let on_proj = Context.any_projection (Acc.contexts acc) in
  let { acc; formula; is_input_linear; typ } =
    formula acc ~for_:None v.val_formula ~on_proj
  in
  let acc, var =
    Acc.register_value acc v.val_name ~obs ~linear:is_input_linear ~typ
  in
  acc,
  {
    ctx_val_var = var, on_proj;
    ctx_val_formula = formula;
    ctx_val_observable = obs;
    ctx_val_linear = is_input_linear;
  }

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
  | DHolderPool p ->
    let acc, p = comp_pool acc p in
    Acc.add_program_decl acc (DVarPool p)
  | DConstant c -> constant acc c
  | DHolderValue v ->
    let acc, v = value acc v in
    Acc.add_program_decl acc (DVarValue v)
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
