open Surface
open Internal
open Ir

(* This pass does mainly two things :
   - Swapping abstract variables (user-side names) to their contextualized
   versions, the concrete computation objects.
   - Building redistribution trees for each variables flowing elsewhere. *)

module Acc : sig
  type t = {
    pinfos : Ast.program_infos;
    used_variables : Variable.Set.t;
    ctx_derivations : Variable.t Context.Group.Map.t Variable.Map.t;
    trees : RedistTree.t Variable.Map.t;
    events : event Variable.Map.t;
  }

  type derivation_mode = Strict | Inclusive

  type compound_derivation =
      ActorComp of { base : Variable.t; compound : Variable.t list; }
    | ContextVar of Variable.t list

  val contexts : t -> Context.world
  val make : Ast.program_infos -> t
  val var_shape : t -> Variable.t -> Context.shape
  val type_of : t -> Variable.t -> ValueType.t
  val find_const_opt : t -> Variable.t -> Ast.literal option
  val get_derivative_var :
    t -> Variable.t -> Context.Group.t -> t * Variable.t

  val derive_ctx_variables :
    mode:derivation_mode ->
    t -> Variable.t -> Context.Group.t -> t * compound_derivation

  val register_event : t -> Variable.t -> event -> t
  val lift_event : t -> event -> t * Variable.t
  val add_tree : t -> source:Variable.t -> RedistTree.kind_tree -> t
  val add_default : t -> source:Variable.t -> Variable.t -> t
  val add_deficit : t -> provider:Variable.t -> Variable.t -> t
  val filter_usage : t -> t

end = struct

  type t = {
    pinfos : Ast.program_infos;
    used_variables : Variable.Set.t;
    ctx_derivations : Variable.t Context.Group.Map.t Variable.Map.t;
    (* Association of abstract variables to their context group versions *)
    trees : RedistTree.t Variable.Map.t;
    events : event Variable.Map.t;
  }

  let contexts t = t.pinfos.contexts

  let make (pinfos : Ast.program_infos) =
    { pinfos;
      used_variables = Variable.Set.empty;
      ctx_derivations = Variable.Map.empty;
      trees = Variable.Map.empty;
      events = Variable.Map.empty;
    }

  (** Explicit flagging of program objects to remove clutter generated in
      previous pass *)
  let flag_variable_usage t (v : Variable.t) =
    { t with used_variables = Variable.Set.add v t.used_variables }

  let var_shape t (v : Variable.t) =
    match Variable.Map.find_opt v t.pinfos.var_shapes with
    | Some shape -> shape
    | None -> Errors.raise_error "No shape for var %d" (Variable.uid v)

  let type_of t v =
    match Variable.Map.find_opt v t.pinfos.types with
    | Some typ -> typ
    | None -> Errors.raise_error "(internal) Cannot find type of variable"

  let is_actor t (v : Variable.t) =
    Variable.Map.mem v t.pinfos.actors

  let find_const_opt t (v : Variable.t) =
    Variable.Map.find_opt v t.pinfos.Ast.constants

  let find_compound_vars t (v : Variable.t) =
    match Variable.Map.find_opt v t.pinfos.compounds with
    | None -> [v]
    | Some vs -> Variable.Set.elements vs

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
      (* Fragile: Generating new variable for the given group without checking
         whever there is already one whose group overlap. This should not be an
         issue as the analysis should not produce such cases, but vulnerable
         nonetheless. *)
      let dv = Variable.create () in
      let { Variable.var_name } = Variable.Map.find v t.pinfos.var_info in
      let typ = type_of t v in
      let t = flag_variable_usage t dv in
      let t = {
        t with
        pinfos = {
          t.pinfos with
          var_info = Variable.Map.add dv { Variable.var_name } t.pinfos.var_info;
          types =  Variable.Map.add dv typ t.pinfos.types;
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
      let t =
        match Variable.Map.find_opt v t.pinfos.inputs with
        | None -> t
        | Some kind ->
          { t with pinfos = { t.pinfos with inputs = Variable.Map.add dv kind t.pinfos.inputs }}
      in
      let t =
        match Variable.Map.find_opt v t.pinfos.actors with
        | None -> t
        | Some way ->
          { t with pinfos = { t.pinfos with actors = Variable.Map.add dv way t.pinfos.actors }}
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
      let t = List.fold_left flag_variable_usage t compound in
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
      let var_name =
        match event with
        | EvtOnRaise evt ->
          "when_" ^ (Variable.Map.find evt t.pinfos.var_info).var_name
        | _ -> Variable.unique_anon_name "anon_event"
      in
      let v = Variable.create () in
      let t = flag_variable_usage t v in
      let t =
        { t with
          pinfos =
            { t.pinfos with
              var_info = Variable.Map.add v { Variable.var_name } t.pinfos.var_info;
              types = Variable.Map.add v ValueType.TEvent t.pinfos.types;
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

  let add_deficit t ~(provider : Variable.t) (p : Variable.t) =
    let trees =
      Variable.Map.update p (function
          | None -> Some (RedistTree.of_deficit provider)
          | Some existing ->
            Some (RedistTree.add_deficit provider existing))
        t.trees
    in
    { t with trees }

  let filter_usage t =
    let filter map =
      Variable.Map.filter (fun v _ -> Variable.Set.mem v t.used_variables) map
    in
    { t with
      pinfos = {
        t.pinfos with
        var_info = filter t.pinfos.var_info;
        var_shapes = filter t.pinfos.var_shapes;
        inputs = filter t.pinfos.inputs;
        actors = filter t.pinfos.actors;
        compounds = filter t.pinfos.compounds;
        types = filter t.pinfos.types;
        constants = filter t.pinfos.constants;
      }
    }

end

let shape_of_ctx_var acc (v : Ast.contextualized_variable) =
  let v, proj = v in
  let vshape = Acc.var_shape acc v in
  Context.shape_filter_projection vshape proj

let resolve_projection_context acc ~context ~refinement =
  (* Refinement has priority over operation context *)
  (* TODO: fix case where refinement explicitly take every groups *)
  if Context.is_any_projection (Acc.contexts acc) refinement then context else refinement

(* Used only to compute constant quoteparts *)
let rec reduce_formula (f : formula) =
  let to_rationnal = function
    | Literal (LInteger f) -> Some R.(~$f)
    | Literal (LRational f) -> Some f
    | _ -> None
  in
  let lit_of_op op f1 f2 =
    let r = match (op : binop) with
    | Add -> R.(f1 + f2)
    | Sub -> R.(f1 - f2)
    | Mult -> R.(f1 * f2)
    | Div -> R.(f1 / f2)
    in
    Literal (LRational r)
  in
  match f with
  | Literal _
  | Variable _ -> f
  | Binop (op, f1, f2) ->
    let f1 = to_rationnal (reduce_formula f1) in
    let f2 = to_rationnal (reduce_formula f2) in
      match f1, f2 with
      | Some f1, Some f2 -> lit_of_op op f1 f2
      | _ -> f

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
  Binop (op, f1, f2), typ

(* When refering a named variable, it may actually refers to several variables
   according to the context. In this case, it refers to their sum. *)
let aggregate_vars ~view (typ : ValueType.t) (vars : Variable.t list) =
  begin match typ with
    | ValueType.TInteger
    | ValueType.TRational
    | ValueType.TMoney -> ()
    | ValueType.TEvent
    | ValueType.TDate
    | ValueType.TDuration ->
      Errors.raise_error
        "(internal) there should not exist multiple derivatives for \
         variable of type %a"
        FormatAst.print_type typ
  end;
  match vars with
  | [] -> Errors.raise_error "(internal) should have found derivative vars"
  | v::vs ->
    List.fold_left (fun f v ->
        (Binop (Add, f, Variable (v, view))))
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
    (* To preserve the intuitive priority order of the source program, the
       resulting tree of a chain of befores and afters looks like two combs of
       opposite ways one below the other, regardless of the actual events.

       This can be improved with a proper analysis on relations between those
       events, as well as reveal incoherent or dead branches. *)
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
      Acc.add_deficit acc ~provider pool)
    acc pool_local_shape

let translate_declaration acc (decl : Ast.contextualized Ast.declaration) =
  match decl with
  | DVarOperation o -> translate_operation acc o
  | DVarEvent e ->
    let acc, evt_formula = translate_event acc e.ctx_event_expr in
    Acc.register_event acc e.ctx_event_var evt_formula
  | DVarDefault d -> translate_default acc d
  | DVarDeficit d -> translate_deficit acc d

let level_fractional_attribution (acc : Acc.t) (src : Variable.t) (t : RedistTree.t) =
  match t with
  | Flat _ -> acc
  | Fractions { base_shares; balance; branches } ->
    let redist_part (r : RedistTree.frac RedistTree.redist) =
      match r with
      | NoInfo -> R.zero
      | Shares sh -> Variable.Map.fold (fun _v -> R.(+)) sh R.zero
    in
    let branch_part default (tree : RedistTree.frac RedistTree.tree) =
      Variable.BDT.merge (fun _k d t ->
          match d, t with
          | None, None -> None
          | None, Some r -> Some R.(- redist_part r)
          | d, None -> d
          | Some d, Some r -> Some R.(d - (redist_part r))
        )
        default tree
    in
    match balance with
    | BalanceTree _ ->
      Errors.raise_error "(internal) Default tree already computed"
    | BalanceVars balance ->
      let base_part = redist_part base_shares in
      let default_redist = Variable.BDT.Action R.(one - base_part) in
      let branches_parts =
        List.fold_left branch_part default_redist branches
      in
      let make_default_redist part =
        match balance.default with
        | Some d -> Variable.BDT.Action (RedistTree.Shares (Variable.Map.singleton d part))
        | None -> Errors.raise_error "Missing fractionnal part, may needs a default"
      in
      let make_deficit_redist part =
        Variable.BDT.Action
          (RedistTree.Flats { transfers = Variable.Map.empty;
                   balances = Variable.Map.singleton src part
                 })
      in
      let default_tree, deficit_tree =
        Variable.BDT.fold branches_parts
          ~noaction:(fun _k -> Variable.BDT.(NoAction, NoAction))
          ~action:(fun _k part ->
              if R.(part > zero)
              then make_default_redist part, NoAction
              else if R.(part < zero)
              then NoAction, make_deficit_redist R.(~- part)
              else NoAction, NoAction)
          ~decision:(fun _k evt (adft, adfc) (bdft, bdfc) ->
              let branch_or_nothing before after =
                match before, after with
                | Variable.BDT.(NoAction, NoAction) -> Variable.BDT.NoAction
                | _, _ ->
                  Decision (evt,
                    Variable.BDT.cut (Variable.Map.singleton evt true) after,
                    Variable.BDT.cut (Variable.Map.singleton evt false) before)
              in
              branch_or_nothing bdft adft, branch_or_nothing bdfc adfc)
      in
      let src_tree =
        RedistTree.Fractions
          { base_shares; branches; balance = BalanceTree default_tree }
      in
      let acc = { acc with trees = Variable.Map.add src src_tree acc.trees } in
      match balance.deficit with
      | Some provider ->
        if deficit_tree = Variable.BDT.NoAction then
          acc (* TODO warning unnecessary deficit *)
        else
          Acc.add_tree acc ~source:provider RedistTree.(FlatTree deficit_tree)
      | None ->
        if deficit_tree <> Variable.BDT.NoAction
        then Errors.raise_error "Fractionnal repartition exceeds total, needs a deficit"
        else acc

let level_attributions (acc : Acc.t) =
  Variable.Map.fold (fun src tree acc ->
        level_fractional_attribution acc src tree)
    acc.trees acc

let dependancy_graph (acc : Acc.t) =
  let rec dep_formula graph k src (f : formula) =
    match f with
    | Literal _ -> graph
    | Variable (v, _) -> Variable.Graph.add_edge_e graph (v, k, src)
    | Binop (_, f1, f2) ->
      let graph = dep_formula graph k src f1 in
      dep_formula graph k src f2
  in
  let dep_redist (type a) graph k src (r : a RedistTree.redist) =
    match r with
    | RedistTree.NoInfo -> graph
    | RedistTree.Shares sh ->
      Variable.Map.fold (fun dest _ graph -> Variable.Graph.add_edge_e graph (src, k, dest))
        sh graph
    | RedistTree.Flats fs ->
      let graph =
        Variable.Map.fold (fun dest formula graph ->
            let graph = Variable.Graph.add_edge_e graph (src, k, dest) in
            dep_formula graph k src formula)
          fs.transfers graph
      in
      Variable.Map.fold (fun dest _factor graph ->
          Variable.Graph.add_edge_e graph (src, k, dest))
        fs.balances graph
  in
  let rec dep_tree :
    type a. Variable.Graph.t -> bool Variable.Map.t -> Variable.t -> a RedistTree.tree -> Variable.Graph.t =
    fun graph k src tree ->
      match tree with
      | NoAction -> graph
      | Action r -> dep_redist graph k src r
      | Decision (cond, after, before) ->
        let graph = dep_tree graph (Variable.Map.add cond false k) src before in
        dep_tree graph (Variable.Map.add cond true k) src after
  in
  Variable.Map.fold (fun src trees graph ->
      match trees with
      | RedistTree.Fractions { base_shares; balance; branches } ->
        let graph = dep_redist graph Variable.Map.empty src base_shares in
        let graph =
          match balance with
          | BalanceVars b ->
            let graph = match b.default with
              | Some d -> Variable.Graph.add_edge graph src d
              | None -> graph
            in
            begin match b.deficit with
              | Some d -> Variable.Graph.add_edge graph d src
              | None -> graph
            end
          | BalanceTree tree -> dep_tree graph Variable.Map.empty src tree
        in
        List.fold_left (fun graph tree -> dep_tree graph Variable.Map.empty src tree)
          graph branches
      | RedistTree.Flat fs ->
        List.fold_left (fun graph tree -> dep_tree graph Variable.Map.empty src tree)
          graph fs)
    acc.trees Variable.Graph.empty

let evaluation_order (acc : Acc.t) (g : Variable.Graph.t) =
  let module SCC = Graph.Components.Make(Variable.Graph) in
  let scc = SCC.scc_list g in
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
  let dep_graph = dependancy_graph acc in
  let eval_order = evaluation_order acc dep_graph in
  {
    infos = acc.pinfos;
    trees = acc.trees;
    events = acc.events;
    eval_order;
    dep_graph;
    equations = Variable.Map.empty;
  }
