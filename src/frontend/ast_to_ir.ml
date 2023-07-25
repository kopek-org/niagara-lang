
module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

(*************)

open Ir

module Acc = struct

  (* type t = program *)

  type derivation_mode = Strict | Inclusive

  let make (infos : Ast.program_infos) =
    { infos;
      ctx_derivations = Variable.Map.empty;
      trees = Variable.Map.empty;
      events = Variable.Map.empty; }

  let contexts t = t.infos.contexts

  let var_shape t (v : Variable.t) =
    match Variable.Map.find_opt v t.infos.var_shapes with
    | Some shape -> shape
    | None -> Errors.raise_error "No shape for var %d" v

  let type_of t v =
    match Variable.Map.find_opt v t.infos.types with
    | Some typ -> typ
    | None -> Errors.raise_error "(internal) Cannot find type of variable"

  let anon_var_name =
    let c = ref 0 in
    fun name ->
      let i = !c in incr c;
      "anon_" ^ name ^ "_" ^ string_of_int i

  let find_const_opt t (v : Variable.t) =
    Variable.Map.find_opt v t.infos.Ast.constants

  let find_derivation_opt t (v : Variable.t) (ctx : Context.group) =
    match Variable.Map.find_opt v t.ctx_derivations with
    | None -> None
    | Some cgm ->
      Context.GroupMap.find_opt ctx cgm

  let get_derivative_var t (v : Variable.t) (ctx : Context.group) =
    match find_derivation_opt t v ctx with
    | Some v -> t, v
    | None ->
      let dv = Variable.new_var () in
      let { Variable.var_name } = Variable.Map.find v t.infos.var_info in
      let typ = type_of t v in
      let t = {
        t with
        infos = {
          t.infos with
          var_info = Variable.Map.add dv { Variable.var_name } t.infos.var_info;
          types =  Variable.Map.add dv typ t.infos.types;
          var_shapes = Variable.Map.add dv (Context.shape_of_groups [ctx]) t.infos.var_shapes;
        };
        ctx_derivations =
          Variable.Map.update v (function
              | None -> Some (Context.GroupMap.singleton ctx dv)
              | Some groups -> Some (Context.GroupMap.add ctx dv groups)
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
      t, dv

  let derive_ctx_variables ~mode t (v : Variable.t) (ctx : Context.projection) =
    let shape = var_shape t v in
    let subshape =
      match mode with
      | Strict -> Context.projection_subshape_strict (contexts t) shape ctx
      | Inclusive -> Context.projection_subshape_inclusive (contexts t) shape ctx
    in
    Context.fold_shape (fun (t, vars) group ->
        let t, v = get_derivative_var t v group in
        t, v::vars)
      (t, []) subshape

  let register_event t (v : Variable.t) (event : event) =
    { t with
      events = Variable.Map.add v event t.events
    }

  let lift_event t (event : event) =
    let var_name = anon_var_name "event" in
    let v = Variable.new_var () in
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

  let add_redist t ~(source : Variable.t) (tree : RedistTree.tree) =
    let add_tree_to_redists rs t =
      match (t : RedistTree.tree) with
      | RedistTree.Redist r ->
        List.map (function
            | RedistTree.Redist r' ->
              RedistTree.Redist (RedistTree.merge_redist r' r)
            | RedistTree.Branch _ as b -> b)
          rs
      | Branch _ -> t::rs
    in
    let trees =
      Variable.Map.update source (function
          | None -> Some [tree]
          | Some existing_tree ->
            Some (add_tree_to_redists existing_tree tree))
        t.trees
    in
    { t with trees }

end

(* module Env = struct *)

(*   type t = { *)
(*     view : flow_view; *)
(*     context : Context.t; *)
(*     source_layout : var_layout; *)
(*   } *)

(*   let empty = { *)
(*     view = AtInstant; *)
(*     context = Context.everything; *)
(*     source_layout = SimpleVar; *)
(*   } *)

(*   let with_view view t = { t with view } *)

(*   let with_context context t = { t with context } *)

(*   let with_source_layout source_layout t = { t with source_layout } *)

(*   let view t = t.view *)

(*   let context t = t.context *)

(*   let source_layout t = t.source_layout *)

(* end *)

let shape_of_ctx_var acc (v : Ast.contextualized_variable) =
  let v, proj = v in
  let vshape = Acc.var_shape acc v in
  Context.projection_subshape_strict (Acc.contexts acc)
    vshape proj

let resolve_projection_context ~context ~refinement =
  if Context.is_any_projection refinement then context else refinement

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
        Literal (LDate (CalendarLib.Date.add d dr))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DrAdd, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LDuration dr1), Literal (LDuration dr2) ->
        Literal (LDuration (CalendarLib.Date.Period.add dr1 dr2))
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
        Literal (LDate (CalendarLib.Date.rem d dr))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop (DrSub, f1, f2) ->
    begin match reduce_formula f1, reduce_formula f2 with
      | Literal (LDuration dr1), Literal (LDuration dr2) ->
        Literal (LDuration (CalendarLib.Date.Period.sub dr1 dr2))
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
        let open CalendarLib in
        let ddr = Date.Period.nb_days dr in
        Literal (LDuration (Date.Period.day (int_of_float (float_of_int ddr *. f +. 0.5))))
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
        let open CalendarLib in
        let ddr = Date.Period.nb_days dr in
        Literal (LDuration (Date.Period.day (int_of_float (float_of_int ddr /. f +. 0.5))))
      | Literal _, Literal _ -> assert false
      | _ -> f
    end
  | Binop ((IEq|REq|MEq|DEq|DrEq), _, _) -> f

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

let translate_comp (comp : Ast.comp)
    (f1, t1 : formula * ValueType.t)
    (f2, t2 : formula * ValueType.t) =
  match comp, t1, t2 with
  | Eq, TInteger, TInteger -> Binop (IEq, f1, f2), ValueType.TEvent
  | Eq, TInteger, TRational -> Binop (REq, RCast f1, f2), ValueType.TEvent
  | Eq, TRational, TInteger -> Binop (REq, f1, RCast f2), ValueType.TEvent
  | Eq, TRational, TRational -> Binop (REq, f1, f2), ValueType.TEvent
  | Eq, TMoney, TMoney -> Binop (MEq, f1, f2), ValueType.TEvent
  | Eq, TDate, TDate -> Binop (DEq, f1, f2), ValueType.TEvent
  | Eq, TDuration, TDuration -> Binop (DrEq, f1, f2), ValueType.TEvent
  | _ -> Errors.raise_error "Mismatching types for comp"

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

let rec translate_formula ~(ctx : Context.projection) acc ~(view : flow_view)
    (f : Ast.contextualized Ast.formula) =
  match f with
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
      let proj = resolve_projection_context ~context:ctx ~refinement:proj in
      let acc, v = Acc.derive_ctx_variables ~mode:Strict acc v proj in
      let f = aggregate_vars ~view t v in
      acc, (f, t)
    end
  | Binop (op, f1, f2) ->
    let acc, f1 = translate_formula ~ctx acc ~view f1 in
    let acc, f2 = translate_formula ~ctx acc ~view f2 in
    acc, (translate_binop op f1 f2)
  | Comp (comp, f1, f2) ->
    let acc, f1 = translate_formula ~ctx acc ~view f1 in
    let acc, f2 = translate_formula ~ctx acc ~view f2 in
    acc, (translate_comp comp f1 f2)
  | Instant f -> translate_formula ~ctx acc ~view:AtInstant f
  | Total f -> translate_formula ~ctx acc ~view:Cumulated f

let translate_redist ~(ctx : Context.projection) acc ~(dest : Ast.contextualized_variable)
    (redist : Ast.contextualized Ast.redistribution) =
  let proj = resolve_projection_context ~context:ctx ~refinement:(snd dest) in
  let acc, dest = Acc.derive_ctx_variables ~mode:Inclusive acc (fst dest) proj in
  let dest =
    match dest with
    | [dest] -> dest
    | _ -> Errors.raise_error "(internal) Destination context inapplicable"
  in
  match redist with
  | Part f ->
    let acc, (f, ft) = translate_formula ~ctx acc ~view:AtInstant f in
    acc, RedistTree.share dest (reduce_formula f, ft)
  | Flat f ->
    let acc, f = translate_formula ~ctx acc ~view:AtInstant f in
    acc, RedistTree.flat dest f

let rec translate_event acc (event : Ast.contextualized Ast.event_expr) =
  match event with
  | EventVar v -> acc, EvtVar v
  | EventConj (e1, e2) ->
    let acc, e1 = translate_event acc e1 in
    let acc, e2 = translate_event acc e2 in
    acc, EvtAnd(e1,e2)
  | EventDisj (e1, e2) ->
    let acc, e1 = translate_event acc e1 in
    let acc, e2 = translate_event acc e2 in
    acc, EvtOr(e1,e2)
  | EventFormula f ->
    let acc, (f, t) = translate_formula ~ctx:Context.any_projection acc ~view:Cumulated f in
    match (t : ValueType.t) with
    | TEvent -> acc, EvtCond f
    | TDate -> acc, EvtDate f
    | TInteger | TRational
    | TMoney | TDuration -> Errors.raise_error "Formula is not an event"

let lift_event acc (event : Ast.contextualized Ast.event_expr) =
  let acc, evt = translate_event acc event in
  match evt with
  | EvtVar v -> acc, v
  | _ ->
    let acc, v = Acc.lift_event acc evt in
    acc, v

let translate_redist_with_dest ~(ctx : Context.projection) acc
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

let rec translate_guarded_redist ~(ctx : Context.projection) acc
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
      | r::rs -> acc, RedistTree.Redist (List.fold_left RedistTree.merge_redist r rs)
    end
  | Guardeds gs ->
    translate_guard_group ~ctx acc ~default_dest gs

and translate_guard_group ~ctx acc ~default_dest
    (group : (Ast.contextualized Ast.guard * Ast.contextualized Ast.guarded_redistrib) list)
  =
  let noinfo = RedistTree.Redist NoInfo in
  let rec aux acc group
      (partial : 'acc -> RedistTree.tree -> 'acc * RedistTree.tree) =
    match group with
    | [] -> partial acc noinfo
    | (guard, redist)::group ->
      let acc, tree = translate_guarded_redist ~ctx acc ~default_dest redist in
      match (guard : Ast.contextualized Ast.guard) with
      | Before event ->
        let acc, evt = lift_event acc event in
        let partial acc after =
          partial acc (RedistTree.Branch {evt; before = tree; after})
        in
        aux acc group partial
      | After event ->
        let acc, evt = lift_event acc event in
        let partial acc before =
          match (before : RedistTree.tree) with
          | Redist NoInfo ->
            let acc, before = partial acc noinfo in
            acc, RedistTree.Branch {evt; before; after = tree}
          | _ -> Errors.raise_error "pre-event guard after post-event guard have \
                                     undefined semantics"
        in
        aux acc group partial
      | When _ -> assert false
  in
  aux acc group (fun acc tree -> acc, tree)

let translate_operation acc (o : Ast.ctx_operation_decl) =
  let source_local_shape = shape_of_ctx_var acc o.ctx_op_source in
  Context.fold_shape (fun acc group ->
      let acc, source = Acc.get_derivative_var acc (fst o.ctx_op_source) group in
      let ctx = Context.projection_of_group group in
      let acc, tree =
        translate_guarded_redist ~ctx acc ~default_dest:o.ctx_op_default_dest
          o.ctx_op_guarded_redistrib
      in
      Acc.add_redist acc ~source tree)
    acc source_local_shape

let translate_declaration acc (decl : Ast.contextualized Ast.declaration) =
  match decl with
  | DVarOperation o -> translate_operation acc o
  | DVarEvent e ->
    let acc, evt_formula = translate_event acc e.ctx_event_expr in
    Acc.register_event acc e.ctx_event_var evt_formula
  | DVarAdvance _
  | DVarDefault _
  | DVarDeficit _ -> assert false

let translate_program (Contextualized (infos, prog) : Ast.contextualized Ast.program) =
  let acc = Acc.make infos in
  let acc =
    List.fold_left
      (fun acc decl ->
         translate_declaration acc decl)
      acc prog
  in
  acc
