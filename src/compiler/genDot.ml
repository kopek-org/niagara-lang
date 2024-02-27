open Odot
open Surface
open Internal
open Ir

type var_incl_policy = Exact | DepsTo | DepsOf

type filtering = {
  event_knowledge : bool Variable.Map.t;
  context : Context.Group.t option;
  variable_inclusion : (Variable.Set.t * var_incl_policy) option;
}

let no_filtering = {
  event_knowledge = Variable.Map.empty;
  context = None;
  variable_inclusion = None;
}

let label s =
  Simple_id "label", Some (Double_quoted_id s)

let tlabel s =
  Simple_id "taillabel", Some (Double_quoted_id s)

let color s =
  Simple_id "color", Some (Double_quoted_id s)

let shape s =
  Simple_id "shape", Some (Double_quoted_id s)

let add_var p g v =
  let is_actor = Variable.Map.mem v p.infos.Ast.actors in
  let ncolor =
    if is_actor then color "red" else color "blue"
  in
  let n =
    Double_quoted_id(Format.asprintf "%a"
                       (FormatIr.print_variable ~with_ctx:(not is_actor) p.infos) v),
    None
  in
  g.stmt_list <- Stmt_node (n,[ncolor])::g.stmt_list;
  n

let id = let c = ref 0 in fun () -> incr c; "anon"^(string_of_int !c)

let add_event p g v =
  let n = Double_quoted_id(id ()), None in
  g.stmt_list <-
    Stmt_node (n,
               [shape "box";
                label (Format.asprintf "%a" (FormatIr.print_variable ~with_ctx:false p.infos) v)
               ])
    ::g.stmt_list;
  n

let add_edge g s e ls =
  if s <> e then
  g.stmt_list <-
    Stmt_edge (Edge_node_id s, [Edge_node_id e],ls)
    ::g.stmt_list

let dot_of_redist (type a) p g (r : a Ir.RedistTree.redist) =
  match r with
  | NoInfo -> []
  | Shares sh ->
    Variable.Map.fold (fun v por es ->
      match (por : RedistTree.part_or_remain) with
      | Remain -> es
      | Part f ->
        let dest = add_var p g v in
        let l = Format.asprintf "%a%%" R.print_as_dec_repr R.(f * ~$100) in
        let attr = [ label l ] in
        (dest, attr)::es)
      sh []
  | Flats fs ->
    let es =
      Variable.Map.fold (fun dest s es ->
          let dest = add_var p g dest in
          let attr = [ label (Format.asprintf "%a" (FormatIr.print_formula p.infos) s) ] in
          (dest, attr)::es)
        fs.transfers []
    in
    Variable.Map.fold (fun dest f es ->
        let dest = add_var p g dest in
        let attr = [ label (Format.asprintf "deficit %a%%" R.print_as_dec_repr R.(f * ~$100)) ] in
        (dest, attr)::es)
      fs.balances es

let rec dot_of_tree : type a. program -> graph -> a Ir.RedistTree.tree -> ((id * _ option) * attr list) list =
  fun p g t ->
  match t with
  | NoAction -> []
  | Action r ->
    dot_of_redist p g r
  | Decision (evt, after, before) ->
    begin 
      match dot_of_tree p g before,
            dot_of_tree p g after with
      | [], [] -> []
      | bf, af -> 
        let e = add_event p g evt in
        List.iter (fun (bn,l) -> add_edge g e bn ((tlabel "avant")::l)) bf;
        List.iter (fun (an,l) -> add_edge g e an ((tlabel "apres")::l)) af;
        [e, []]
    end

let dot_of_trees p g ts =
  List.map (dot_of_tree p g) ts
  |> List.flatten

let filter_redist (type a) ~filter (r : a RedistTree.redist) : a RedistTree.redist =
  let vars_filter m =
    match filter.variable_inclusion with
    | None -> m
    | Some (vars, _) -> Variable.Map.filter (fun v _ -> Variable.Set.mem v vars) m
  in
  match r with
  | NoInfo -> NoInfo
  | Shares sh ->
    let fsh = vars_filter sh in
    if Variable.Map.is_empty fsh
    then NoInfo
    else Shares fsh
  | Flats { transfers; balances } ->
    let transfers = vars_filter transfers in
    let balances = vars_filter balances in
    if Variable.Map.is_empty transfers && Variable.Map.is_empty balances
    then NoInfo
    else Flats { transfers; balances }

let filter_tree ~filter t =
  let rec aux : type a. a RedistTree.tree -> a RedistTree.tree =
    fun t -> match t with
    | NoAction -> NoAction
    | Action r ->
      begin match filter_redist ~filter r with
        | NoInfo -> NoAction
        | r -> Action r
      end
    | Decision (evt, after, before) ->
      match Variable.Map.find_opt evt filter.event_knowledge with
      | None ->
        begin match aux after, aux before with
        | NoAction, NoAction -> NoAction
        | after, before -> Decision (evt, after, before)
        end
      | Some true ->
        begin match aux after with
          | NoAction -> NoAction
          | a -> Decision (evt, a, NoAction)
        end
      | Some false ->
        begin match aux before with
          | NoAction -> NoAction
          | a -> Decision (evt, NoAction, a)
        end
  in
  aux t

let dot_of_t ~filter p g src t =
  match (t : Ir.RedistTree.t) with
  | Fractions { base_shares; balance; branches } ->
    dot_of_redist p g (filter_redist ~filter base_shares)
    @ dot_of_trees p g (List.map (filter_tree ~filter) branches)
    @ (match (balance : Ir.RedistTree.frac_balance) with
        | BalanceVars b ->
          begin match b.deficit with
            | None -> ()
            | Some v ->
              let v = add_var p g v in
              add_edge g v src [label "deficit"]
          end;
          begin match b.default with
            | None -> []
            | Some v ->
              let v = add_var p g v in
              [v, [label "default"]]
          end
        | BalanceTree tree -> dot_of_tree p g (filter_tree ~filter tree))
  | Flat fs -> dot_of_trees p g (List.map (filter_tree ~filter) fs)

let graph_filter p g ~(filter : filtering) (starts_v : Variable.Set.t) =
  let match_context v =
    match filter.context with
    | None -> true
    | Some ctx ->
      let s = Variable.Map.find v p.infos.var_shapes in
      not (Context.is_empty_shape (Context.shape_overlap_subshape s ctx))
  in
  let rec aux v incl =
    let es = Variable.Graph.succ_e g v in
    match es with [] -> Some incl | _ ->
      List.fold_left (fun inclo (_s, k, e) ->
          if Variable.Set.mem e incl then inclo else
          if Variable.BDT.contradictory_knowledge filter.event_knowledge k
          then inclo else
          if match_context e then
            match inclo, aux e (Variable.Set.add e incl) with
            | inclo, None -> inclo
            | None, Some i -> Some i
            | Some io, Some i -> Some (Variable.Set.union i io)
          else inclo)
        None es
  in
  let starts = Variable.Set.filter (Variable.Graph.mem_vertex g) starts_v in
  Variable.Set.fold (fun v incl ->
      match aux v incl with
      | None -> incl
      | Some i -> Variable.Set.union i incl)
    starts starts

let graph_of_program p filter =
  let graph = {
    strict = false;
    kind = Digraph;
    id = None;
    stmt_list = [
      (* Stmt_attr (Attr_graph [Simple_id "ranksep", Some (Simple_id "0.8")]) *)
    ];
  }
  in
  let variable_inclusion =
    match filter.variable_inclusion with
    | None | Some (_, Exact) -> filter.variable_inclusion
    | Some (vars, DepsOf) ->
      let module GOP = Graph.Oper.P(Variable.Graph) in
      let rev_graph = GOP.mirror p.dep_graph in
      let vars = graph_filter p rev_graph ~filter vars in
      Some (vars, Exact)
    | Some (vars, DepsTo) ->
      let vars = graph_filter p p.dep_graph ~filter vars in
      Some (vars, Exact)
  in
  let filter = { filter with variable_inclusion } in
  Variable.Map.iter (fun v t ->
      let is_included =
        match filter.variable_inclusion with
        | None -> true
        | Some (vars, Exact) -> Variable.Set.mem v vars
        | _ -> assert false
      in
      if is_included then begin
        let src = add_var p graph v in
        let es = dot_of_t ~filter p graph src t in
        List.iter (fun (e,a) ->
            add_edge graph src e a)
          es
      end)
    p.trees;
  graph

let dot_string_of_program p filter =
  string_of_graph @@ graph_of_program p filter

let dot_of_program p filter =
  print_file "graph.dot" @@ graph_of_program p filter
