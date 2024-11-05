open Execution

let print_event_switch fmt (changes : (string * bool) Variable.Map.t) =
  let open Format in
  if Variable.Map.is_empty changes then fprintf fmt "no events" else
    fprintf fmt "@[<hov>%a@]"
      (pp_print_list (fun fmt (_, (n, b)) ->
           fprintf fmt "%s event %s "
             (if b then "after" else "before")
             n))
      (Variable.Map.bindings changes)

let find_step_value (v : Variable.t) (step : output_step) =
  match Variable.Map.find_opt v step.step_valuations with
  | None -> Absent
  | Some v -> v

let print_repartition ?(default=false) fmt (dest, value) =
  if default then Format.fprintf fmt "default ";
  Format.fprintf fmt "%a -> %s"
    Value.human_print value
    dest

let print_default fmt rep = print_repartition ~default:true fmt rep

let rec print_item ?(close=true) fmt layout step (item : Results.top_item) =
  let open Format in
  let map_reps reps =
    List.filter_map (fun (dest, value) ->
        match find_step_value value step with
        | Absent -> None
        | Present v ->
          let name =
            match (Variable.Map.find dest layout : Results.top_item) with
            | Top item | Detail item -> item.display_name
            | Super item -> item.super_item.display_name
          in
          Some (name, v))
      (Variable.Map.bindings reps)
  in
  match item with
  | Top item | Detail item ->
    (match find_step_value item.at_step step with
     | Absent -> false
     | Present at_step ->
       fprintf fmt "@[<v 2>- %s%s { %a, %a }:"
         item.display_name
         (if item.provider then " (as provider)" else "")
         Value.human_print at_step
         (fun fmt o -> match o with
            | None -> pp_print_string fmt "%not_computed%"
            | Some v ->
              match find_step_value v step with
              | Absent -> pp_print_string fmt "%unknown%"
              | Present v -> Value.human_print fmt v)
         item.cumulated;
       let reps = map_reps item.reps in
       if List.length reps > 0 then
         fprintf fmt "@,%a"
           (pp_print_list print_repartition) reps;
       let defaults = map_reps item.defaults in
       if List.length defaults > 0 then
         fprintf fmt "@,%a"
           (pp_print_list print_default) defaults;
       if close then pp_close_box fmt ();
       true)
  | Super item ->
    if print_item ~close:false fmt layout step (Top item.super_item) then
      begin
        if Variable.Set.cardinal item.super_detail_items > 0 then
          fprintf fmt "@,%a"
            (pp_print_list ~pp_sep:(fun _ _ -> ()) (fun fmt item ->
                if print_item fmt layout step item then
                  pp_print_cut fmt ()))
            (Variable.Set.fold (fun v items ->
                 let i = Variable.Map.find v layout in
                 i::items)
                item.super_detail_items []);
        pp_close_box fmt ();
        true
      end
    else false

let print_step layout ~iter_vars fmt (step : output_step) =
  iter_vars layout (fun (item : Results.top_item) ->
      if print_item fmt layout step item then
        Format.pp_print_cut fmt ())

let print_event_line layout ~iter_vars ~changes fmt step =
  Format.fprintf fmt "@[<v 2>++ %a:@ %a@]@,"
    print_event_switch changes
    (print_step layout ~iter_vars) step

let event_flips (infos : VarInfo.collection) (past_state : bool Variable.Map.t)
    (new_state : bool Variable.Map.t) =
  let visible_name v b =
    match (Variable.Map.find v infos).origin with
    | Named name -> Some (name, b)
    | AnonEvent -> Some ("anon_event_" ^ string_of_int (Variable.uid v), b)
    | Peeking _ | RisingEvent _ -> None
    | _ -> assert false
  in
 Variable.Map.merge (fun v e1 e2 ->
      match e1,e2 with
      | None, Some true -> visible_name v true
      | Some pb, Some nb -> if pb <> nb then visible_name v nb else None
      | Some true, None -> visible_name v false
      | _ -> None)
   past_state new_state

let print_intepreter_outputs (p : Dataflow.Equ.program) fmt (lines : computation_outputs) =
  let var_infos = p.infos.Surface.Ast.var_info in
  let iter_vars = Results.iter_layout var_infos in
  let layout = Results.build_result_layout p.infos in
  let open Format in
  fprintf fmt "@[<v>### OUTPUTS ###@,";
  let _ =
    IntMap.fold (fun i line past_events ->
        fprintf fmt "%d: @[<v>" i;
        let changes =
          List.fold_left (fun past_events step ->
              let changes = event_flips var_infos past_events step.step_events in
              print_event_line layout ~iter_vars ~changes fmt step;
              step.step_events)
            past_events line
        in
        fprintf fmt "@.";
        changes)
      lines Variable.Map.empty
  in
  pp_print_flush fmt ()

