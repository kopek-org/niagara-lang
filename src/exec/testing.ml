open Interpreter
open Dataflow

let find_var (infos : VarInfo.collection) info_filter  =
  let vars =
    Variable.Map.filter (fun _ info -> info_filter info)
      infos
  in
  match Variable.Map.cardinal vars with
  | 0 -> None
  | 1 -> Some (fst (Variable.Map.choose vars))
  | _ -> None

let find_partner (infos : VarInfo.collection) (name : string) =
  let filter_info info =
    match info.VarInfo.kind with
    | Partner _ ->
      (match info.origin with
      | Named n -> String.equal name n
      |  _ -> false)
    | _ -> false
  in
  find_var infos filter_info

let test_stdin (p : Equ.program) (l : Equ.limits) (for_partner : string option) (for_all : bool) =
  Format.printf "Awaiting inputs:@.";
  let raw_init, raw_inputs = Testlex.parse stdin in
  let init = Interpreter.Initialization.of_raw p.infos raw_init in
  let inputs = Interpreter.Input.of_raw p.infos raw_inputs in
  let lines =
    IntMap.fold (fun i _ is -> IntMap.add i Results.AllSteps is) inputs IntMap.empty
  in
  let norm_mode =
    if for_all then Results.SquashAllButPartners else
      match for_partner with
      | None -> Results.Canonical
      | Some s ->
        match find_partner p.infos.var_info s with
        | None -> Report.raise_error "Unable to find partner %s" s
        | Some for_partner ->
          let relevancy_check =
            let rs =
              match Variable.Map.find_opt for_partner p.infos.relevance_sets with
              | None -> Report.raise_error "No relevant set for partner %s" s
              | Some rs -> rs
            in
            fun v -> Variable.Set.mem v rs.relevant_vars
          in
          Results.Explain {
            for_partner; lines;
            in_out_details = true;
            partner_display = true;
            relevancy_check;
          }
  in
  let outputs = Interpreter.Execution.compute_input_lines p l init inputs in
  Interpreter.Printer.print_intepreter_outputs
    p (Format.formatter_of_out_channel stdout) norm_mode outputs
