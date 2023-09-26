open Internal
open Interface
open Execution

let print_var_with_ctx (desc : program_desc) fmt (v : Variable.t) =
  let var_desc = Variable.Map.find v desc.variables in
  let print_with_ctx fmt name ctx =
    Format.fprintf fmt "%s(%a)" name (Context.print_group_desc desc.contexts) ctx
  in
  let print_with_label ~providing fmt name label =
    begin match label with
    | None -> Format.fprintf fmt "%s" name
    | Some label ->
      Format.fprintf fmt "%s$%s" name label
    end;
    if providing then Format.pp_print_string fmt " (as provider)"
  in
  match var_desc.var_kind with
    | ParameterInput ctx
    | PoolInput ctx
    | Intermediary ctx ->
      print_with_ctx fmt var_desc.var_name ctx.var_context_desc
    | ReceivingActor l ->
      print_with_label ~providing:false fmt var_desc.var_name l
    | ProvidingActor l ->
      print_with_label ~providing:true fmt var_desc.var_name l

let print_input_line (desc : program_desc) fmt (i, line : int * input_line) =
  Format.fprintf fmt "%d: %a += %a"
    i (print_var_with_ctx desc) line.input_variable
    FormatIr.print_literal line.input_value

let print_intepreter_inputs (desc : program_desc) fmt
    (lines : computation_inputs) =
  Format.fprintf fmt "@[<v>### INPUTS ###@,%a@]"
    (Format.pp_print_list (print_input_line desc)) (IntMap.bindings lines)

let print_event_switch (desc : program_desc) fmt (evt : event_switch) =
  match evt with
  | NoEvent -> Format.fprintf fmt "no events"
  | SwitchTo { event; value } ->
    let { event_name } = Variable.Map.find event desc.events in
    Format.fprintf fmt "switch %s event '%s'"
      (if value then "after" else "before")
      event_name

let print_value_with_typ (typ : ValueType.t) fmt value =
  (* TODO unhack *)
  match typ with
  | ValueType.TInteger -> FormatIr.print_literal fmt (Ir.LInteger value)
  | ValueType.TMoney -> FormatIr.print_literal fmt (Ir.LMoney value)
  | _ -> assert false

let print_tally (typ : ValueType.t) fmt tally =
  Format.fprintf fmt "{ %a, %a }"
    (print_value_with_typ typ) tally.at_instant
    (print_value_with_typ typ) tally.total

let print_repartition (desc : program_desc) (typ : ValueType.t) fmt rep =
  let rep = Variable.Map.bindings rep in
  Format.pp_print_list (fun fmt (var, value) ->
      Format.fprintf fmt "%a -> %a"
        (print_value_with_typ typ) value
        (print_var_with_ctx desc) var)
    fmt
    rep

let print_count (desc : program_desc) fmt (var, count) =
  let typ = (Variable.Map.find var desc.variables).var_type in
  Format.fprintf fmt "@[<v 2>- %a %a:@ %a@]"
    (print_var_with_ctx desc) var
    (print_tally typ) count.tally
    (print_repartition desc typ) count.repartition

let print_event_line (desc : program_desc) fmt (evt_swt, count) =
  let count =
    Variable.Map.bindings count
    |> List.filter (fun (_, c) -> c.tally.at_instant <> 0)
    |> List.sort (fun (v1, c1) (v2, c2) ->
        if Variable.Map.mem v1 c2.repartition then 1
        else if Variable.Map.mem v2 c1.repartition then -1
        else
          Interface.compare_kind
            (Variable.Map.find v1 desc.variables).var_kind
            (Variable.Map.find v2 desc.variables).var_kind)
  in
  Format.fprintf fmt "@[<v 2>++ %a:@ %a@]"
    (print_event_switch desc) evt_swt
    (Format.pp_print_list (print_count desc)) count

let print_output_line (desc : program_desc) fmt (i, line : int * output_line) =
  Format.fprintf fmt "%d: @[<v>%a@]"
    i
    (Format.pp_print_list (print_event_line desc)) (List.rev line)

let print_intepreter_outputs (desc : program_desc) fmt
    (lines : computation_outputs) =
  Format.fprintf fmt "@[<v>### OUTPUTS ###@,%a@]"
    (Format.pp_print_list (print_output_line desc)) (IntMap.bindings lines)
