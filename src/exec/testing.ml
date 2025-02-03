open Interpreter
open Dataflow
open Grammar

let find_var (infos : VarInfo.collection) info_filter  =
  let vars =
    Variable.Map.filter (fun _ info -> info_filter info)
      infos
  in
  match Variable.Map.cardinal vars with
  | 0 -> None
  | 1 -> Some (fst (Variable.Map.choose vars))
  | _ -> None

let find_input (infos : VarInfo.collection) (name : string) (ctx : Context.Group.t) =
  let filter_info info =
    if not (VarInfo.is_input info) then false else
      match info.origin with
      | Named n -> String.equal name n
      | ContextSpecialized { origin; context } ->
        let n =
              match (Variable.Map.find origin infos).origin with
              | Named n -> n
              | _ -> assert false
        in
        String.equal n name && Context.Group.includes context ctx
      |  _ -> false
  in
  find_var infos filter_info

let find_partner (infos : VarInfo.collection) (name : string) =
  let filter_info info =
    match info.VarInfo.kind with
    | ReceivingPartner ->
      (match info.origin with
      | Named n -> String.equal name n
      |  _ -> false)
    | _ -> false
  in
  find_var infos filter_info

let convert_line (p : Equ.program) (input : string) (amount : string) =
  let (name, ctx) =
      ParserMain.parse_string ~entry:Parser.Incremental.raw_pool input
  in
  let var =
    find_input p.infos.var_info name
      (Compiler.Contextualize.projection_of_context_refinement
         p.infos.contexts ctx)
  in
  let amount =
    match Lexer.parse_money_amount_opt amount with
    | Some m -> Literal.LMoney m
    | None ->
      try Literal.LInteger (Z.of_string amount) with
      | Invalid_argument _ ->
        Errors.raise_error "%s is not a valid amount" amount
  in
  Option.map (fun v -> v, amount) var

let test_stdin (p : Equ.program) (l : Equ.limits) (for_partner : string option) (for_all : bool) =
  Format.printf "Awaiting inputs:@.";
  let raw_inputs = Testlex.parse stdin in
  let inputs =
    IntMap.mapi (fun i (input, amount) ->
      match convert_line p input amount with
        | Some (input_variable, input_value) ->
          Execution.{ input_variable; input_value }
        | None -> Errors.raise_error "Unable to find variable for \
                                      input %d. Make sure you state \
                                      the right context" i)
      raw_inputs
  in
  let norm_mode =
    if for_all then Results.SquashAllButPartners else
      match for_partner with
      | None -> Results.Canonical
      | Some s ->
        match find_partner p.infos.var_info s with
        | None -> Errors.raise_error "Unable to find partner %s" s
        | Some v -> Results.PartnerView v
  in
  let outputs = Interpreter.Execution.compute_input_lines p l inputs in
  Interpreter.Printer.print_intepreter_outputs
    p (Format.formatter_of_out_channel stdout) norm_mode outputs
