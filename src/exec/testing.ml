open Interpreter
open Dataflow
open Grammar
open Surface

let find_input (infos : VarInfo.collection) (name : string) (ctx : Context.Group.t) =
  let vars =
    Variable.Map.filter (fun _ info ->
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
          |  _ -> false)
      infos
  in
  match Variable.Map.cardinal vars with
  | 0 -> None
  | 1 -> Some (fst (Variable.Map.choose vars))
  | _ -> Errors.raise_error "(internal) Unable to distinguish input"

let convert_line (p : Equ.program) (input : string) (amount : string) =
  let (name, ctx) =
      ParserMain.parse_string ~entry:Parser.Incremental.raw_pool input
  in
  let var =
    find_input p.infos.Ast.var_info name
      (Compiler.Contextualize.projection_of_context_refinement
         p.infos.Ast.contexts ctx)
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

let test_stdin (p : Equ.program) (l : Equ.limits) =
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
  let outputs = Interpreter.Execution.compute_input_lines p l inputs in
  Interpreter.Printer.print_intepreter_outputs
    p (Format.formatter_of_out_channel stdout) outputs
