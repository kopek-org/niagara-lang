open Execution

type raw = {
  id : int;
  name : string;
  context : string list;
  value : string;
}

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

let to_interpreter_input (pinfos : ProgramInfo.t) (i : raw) : input_line =
  let ctx =
    List.map (fun c ->
        Surface.Ast.{ cri_desc = CCase c; cri_loc = Pos.dummy })
      i.context
  in
  let var =
    match
      find_input pinfos.var_info i.name
        (Compiler.Contextualize.projection_of_context_refinement
           pinfos.contexts ctx)
    with
    | None ->
      Report.raise_error
        "Unable to find variable for input %d. Make sure you state the \
         right context" i.id
    | Some var -> var
  in
  let amount =
    match Grammar.Lexer.parse_money_amount_opt i.value with
    | Some m -> Literal.LMoney m
    | None ->
      try Literal.LInteger (Z.of_string i.value) with
      | Invalid_argument _ ->
        Report.raise_error "%s is not a valid amount" i.value
  in
  { input_variable = var; input_value = amount }

let to_interpreter_inputs (pinfos : ProgramInfo.t) (is : raw list) : computation_inputs =
  List.fold_left (fun ci i ->
      InputLineMap.add i.id (to_interpreter_input pinfos i) ci)
    Execution.InputLineMap.empty
    is
