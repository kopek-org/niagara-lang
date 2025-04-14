open Execution

type raw = {
  id : int;
  name : string;
  context : string list; (* A convex characterization of the context.
                            Can be one element, or can be a convex subset. *)
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

(** Computes the largest convex domain from a list of domains. *)
let largest_domain
      (world : Context.world)
      (dl : Context.CaseSet.t Context.DomainMap.t list) : string list =
  let _, l =
    List.fold_left
      (fun ((size, _l) as acc) dm ->
        let (size', _l') as acc' =
          Context.DomainMap.fold
            (fun _ s (size', l') ->
              (size' + Context.CaseSet.cardinal s),
              Context.CaseSet.elements s @ l'
            )
            dm
            (0, [])
        in
        if size >= size' then acc else acc'
      )
      (0, [])
      dl
  in
  List.map (Context.case_name world) l

let of_interpreter_input (pinfos : ProgramInfo.t) (id : int) (i : input_line) : raw =
  let name =
    match VarInfo.get_name pinfos.var_info i.input_variable with
    | Some n -> n
    | None -> invalid_arg "Input.of_interpreter_input (name)"
  in
  let value = Format.asprintf "%a" Literal.print i.input_value in
  let context =
    (* We will take the largest convex sub-set. *)
    match Variable.Map.find_opt i.input_variable pinfos.var_info with
    | None -> invalid_arg "Input.of_interpreter_input (value)"
    | Some { origin = ContextSpecialized {context; _ }; _ } ->
       let desc = Context.group_desc pinfos.contexts context in
       largest_domain pinfos.contexts desc
    | Some _ -> []
  in
  {id; name; context; value}

let of_interpreter_inputs (pinfos : ProgramInfo.t) (ci : computation_inputs) : raw list =
  InputLineMap.fold
    (fun id i acc -> of_interpreter_input pinfos id i :: acc)
    ci
    []
  |> List.rev
