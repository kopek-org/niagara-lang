
type label_or_ctx =
  | Bare
  | Label of string
  | Ctx of string list

type raw = {
  name : string;
  spec : label_or_ctx;
  for_opp : string option;
  value : string;
}

type t =
  | Zeros
  | FromValues of Literal.t Variable.Map.t

let print_raw_var fmt (r : raw) =
  let open Format in
  match r.spec with
  | Bare -> fprintf fmt "%s" r.name
  | Label l -> fprintf fmt "%s[%s]" r.name l
  | Ctx cases ->
    fprintf fmt "%s(%a)" r.name
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")
         pp_print_string)
      cases

let init_of_raw (pinfos : ProgramInfo.t) (r : raw) : Variable.t * Literal.t =
  let rec matching_spec r info =
    match info.VarInfo.origin with
    | OpposingVariant { target; origin; _ } ->
      (match r.for_opp with
      | None -> false
      | Some p ->
        let c =
          matching_spec
            { r with for_opp = None }
            (Variable.Map.find origin pinfos.var_info)
        in
        c && String.equal p (VarInfo.get_any_name pinfos.var_info target))
    | Cumulative v when r.for_opp = None ->
      matching_spec r (Variable.Map.find v pinfos.var_info)
    | Named n when r.for_opp = None -> String.equal n r.name
    | ContextSpecialized { origin; context } when r.for_opp = None ->
      if matching_spec r (Variable.Map.find origin pinfos.var_info) then
        match r.spec with
         | Bare -> true
         | Ctx cases ->
           let ctx =
             List.map (fun c ->
                 Surface.Ast.{ cri_desc = CCase c; cri_loc = Pos.dummy })
               cases
             |> Compiler.Contextualize.projection_of_context_refinement
               pinfos.contexts
           in
           Context.Group.includes context ctx
         | Label _ -> false
      else false
    | LabelOfPartner { partner; label } when r.for_opp = None ->
      if matching_spec r (Variable.Map.find partner pinfos.var_info) then
        match r.spec with
         | Bare | Ctx _ -> false
         | Label l -> String.equal l label
      else false
    | _ -> false
  in
  let filter_var_info v info =
    Variable.Set.mem v pinfos.init_requirements.initializable_values
    && matching_spec r info
  in
  let vars = Variable.Map.filter filter_var_info pinfos.var_info in
  match Variable.Map.bindings vars with
  | [] ->
    Report.raise_error
      "Unable to find variable '%a'." print_raw_var r
  | _::_::_ ->
    Report.raise_error
      "Unable to pinpoint exactly one variable '%a'." print_raw_var r
  | [v, _] ->
    let amount =
      match Grammar.Lexer.parse_money_amount_opt r.value with
      | Some m -> Literal.LMoney m
      | None ->
        try Literal.LInteger (Z.of_string r.value) with
        | Invalid_argument _ ->
          Report.raise_error "%s is not a valid amount" r.value
    in
    v, amount

let of_raw (pinfos : ProgramInfo.t) (raws : raw list) =
  let inits =
    List.fold_left (fun inits raw ->
        let v, amount = init_of_raw pinfos raw in
        Variable.Map.update v (function
            | None -> Some amount
            | Some _ ->
              Report.raise_error "Multiple initialization of variable '%a'."
                print_raw_var raw)
          inits)
      Variable.Map.empty
      raws
  in
  if Variable.Map.is_empty inits then Zeros else
    let () =
      Variable.Set.iter (fun v ->
          if not (Variable.Map.mem v inits) then
            Report.raise_error "Missing mandatory value for %s at init."
              (VarInfo.get_any_name pinfos.var_info v))
        pinfos.init_requirements.mandatory_values
    in
    FromValues inits

let init_to_raw (pinfos : ProgramInfo.t) (v : Variable.t) (l : Literal.t) : raw =
  let rec v_def spec for_opp v =
    match (Variable.Map.find v pinfos.var_info).origin with
    | Named n -> n, spec, for_opp
    | Cumulative v -> v_def spec for_opp v
    | OpposingVariant { target; origin; _ } ->
      v_def spec (Some (VarInfo.get_any_name pinfos.var_info target)) origin
    | LabelOfPartner { partner; label } -> v_def (Label label) for_opp partner
    | ContextSpecialized { origin; context } ->
      let context =
        let desc = Context.group_desc pinfos.contexts context in
        Input.largest_domain pinfos.contexts desc
      in
      v_def (Ctx context) for_opp origin
    | _ -> Report.raise_error "Invalid variable for init %s" (VarInfo.get_any_name pinfos.var_info v)
  in
  let name, spec, for_opp = v_def Bare None v in
  let value = Format.asprintf "%a" Literal.print l in
  { name; spec; for_opp; value }

let to_raw (pinfos : ProgramInfo.t) (inputs : t) : raw list =
  match inputs with
  | Zeros -> []
  | FromValues vals ->
    Variable.Map.fold (fun v l raws ->
        init_to_raw pinfos v l :: raws)
      vals
      []
