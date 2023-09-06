(*****************************************************************************)
(*                                                                           *)
(*  Copyright (c) 2023 OCamlPro SAS                                          *)
(*                                                                           *)
(* All rights reserved.                                                      *)
(* This source code is licensed under the GNU Affero General Public License  *)
(* version 3 found in the LICENSE.md file in the root directory of this      *)
(* source tree.                                                              *)
(*                                                                           *)
(*****************************************************************************)

type variable_context = {
  var_context_group : Context.Group.t;
  var_context_desc : Context.desc;
}

type actor_label = string option

type variable_kind =
  | ReceivingActor of actor_label      (* final cascade output *)
  | ProvidingActor of actor_label      (* for money injection *)
  | ParameterInput of variable_context (* any value not flowing through the cascade *)
  | PoolInput of variable_context      (* money to redistribute *)
  | Intermediary of variable_context   (* intermediary pool of money *)

type var_infos = {
  var_name : string;
  var_kind : variable_kind;
  var_type : ValueType.t;
}

type event_infos = {
  event_name : string;
}

type program_desc = {
  variables : var_infos Variable.Map.t;
  events : event_infos Variable.Map.t;
  contexts : Context.world;
}

let context_of_variable (p : ConditionLifting.program_with_threshold) (v : Variable.t) =
  match Variable.Map.find_opt v p.infos.var_shapes with
  | None -> Errors.raise_error "(internal) Cannot find variable %d context group" v
  | Some shape ->
    Context.shape_fold (fun res g ->
        match res with
        | Some _ -> Errors.raise_error "(internal) Unexpected compound shape"
        | None ->
          Some {
            var_context_group = g;
            var_context_desc = Context.group_desc p.infos.contexts g;
          })
      None shape
    |> Option.get

let description_from_program (p : ConditionLifting.program_with_threshold) =
  let events =
    Variable.Map.mapi (fun evt _expr ->
        let event_name =
          match Variable.Map.find_opt evt p.infos.var_info with
          | None -> Errors.raise_error "(internal) Cannot find event infos"
          | Some info -> info.Variable.var_name
        in
        { event_name })
    p.equations
  in
  let variables =
    Variable.Map.filter_map (fun v Variable.{ var_name } ->
        if Variable.Map.mem v events then None else
          let var_name, label_name =
            match String.split_on_char '$' var_name with
            | [] -> assert false
            | [vn] -> vn, None
            | vn::ln -> vn, Some (String.concat "$" ln)
          in
          let var_kind =
            match Variable.Map.find_opt v p.infos.inputs with
            | Some ReadOnly -> ParameterInput (context_of_variable p v)
            | Some Attributable -> PoolInput (context_of_variable p v)
            | None ->
              match Variable.Map.find_opt v p.infos.actors with
              | Some Upstream -> ProvidingActor label_name
              | Some Downstream -> ReceivingActor label_name
              | None -> Intermediary (context_of_variable p v)
          in
          let var_type =
            match Variable.Map.find_opt v p.infos.types with
            | None -> Errors.raise_error "(internal) Cannot find variable type"
            | Some t -> t
          in
          Some { var_name; var_kind; var_type; }
      )
      p.infos.var_info
  in
  {
    variables;
    events;
    contexts = p.infos.contexts;
  }

let print_event_desc fmt (evt, { event_name }) =
  Format.fprintf fmt "e%d: name: '%s'" evt event_name

let print_actor_label fmt (l : actor_label) =
  match l with
  | None -> Format.fprintf fmt "default label"
  | Some l -> Format.fprintf fmt "labeled '%s'" l

let print_var_context contexts fmt (ctx : variable_context) =
  Format.fprintf fmt "@[<v 2>Context:@ Group id: %a@ Group desc: @[<h>%a@]@]"
    Context.print_group_id ctx.var_context_group
    (Context.print_group_desc contexts) ctx.var_context_desc

let print_var_kind contexts fmt (kind : variable_kind) =
  match kind with
  | ReceivingActor l ->
    Format.fprintf fmt "Receiving actor, %a" print_actor_label l
  | ProvidingActor l ->
    Format.fprintf fmt "Providing actor, %a" print_actor_label l
  | ParameterInput ctx ->
    Format.fprintf fmt "Parameter input@ %a" (print_var_context contexts) ctx
  | PoolInput ctx ->
    Format.fprintf fmt "Pool input@ %a" (print_var_context contexts) ctx
  | Intermediary ctx ->
    Format.fprintf fmt "Intermediate pool@ %a" (print_var_context contexts) ctx

let print_variable_desc contexts fmt (var, desc) =
  Format.fprintf fmt "@[<v 2>v%d:@," var;
  Format.fprintf fmt "name: '%s'@," desc.var_name;
  Format.fprintf fmt "kind: @[<v>%a@]@," (print_var_kind contexts) desc.var_kind;
  Format.fprintf fmt "type: %a@]" FormatAst.print_type desc.var_type

let print_program_desc fmt (p : program_desc) =
  Format.fprintf fmt "@[<v 2>Program description:@ ";
  Format.fprintf fmt "@[<v 2>Contexts:@ %a@]@,"
    Context.print_world_desc p.contexts;
  Format.fprintf fmt "@[<v 2>Events:@ %a@]@,"
    (Format.pp_print_list print_event_desc) (Variable.Map.bindings p.events);
  Format.fprintf fmt "@[<v 2>Variables:@ %a@]@]"
    (Format.pp_print_list (print_variable_desc p.contexts))
       (Variable.Map.bindings p.variables)

let print_var_with_ctx (desc : program_desc) fmt (v : Variable.t) =
  let var_desc = Variable.Map.find v desc.variables in
  let print_with_ctx fmt name ctx =
    Format.fprintf fmt "%s(%a)" name (Context.print_group_desc desc.contexts) ctx
  in
  let print_with_label fmt name label =
    match label with
    | None -> Format.fprintf fmt "%s" name
    | Some label -> Format.fprintf fmt "%s$%s" name label
  in
  match var_desc.var_kind with
    | ParameterInput ctx
    | PoolInput ctx
    | Intermediary ctx ->
      print_with_ctx fmt var_desc.var_name ctx.var_context_desc
    | ReceivingActor l
    | ProvidingActor l ->
      print_with_label fmt var_desc.var_name l

let print_input_line (desc : program_desc) fmt (i, line : int * Interpreter.input_line) =
  Format.fprintf fmt "%d: %a += %a"
    i (print_var_with_ctx desc) line.input_variable
    FormatIr.print_literal line.input_value

let print_intepreter_inputs (desc : program_desc) fmt
    (lines : Interpreter.computation_inputs) =
  Format.fprintf fmt "@[<v>### INPUTS ###@,%a@]"
    (Format.pp_print_list (print_input_line desc)) (IntMap.bindings lines)

let print_event_switch (desc : program_desc) fmt (evt : Interpreter.event_switch) =
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
    (print_value_with_typ typ) tally.Interpreter.at_instant
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
    (print_tally typ) count.Interpreter.tally
    (print_repartition desc typ) count.repartition

let print_event_line (desc : program_desc) fmt (evt_swt, count) =
  Format.fprintf fmt "@[<v 2>++ %a:@ %a@]"
    (print_event_switch desc) evt_swt
    (Format.pp_print_list (print_count desc)) (Variable.Map.bindings count)

let print_output_line (desc : program_desc) fmt (i, line : int * Interpreter.output_line) =
  Format.fprintf fmt "%d: @[<v>%a@]"
    i
    (Format.pp_print_list (print_event_line desc)) (List.rev line)

let print_intepreter_outputs (desc : program_desc) fmt
    (lines : Interpreter.computation_outputs) =
  Format.fprintf fmt "@[<v>### OUTPUTS ###@,%a@]"
    (Format.pp_print_list (print_output_line desc)) (IntMap.bindings lines)
