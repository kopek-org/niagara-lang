type source = private SRC
type contextualized = private CTX


type literal =
  | LitInt of int
  | LitRational of float
  | LitMoney of int
  | LitDuration of CalendarLib.Date.Period.t
  | LitDate of CalendarLib.Date.t

type binop =
  | Add
  | Sub
  | Mult
  | Div

type comp = Eq

type context_refinement_item = {
  cri_loc : Pos.t;
  cri_desc : context_refinement_item_desc;
}

and context_refinement_item_desc =
  | CCase of string
  | CFullDomain of string

type context_refinement = context_refinement_item list

type actor = {
  actor_loc : Pos.t;
  actor_desc : actor_desc;
}

and actor_desc =
  | PlainActor of string
  | LabeledActor of string * string

type holder = {
  holder_loc : Pos.t;
  holder_desc : holder_desc;
}

and holder_desc =
  | Pool of string * context_refinement
  | Actor of actor

type named = {
  named_loc : Pos.t;
  named_desc : named_desc;
}

and named_desc =
  | Name of string * context_refinement
  | Holder of holder

type contextualized_variable = Variable.t * Context.Group.t

type 'a formula = {
  formula_loc : Pos.t;
  formula_desc : 'a formula_desc;
}

and _ formula_desc =
  | Literal of literal
  | Named : named -> source formula_desc
  | Variable : contextualized_variable -> contextualized formula_desc
  | Binop : binop * 'a formula * 'a formula -> 'a formula_desc
  | Total : 'a formula -> 'a formula_desc
  | Instant : 'a formula -> 'a formula_desc

type 'a redistribution = {
  redistribution_loc : Pos.t;
  redistribution_desc : 'a redistribution_desc;
}

and _ redistribution_desc =
  | Part : 'a formula -> 'a redistribution_desc
  | Flat : 'a formula -> 'a redistribution_desc
  | Retrocession : source formula * holder -> source redistribution_desc

(* type _ opposition = *)
(*   | NoOpposition : 'a opposition *)
(*   | Opposable : 'a formula * actor *)

type _ redistrib_with_dest =
  | WithHolder : source redistribution * holder option
      -> source redistrib_with_dest
  | WithVar : contextualized redistribution * contextualized_variable option
      -> contextualized redistrib_with_dest

(* type redistrib_with_dest = redistribution * (holder * opposition) option *)

type 'a event_expr = {
  event_expr_loc : Pos.t;
  event_expr_desc : 'a event_expr_desc;
}

and _ event_expr_desc =
  | EventId : string -> source event_expr_desc
  | EventVar : Variable.t -> contextualized event_expr_desc
  | EventComp : comp * 'a formula * 'a formula -> 'a event_expr_desc
  | EventConj : 'a event_expr * 'a event_expr -> 'a event_expr_desc
  | EventDisj : 'a event_expr * 'a event_expr -> 'a event_expr_desc

type 'a conditional_redistrib =
  'a event_expr * 'a guarded_redistrib

and 'a guarded_redistrib =
  | Whens of 'a conditional_redistrib list
  | Branches of {
      befores : 'a conditional_redistrib list;
      afters : 'a conditional_redistrib list;
    }
  | Redists of 'a redistrib_with_dest list

type context =
  | Forall of string
  | Cases of string * string list

type operation_decl = {
  op_loc : Pos.t;
  op_label : string;
  op_default_dest : holder option;
  (* op_default_dest : (holder * opposition) option; *)
  op_context : context list;
  op_source : holder;
  op_guarded_redistrib : source guarded_redistrib;
}

type ctx_operation_decl = {
  ctx_op_label : string;
  ctx_op_default_dest : contextualized_variable option;
  (* op_default_dest : (holder * opposition) option; *)
  ctx_op_source : contextualized_variable;
  ctx_op_guarded_redistrib : contextualized guarded_redistrib;
}

type advance_decl = {
  adv_label : string;
  adv_output : holder;
  adv_provider : actor;
  adv_amount : source formula;
}

type ctx_advance_decl = {
  ctx_adv_label : string;
  ctx_adv_output : contextualized_variable;
  ctx_adv_provider : contextualized_variable;
  ctx_adv_amount : contextualized formula;
}

type event_decl = {
  event_loc : Pos.t;
  event_name : string;
  event_expr : source event_expr;
}

type ctx_event_decl = {
  ctx_event_var : Variable.t;
  ctx_event_expr : contextualized event_expr;
}

type const_decl = {
  const_name : string;
  const_value : literal;
}

type context_decl = {
  context_type_name : string;
  context_type_cases : string list;
}

type input_kind = ReadOnly | Attributable

type input_decl = {
  input_loc : Pos.t;
  input_name : string;
  input_context : context list list;
  input_type : ValueType.t;
  input_kind : input_kind;
}

type actor_decl = {
  actor_decl_loc : Pos.t;
  actor_decl_desc : string
}

type stream_way = Upstream | Downstream

(* type section = { *)
(*   section_name : string; *)
(*   section_context : context list; *)
(*   section_guards : guard list; *)
(*   section_decl : declaration list; *)
(* } *)

type default_decl = {
  default_source : holder;
  default_dest : holder;
}

type ctx_default_decl = {
  ctx_default_source : contextualized_variable;
  ctx_default_dest : contextualized_variable;
}

type deficit_decl = {
  deficit_pool : holder;
  deficit_provider : holder;
}

type ctx_deficit_decl = {
  ctx_deficit_pool : contextualized_variable;
  ctx_deficit_provider : contextualized_variable;
}

type _ declaration =
  | DHolderOperation : operation_decl -> source declaration
  | DVarOperation : ctx_operation_decl -> contextualized declaration
  | DHolderEvent : event_decl -> source declaration
  | DVarEvent : ctx_event_decl -> contextualized declaration
  | DConstant : const_decl -> source declaration
  | DContext : context_decl -> source declaration
  | DInput : input_decl -> source declaration
  | DActor : actor_decl -> source declaration
  | DHolderAdvance : advance_decl -> source declaration
  | DVarAdvance : ctx_advance_decl -> contextualized declaration
  | DHolderDefault : default_decl -> source declaration
  | DHolderDeficit : deficit_decl -> source declaration
  | DVarDefault : ctx_default_decl -> contextualized declaration
  | DVarDeficit : ctx_deficit_decl -> contextualized declaration
  (* | DSection of section *)

type program_infos = {
  var_info : Variable.info Variable.Map.t;
  var_shapes : Context.shape Variable.Map.t;
  contexts : Context.world;
  inputs : input_kind Variable.Map.t;
  actors : stream_way Variable.Map.t;
  types : ValueType.t Variable.Map.t;
  constants : literal Variable.Map.t;
}

type _ program =
  | Source : source declaration list -> source program
  | Contextualized : program_infos * contextualized declaration list -> contextualized program


let actor ?(loc = Pos.dummy) desc = {
  actor_loc = loc;
  actor_desc = desc;
}

let holder ?(loc = Pos.dummy) desc = {
  holder_loc = loc;
  holder_desc = desc;
}

let named ?(loc = Pos.dummy) desc = {
  named_loc = loc;
  named_desc = desc;
}

let formula ?(loc = Pos.dummy) desc = {
  formula_loc = loc;
  formula_desc = desc;
}

let redistribution ?(loc = Pos.dummy) desc = {
  redistribution_loc = loc;
  redistribution_desc = desc;
}

let event_expr ?(loc = Pos.dummy) desc = {
  event_expr_loc = loc;
  event_expr_desc = desc;
}

let actor_decl ?(loc = Pos.dummy) desc = {
  actor_decl_loc = loc;
  actor_decl_desc = desc;
}

let input_decl ?(loc = Pos.dummy) ~kind ~context ~typ name = {
  input_loc = loc;
  input_name = name;
  input_context = context;
  input_type = typ;
  input_kind = kind;
}

let event_decl ?(loc = Pos.dummy) name expr = {
  event_loc = loc;
  event_name = name;
  event_expr = expr;
}

let context_refinement_item ?(loc = Pos.dummy) desc = {
  cri_loc = loc;
  cri_desc = desc;
}

let operation_decl 
  ?(loc = Pos.dummy) 
  ?default_dest 
  ?(context = [])
  ~source 
  ~guarded_redistrib
  label = {
  op_loc = loc;
  op_label = label;
  op_default_dest = default_dest;
  op_context = context;
  op_source = source;
  op_guarded_redistrib = guarded_redistrib;
}
