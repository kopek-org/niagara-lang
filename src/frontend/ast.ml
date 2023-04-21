type base_type =
  | Integer
  | Rational
  | Money

type typ =
  | Instant of base_type
  | Flow of base_type
  | Duration
  | Date

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

type flow_expr =
  | Pool of string
  | LabeledOutput of string * string
  | ContextPool of string * string list

type formula =
  | Literal of literal
  | ValueId of string
  | FlowExpr of flow_expr
  | Binop of binop * formula * formula

type redistribution =
  | Part of formula
  | Flat of formula

type redistrib_with_dest = redistribution * flow_expr option

type event_expr =
  | EventId of string
  | EventEq of formula * formula
  | EventConj of event_expr * event_expr
  | EventDisj of event_expr * event_expr

type guard =
  | Before of event_expr
  | After of event_expr
  | When of event_expr

type guarded_redistrib =
  | Guarded of guard * guarded_redistrib
  | Redist of redistrib_with_dest
  | Seq of guarded_redistrib list

type context =
  | Forall of string
  | Cases of string * string list

type operation_decl = {
  op_label : string;
  op_default_output : flow_expr option;
  op_context : context list;
  op_source : flow_expr option;
  op_guarded_redistrib : guarded_redistrib;
}

type advance_decl = {
  adv_label : string;
  adv_output : flow_expr;
  adv_provider : flow_expr;
  adv_amount : formula;
}

type event_decl = {
  event_name : string;
  event_expr : event_expr;
}

type const_decl = {
  const_name : string;
  const_value : literal;
}

type context_decl = {
  context_type_name : string;
  context_type_cases : string list;
}

type input_decl = {
  input_name : string;
  input_computable : bool;
  input_context : string list;
  input_type : base_type;
}

type output_decl = string

type section = {
  section_name : string;
  section_context : context list;
  section_guards : guard list;
  section_decl : declaration list;
}

and declaration =
  | Operation of operation_decl
  | Advance of advance_decl
  | Event of event_decl
  | Constant of const_decl
  | Context of context_decl
  | Input of input_decl
  | Output of output_decl
  | Section of section
  | Default of flow_expr * flow_expr
  | Deficit of flow_expr * flow_expr

type program = declaration list
