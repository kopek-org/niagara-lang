type typ =
  | Integer
  | Rational
  | Money
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

type comp = Eq

type context_refinement_item =
  | CCase of string
  | CFullType of string

type context_refinement = context_refinement_item list

type actor =
  | PlainActor of string
  | LabeledActor of string * string

type holder =
  | Pool of string * context_refinement option
  | Actor of actor

type named =
  | Name of string * context_refinement option
  | Holder of holder

type formula =
  | Literal of literal
  | Named of named
  | HolderExpr of holder
  | Binop of binop * formula * formula
  | Comp of comp * formula * formula
  | Total of formula
  | Instant of formula

type redistribution =
  | Part of formula
  | Flat of formula
  | Retrocession of formula * holder

type opposition =
  | NoOpposition
  | Opposable of formula * actor

type redistrib_with_dest = redistribution * (holder * opposition) option

type event_expr =
  | EventId of string
  | EventFormula of formula
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
  op_default_dest : (holder * opposition) option;
  op_context : context list;
  op_source : holder;
  op_guarded_redistrib : guarded_redistrib;
}

type advance_decl = {
  adv_label : string;
  adv_output : holder;
  adv_provider : actor;
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
  input_context : string list;
  input_type : typ;
}

type actor_decl = string

(* type section = { *)
(*   section_name : string; *)
(*   section_context : context list; *)
(*   section_guards : guard list; *)
(*   section_decl : declaration list; *)
(* } *)

type declaration =
  | DOperation of operation_decl
  | DAdvance of advance_decl
  | DEvent of event_decl
  | DConstant of const_decl
  | DContext of context_decl
  | DInput of input_decl
  | DActor of actor_decl
  (* | DSection of section *)
  | DDefault of holder * holder
  | DDeficit of holder * holder

type program = declaration list
