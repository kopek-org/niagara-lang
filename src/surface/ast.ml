(* Source program AST.

   It uses GADT to factorize two versions:
   - the textual source (parsed)
   - the same structure as above, with name resolution and context
   constraints
*)

(* Dummy types for GADT distinction *)
type source = private SRC
type contextualized = private CTX

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
  | Literal of Literal.t
  | Named : named -> source formula_desc
  | Variable : contextualized_variable -> contextualized formula_desc
  | Binop : binop * 'a formula * 'a formula -> 'a formula_desc
  | Total : 'a formula -> 'a formula_desc
  | Instant : 'a formula -> 'a formula_desc
  | Opposed : 'a formula * 'a opposable -> 'a formula_desc

and _ opposable =
  | HolderOpp : {
      opp_towards : actor;
      opp_provider : actor;
      opp_value : source formula;
    } -> source opposable
  | VarOpp : {
      opp_towards : Variable.t;
      opp_provider : Variable.t;
      opp_value : contextualized formula;
    } -> contextualized opposable

type 'a redistribution = {
  redistribution_loc : Pos.t;
  redistribution_desc : 'a redistribution_desc;
}

and _ redistribution_desc =
  | Part : 'a formula * bool -> 'a redistribution_desc
  | Flat : 'a formula -> 'a redistribution_desc
  | Retrocession : source formula * holder -> source redistribution_desc
  | Default

type _ redistrib_with_dest =
  | WithHolder : source redistribution * holder option
      -> source redistrib_with_dest
  | WithVar : contextualized redistribution * contextualized_variable option
      -> contextualized redistrib_with_dest

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

type ('pass, 'leaf) conditional_redistrib =
  'pass event_expr * ('pass, 'leaf) guarded_redistrib

and ('pass, 'leaf) guarded_redistrib =
  | Whens of ('pass, 'leaf) conditional_redistrib list
  | Branches of {
      befores : ('pass, 'leaf) conditional_redistrib list;
      afters : ('pass, 'leaf) conditional_redistrib list;
    }
  | Atom of 'leaf

type context =
  | Forall of string
  | Cases of string * string list

type operation_decl = {
  op_loc : Pos.t;
  op_label : string;
  op_default_dest : holder option;
  op_context : context list;
  op_source : holder;
  op_guarded_redistrib :
    (source, source redistrib_with_dest list) guarded_redistrib;
}

type ctx_operation_decl = {
  ctx_op_label : string;
  ctx_op_default_dest : contextualized_variable option;
  ctx_op_source : contextualized_variable;
  ctx_op_guarded_redistrib :
    (contextualized, contextualized redistrib_with_dest list) guarded_redistrib;
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
  const_value : Literal.t;
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

type comp_pool_decl = {
  comp_pool_loc : Pos.t;
  comp_pool_name : string;
  comp_pool_context : context list;
  comp_pool_guarded_value : (source, source formula) guarded_redistrib;
}

type ctx_comp_pool_decl = {
  ctx_comp_pool_var : contextualized_variable;
  ctx_comp_pool_guarded_value : (contextualized, contextualized formula) guarded_redistrib;
}

type val_decl = {
  val_loc : Pos.t;
  val_name : string;
  val_formula : source formula;
  val_observable : bool;
}

type ctx_val_decl = {
  ctx_val_var : contextualized_variable;
  ctx_val_formula : contextualized formula;
  ctx_val_observable : bool;
  ctx_val_linear : bool;
}

type _ declaration =
  | DHolderOperation : operation_decl -> source declaration
  | DVarOperation : ctx_operation_decl -> contextualized declaration
  | DHolderEvent : event_decl -> source declaration
  | DVarEvent : ctx_event_decl -> contextualized declaration
  | DHolderPool : comp_pool_decl -> source declaration
  | DVarPool : ctx_comp_pool_decl -> contextualized declaration
  | DConstant : const_decl -> source declaration
  | DHolderValue : val_decl -> source declaration
  | DVarValue : ctx_val_decl -> contextualized declaration
  | DContext : context_decl -> source declaration
  | DInput : input_decl -> source declaration
  | DActor : actor_decl -> source declaration
  | DHolderAdvance : advance_decl -> source declaration
  | DHolderDefault : default_decl -> source declaration
  | DHolderDeficit : deficit_decl -> source declaration
  | DVarDefault : ctx_default_decl -> contextualized declaration
  | DVarDeficit : ctx_deficit_decl -> contextualized declaration

type _ program =
  | Source : source declaration list -> source program
  | Contextualized : ProgramInfo.t * contextualized declaration list -> contextualized program


(* constructors with locations *)

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
