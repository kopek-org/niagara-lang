%{
open Surface.Ast

let pos (start, stop) = Pos.Text.make ~start ~stop
%}

%token OPERATION QUOTEPART BONUS SUR ASSIETTE ANS MOIS ENTREE LBRA RBRA DEFAUT
%token CONTEXTUALISEE PAR TYPE ENTIER RATIONNEL ARGENT TOTAL COURANT
%token ACTEUR POUR EVENEMENT ET OU AVANT APRES QUAND CONTEXTE TOUT CONSTANTE
%token LPAR RPAR VERS ATTEINT PLUS MINUS MULT DIV EQ COLON EOF DEFICIT
%token AVANCE MONTANT COMMA RETROCESSION RESTE OPPOSABLE ENVERS VALEUR CALCULEE
%token OBSERVABLE // SECTION FIN
%token<R.t> FLOAT
%token<Z.t> INT MONEY
%token<string> LIDENT UIDENT LABEL
%token<Date.Date.t> DATE

%nonassoc LIDENT
%nonassoc LPAR

%left PLUS MINUS OU
%left MULT DIV ET
%nonassoc TOTAL COURANT OPPOSABLE

%on_error_reduce literal formula named actor

%start<source program> program
%start<string * context_refinement> raw_pool
%%

// Dispatch

operation:
| OPERATION op_label = LABEL op_default_dest = destinataire?
    op_context = op_context* op_source = source
    exprs = expression(simple_exprs)
{
  operation_decl
    ~loc:(pos $sloc)
    op_label
    ?default_dest:op_default_dest
    ~context:op_context
    ~source:op_source
    ~guarded_redistrib:exprs
}

comp_pool_decl:
| ASSIETTE CALCULEE name = LIDENT context = op_context* exprs = expression(value_def)
{{
  comp_pool_loc = pos $sloc;
  comp_pool_name = name;
  comp_pool_context = context;
  comp_pool_guarded_value = exprs;
}}

value_decl:
| VALEUR obs = boption(OBSERVABLE) name = LIDENT value = value_def
{{
  val_loc = pos $sloc;
  val_name = name;
  val_formula = value;
  val_observable = obs;
}}

value_def:
| COLON formula = formula { formula }

advance:
| AVANCE adv_label = LABEL SUR adv_output = holder PAR
    adv_provider = actor MONTANT adv_amount = formula
  {{
      adv_label;
      adv_output;
      adv_provider;
      adv_amount;
  }}

opposable:
| OPPOSABLE f = formula ENVERS t = actor PAR p = actor {
  HolderOpp {
      opp_value = f;
      opp_provider = p;
      opp_towards = t;
  }
}

simple_expr:
| QUOTEPART f = formula d = destinataire? {
  redistribution ~loc:f.formula_loc (Part f), d
}
| QUOTEPART RESTE d = destinataire? {
  let start, stop = $startpos, $endpos in
  redistribution ~loc:(pos (start, stop)) Default, d
}
| BONUS f = formula d = destinataire? {
  redistribution ~loc:f.formula_loc (Flat f), d
}
| RETROCESSION f = formula SUR h = holder d = destinataire?
  { let start, stop = $startpos(f), $endpos(h) in
    redistribution ~loc:(pos (start,stop)) (Retrocession (f, h)), d
  }

simple_exprs:
| es = simple_expr+ { List.map (fun (e, d) -> WithHolder (e, d)) es }

sub_expression(atom):
| es = atom { Atom es }
| LPAR es = expression(atom) RPAR { es }

expression(atom):
| es = atom { Atom es }
| es = branch_expr(atom) { Branches { befores = fst es; afters = snd es } }
| es = when_expr(atom)+ { Whens es }

branch_expr(atom):
| ae = after_expr(atom)+ { ([], ae) }
| be = before_expr(atom)+ ae = after_expr(atom)* { (be, ae) }

after_expr(atom):
| APRES g = event_expr se = sub_expression(atom) { (g, se) }

before_expr(atom):
| AVANT g = event_expr se = sub_expression(atom) { (g, se) }

when_expr(atom):
| QUAND g = event_expr se = sub_expression(atom) { (g, se) }

source:
| SUR p = pool { p }
| PAR a = actor { holder ~loc:(pos $sloc) (Actor a) }

// Formula

formula:
| fd = formula_desc { formula ~loc:(pos $sloc) fd }
| LPAR f = formula RPAR { f }

formula_desc:
| l = literal { Literal l }
| n = named { Named n }
| f1 = formula op = binop f2 = formula { Binop(op, f1, f2) }
| f = formula COURANT { Instant f }
| f = formula TOTAL { Total f }
| f = formula opp = opposable { Opposed (f, opp) }
;

%inline binop:
| PLUS { Add }
| MINUS { Sub }
| MULT { Mult }
| DIV { Div }

%inline comp:
| EQ { Eq }

named:
| n = LIDENT c = ioption(context_refinement)
    { let ctx = Option.fold c ~none:[] ~some:(fun c -> c) in
      named ~loc:(pos $sloc) (Name (n, ctx))
    }
| a = labeled_actor {
  let loc = pos $sloc in
  named ~loc (Holder (holder ~loc (Actor a)))
}
| p = pool { named ~loc:(pos $sloc) (Holder p) }

holder:
| a = actor { holder ~loc:(pos $sloc) (Actor a) }
| p = pool { p }

actor:
| a = actor_desc; {actor ~loc:(pos $sloc) a}
;

actor_desc:
| a = LIDENT { PlainActor a }
| la = labeled_actor_desc { la }
;

labeled_actor:
| a = labeled_actor_desc; {actor ~loc:(pos $sloc) a}
;

labeled_actor_desc:
| a = LIDENT LBRA l = LIDENT RBRA { LabeledActor(a, l) }
;

raw_pool0:
| p = LIDENT c = ioption(context_refinement)
    { let ctx = Option.fold c ~none:[] ~some:(fun c -> c) in
      (p, ctx)
    }

raw_pool:
| p = raw_pool0 EOF { p }

pool:
| ASSIETTE p = raw_pool0
    { let p, ctx = p in
      holder ~loc:(pos $sloc) (Pool(p, ctx))
    }

context_refinement:
| LPAR c = separated_nonempty_list(COMMA,context_refine_item) RPAR { c }

context_refine_item:
| crid = context_refinement_item_desc {
  context_refinement_item ~loc:(pos $sloc) crid
}
;

context_refinement_item_desc:
| c = UIDENT { CCase c }
| TOUT ct = UIDENT { CFullDomain ct }
;

literal:
| i = INT { Literal.LInteger i }
| r = FLOAT { Literal.LRational r }
| m = MONEY { Literal.LMoney m }
| d = duration { Literal.LDuration d }
| d = DATE { Literal.LDate d }

duration:
| d = duration_year { d }
| d = duration_month { d }

duration_year:
| i = INT ANS m = duration_month?
 { let y = Date.Duration.year (Z.to_int i) in
   match m with
   | None -> y
   | Some m -> Date.Duration.add y m
 }

duration_month:
  | i = INT MOIS { Date.Duration.month (Z.to_int i) }

// Flow and IO

input_decl:
| ENTREE input_name = LIDENT input_type = input_type input_context = loption(input_context_decl)
{
  input_decl
    ~loc:(pos $sloc)
    ~context:input_context
    ~typ:input_type
    ~kind:ReadOnly
    input_name
}
| ENTREE ASSIETTE input_name = LIDENT input_context = loption(input_context_decl)
{
  input_decl
    ~loc:(pos $sloc)
    ~context:input_context
    ~typ:TMoney
    ~kind:Attributable
    input_name
 }

input_context_decl:
| CONTEXTUALISEE PAR ctx = input_context_list { ctx }

input_context_list:
| cs = input_context { [cs] }
| css = nonempty_list(preceded(MINUS,input_context)) { css }

input_context:
| cs = nonempty_list(context) { cs }

input_type:
| TYPE t = base_type { t }

base_type:
| ENTIER { TInteger }
| RATIONNEL { TRational }
| ARGENT { TMoney }

actor_decl:
| ACTEUR id = LIDENT { actor_decl ~loc:(pos $sloc) id }

destinataire:
| VERS d = holder { d }

/* destinataire: */
/* | VERS d = holder { d, NoOpposition } */
/* | VERS d = holder o = opposition { d, o } */

/* opposition: */
/* | OPPOSABLE f = formula POUR a = actor { Opposable (f, a) } */

// Event

event_decl:
| EVENEMENT event_name = LIDENT ATTEINT QUAND event_expr = event_expr
 { event_decl ~loc:(pos $sloc) event_name event_expr }

event_expr:
| eed = event_expr_desc { event_expr ~loc:(pos $sloc) eed }
;

event_expr_desc:
| EVENEMENT id = LIDENT { EventId id }
| f1 = formula c = comp f2 = formula { EventComp(c, f1, f2) }
| e1 = event_expr ET e2 = event_expr { EventConj(e1, e2) }
| e1 = event_expr OU e2 = event_expr { EventDisj(e1, e2) }
;
// Context

context_decl:
| CONTEXTE context_type_name = UIDENT COLON context_type_cases = context_case+
 {{ context_type_name; context_type_cases }}

context_case:
| MINUS id = UIDENT { id }

context:
| tid = UIDENT LPAR cases = separated_nonempty_list(COMMA,UIDENT) RPAR { Cases(tid, cases) }
| TOUT id = UIDENT { Forall id }

op_context:
| POUR c = context { c }

// Program

constant_decl:
| CONSTANTE const_name = LIDENT COLON const_value = literal
 {{ const_name; const_value }}

default_decl:
| DEFAUT SUR src = pool VERS dst = holder { src, dst }

deficit_decl:
| DEFICIT SUR src = pool PAR def = holder { src, def }

/* section: */
/* | SECTION section_name = UIDENT section_context = context* */
/*   section_guards = event_guard* section_decl = toplevel_decl* FIN */
/*  {{ */
/*    section_name; */
/*    section_context; */
/*    section_guards; */
/*    section_decl; */
/*  }} */

toplevel_decl:
| o = operation { DHolderOperation o }
| e = event_decl { DHolderEvent e }
| p = comp_pool_decl { DHolderPool p }
| c = constant_decl { DConstant c }
| v = value_decl { DHolderValue v }
| c = context_decl { DContext c }
| i = input_decl { DInput i }
| o = actor_decl { DActor o }
| d = default_decl
  { let (default_source, default_dest) = d in
    DHolderDefault { default_source; default_dest }
  }
| d = deficit_decl
  { let (deficit_pool, deficit_provider) = d in
    DHolderDeficit { deficit_pool; deficit_provider } }
| a = advance { DHolderAdvance a }
/* | s = section { DSection s } */

program: d = toplevel_decl* EOF { Source d }
