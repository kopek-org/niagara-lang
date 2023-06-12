%{
open CalendarLib
open Ast
%}

%token OPERATION QUOTEPART BONUS SUR ASSIETTE ANS MOIS ENTREE LBRA RBRA DEFAUT
%token CONTEXTUALISEE PAR TYPE ENTIER RATIONNEL ARGENT TOTAL COURANT
%token ACTEUR POUR EVENEMENT ET OU AVANT APRES QUAND CONTEXTE TOUT CONSTANTE
%token LPAR RPAR VERS ATTEINT PLUS MINUS MULT DIV EQ COLON EOF DEFICIT
%token AVANCE MONTANT COMMA RETROCESSION OPPOSABLE // SECTION FIN
%token<float> FLOAT
%token<int> INT MONEY
%token<string> LIDENT UIDENT LABEL
%token<CalendarLib.Date.t> DATE

%nonassoc LIDENT
%nonassoc LPAR

%nonassoc TOTAL COURANT
%nonassoc EQ
%left PLUS MINUS OU
%left MULT DIV ET

%on_error_reduce literal formula event_guard named actor

%start<Ast.program> program

%type<Ast.guarded_redistrib> expression
%%

// Dispatch

operation:
| OPERATION op_label = LABEL op_default_dest = destinataire?
    op_context = context* op_source = source
    exprs = expression_group
 {{
   op_label;
   op_default_dest;
   op_context;
   op_source;
   op_guarded_redistrib = Seq exprs;
  }}

advance:
| AVANCE adv_label = LABEL SUR adv_output = holder PAR
    adv_provider = actor MONTANT adv_amount = formula
  {{
      adv_label;
      adv_output;
      adv_provider;
      adv_amount;
  }}

simple_expr:
| QUOTEPART f = formula d = destinataire? { Part f, d }
| BONUS f = formula d = destinataire? { Flat f, d }
| RETROCESSION f = formula SUR h = holder d = destinataire?
  { Retrocession (f, h), d}

expression_group:
| es = simple_expr+ { List.map (fun e -> Redist e) es }
| es = guarded_expr+ { es }

expression:
| es = simple_expr+
  { match es with [e] -> Redist e | _ -> Seq (List.map (fun e -> Redist e) es) }
| e = guarded_expr { e }
| LPAR es = expression_group RPAR { Seq es }

guarded_expr:
| g = event_guard ge = expression { Guarded (g, ge) }

source:
| SUR p = pool { p }
| PAR a = actor { Actor a }

// Formula

formula:
| l = literal { Literal l }
| n = named { Named n }
| f1 = formula op = binop f2 = formula { Binop(op, f1, f2) }
| f1 = formula op = comp f2 = formula { Comp(op, f1, f2) }
| f = formula COURANT { Instant f }
| f = formula TOTAL { Total f }
| LPAR f = formula RPAR { f }

%inline binop:
| PLUS { Add }
| MINUS { Sub }
| MULT { Mult }
| DIV { Div }

%inline comp:
| EQ { Eq }

named:
| n = LIDENT c = ioption(context_refinement) { Name(n, c) }
| a = labeled_actor { Holder (Actor a) }
| p = pool { Holder p }

holder:
| a = actor { Actor a }
| p = pool { p }

actor:
| a = LIDENT { PlainActor a }
| la = labeled_actor { la }

labeled_actor:
| a = LIDENT LBRA l = LIDENT RBRA { LabeledActor(a, l) }

pool:
| ASSIETTE p = LIDENT c = ioption(context_refinement) { Pool(p, c) }

context_refinement:
| LPAR c = separated_nonempty_list(COMMA,context_refine_item) RPAR { c }

context_refine_item:
| c = UIDENT { CCase c }
| TOUT ct = UIDENT { CFullType ct }

literal:
| i = INT { LitInt i }
| r = FLOAT { LitRational r }
| m = MONEY { LitMoney m }
| d = duration { LitDuration d }
| d = DATE { LitDate d }

duration:
| d = duration_year { d }
| d = duration_month { d }

duration_year:
| i = INT ANS m = duration_month?
 { let y = Date.Period.year i in
   match m with
   | None -> y
   | Some m -> Date.Period.add y m
 }

duration_month:
| i = INT MOIS { Date.Period.month i }

// Flow and IO

input_decl:
| ENTREE input_name = LIDENT input_context = loption(input_context) input_type = input_type
 {{
   input_name;
   input_context;
   input_type;
 }}
| ENTREE ASSIETTE input_name = LIDENT input_context = loption(input_context)
 {{
   input_name;
   input_context;
   input_type = Money;
 }}

input_context:
| CONTEXTUALISEE PAR ids = UIDENT+ { ids }

input_type:
| TYPE t = base_type { t }

base_type:
| ENTIER { Integer }
| RATIONNEL { Rational }
| ARGENT { Money }

actor_decl:
| ACTEUR id = LIDENT { id }

destinataire:
| VERS d = holder { d, NoOpposition }
| VERS d = holder o = opposition { d, o }

opposition:
| OPPOSABLE f = formula POUR a = actor { Opposable (f, a) }

// Event

event_decl:
| EVENEMENT event_name = LIDENT ATTEINT QUAND event_expr = event_expr
 {{ event_name; event_expr; }}

event_expr:
| EVENEMENT id = LIDENT { EventId id }
| f = formula { EventFormula f }
| e1 = event_expr ET e2 = event_expr { EventConj(e1, e2) }
| e1 = event_expr OU e2 = event_expr { EventDisj(e1, e2) }

event_guard:
| AVANT e = event_expr { Before e }
| APRES e = event_expr { After e }
| QUAND e = event_expr { When e }

// Context

context_decl:
| CONTEXTE context_type_name = UIDENT COLON context_type_cases = context_case+
 {{ context_type_name; context_type_cases }}

context_case:
| MINUS id = UIDENT { id }

context:
| POUR tid = UIDENT cases = nonempty_list(UIDENT) { Cases(tid, cases) }
| POUR TOUT id = UIDENT { Forall id }

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
| o = operation { DOperation o }
| e = event_decl { DEvent e }
| c = constant_decl { DConstant c }
| c = context_decl { DContext c }
| i = input_decl { DInput i }
| o = actor_decl { DActor o }
/* | s = section { DSection s } */
| d = default_decl { let (s, d) = d in DDefault (s, d) }
| d = deficit_decl { let (s, d) = d in DDeficit (s, d) }
| a = advance { DAdvance a }

program: d = toplevel_decl* EOF { d }
