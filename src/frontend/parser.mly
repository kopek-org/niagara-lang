%{
open CalendarLib
open Ast
%}

%token OPERATION QUOTEPART BONUS SUR ASSIETTE ANS MOIS ENTREE LBRA RBRA DEFAUT
%token LCUR RCUR CALCULABLE CONTEXTUALISEE PAR TYPE ENTIER RATIONNEL ARGENT
%token SORTIE POUR EVENEMENT ET OU AVANT APRES QUAND CONTEXTE TOUT CONSTANTE
%token SECTION FIN LPAR RPAR VERS ATTEINT PLUS MINUS MULT DIV EQ COLON EOF
%token DEFICITAIRE AVANCE MONTANT COMMA
%token<float> FLOAT
%token<int> INT MONEY
%token<string> LIDENT UIDENT LABEL
%token<CalendarLib.Date.t> DATE

%left PLUS MINUS OU
%left MULT DIV ET

%on_error_reduce literal formula event_guard

%start<Ast.program> program

%%

// Dispatch

operation:
| OPERATION op_label = LABEL op_default_output = destinataire?
    op_context = context* op_source = source? es = expression+
 {{
   op_label;
   op_default_output;
   op_context;
   op_source;
   op_guarded_redistrib = match es with [e] -> e | _ -> Seq es;
  }}

advance:
| AVANCE adv_label = LABEL adv_source = source PAR
    adv_provider = flow_expr MONTANT adv_amount = formula adv_output = destinataire
  {{
      adv_label;
      adv_output;
      adv_source;
      adv_provider;
      adv_amount;
  }}

simple_expr:
| QUOTEPART f = formula d = destinataire? { Part f, d }
| BONUS f = formula d = destinataire? { Flat f, d }

expression:
| e = simple_expr { Redist e }
| g = event_guard ge = expression { Guarded (g, ge) }
| LCUR es = expression+ RCUR { Seq es }

source:
| SUR f = flow_expr { f }

// Formula

formula:
| l = literal { Literal l }
| id = LIDENT { ValueId id }
| f = flow_expr { Flow f }
| f1 = formula op = binop f2 = formula { Binop(op, f1, f2) }
| LPAR f = formula RPAR { f }

%inline binop:
| PLUS { Add }
| MINUS { Sub }
| MULT { Mult }
| DIV { Div }

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
| ENTREE input_computable = boption(CALCULABLE) input_name = LIDENT
  input_context = loption(input_context) input_type = input_type
 {{
   input_name;
   input_computable;
   input_context;
   input_type;
 }}

input_context:
| CONTEXTUALISEE PAR ids = UIDENT+ { ids }

input_type:
| TYPE t = base_type { t }

base_type:
| ENTIER { Integer }
| RATIONNEL { Rational }
| ARGENT { Money }

output_decl:
| SORTIE id = LIDENT { id }

destinataire:
| VERS f = flow_expr { f }

flow_expr:
| ASSIETTE id = LIDENT { Pool id }
| id = LIDENT LBRA l = LIDENT RBRA { LabeledOutput (id, l) }
| id = LIDENT LBRA l = separated_nonempty_list(COMMA,UIDENT) RBRA { ContextPool (id, l) }

// Event

event_decl:
| EVENEMENT event_name = LIDENT ATTEINT QUAND event_expr = event_expr
 {{ event_name; event_expr; }}

event_expr:
| EVENEMENT id = LIDENT { EventId id }
| f1 = formula EQ f2 = formula { EventEq(f1, f2) }
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
| DEFAUT SUR src = flow_expr VERS dst = flow_expr { src, dst }

deficit_decl:
| DEFICITAIRE SUR src = flow_expr PAR def = flow_expr { src, def }

section:
| SECTION section_name = UIDENT section_context = context*
  section_guards = event_guard* section_decl = toplevel_decl* FIN
 {{
   section_name;
   section_context;
   section_guards;
   section_decl;
 }}

toplevel_decl:
| o = operation { Operation o }
| e = event_decl { Event e }
| c = constant_decl { Constant c }
| c = context_decl { Context c }
| i = input_decl { Input i }
| o = output_decl { Output o }
| s = section { Section s }
| d = default_decl { let (s, d) = d in Default (s, d) }
| d = deficit_decl { let (s, d) = d in Deficit (s, d) }
| a = advance { Advance a }

program: d = toplevel_decl* EOF { d }
