%{
open CalendarLib
open Ast
%}

%token REMUNERATION QUOTEPART BONUS SOURCE TOTALISE ANS MOIS ENTREE CALCULABLE
%token CONTEXTUALISEE PAR TYPE ENTIER RATIONNEL ARGENT FLUX SORTIE POUR COULOIR
%token EVENEMENT ET OU AVANT APRES QUAND CONTEXTE TOUT CONSTANTE SECTION FIN
%token LPAR RPAR VERS ATTEINT PLUS MINUS MULT DIV EQ COLON EOF
%token<float> FLOAT
%token<int> INT MONEY
%token<string> LIDENT UIDENT
%token<CalendarLib.Date.t> DATE

%nonassoc TOTALISE PAR
%left PLUS MINUS OU
%left MULT DIV ET

%on_error_reduce literal formula event_guard

%start<Ast.program> program

%%

// Dispatch

remuneration:
| REMUNERATION rem_default_output = destinataire? ce = context_expr
 {
  let (rem_context,
       (rem_source, rem_guarded_redistrib))
    = ce
  in
  {
   rem_default_output;
   rem_context;
   rem_source;
   rem_guarded_redistrib;
  }}

simple_expr:
| QUOTEPART f = formula d = strict_destinataire? { Part f, d }
| BONUS f = formula d = strict_destinataire? { Flat f, d }

expression:
| e = simple_expr { [], e }
| g = event_guard ge = expression
 { let (gs, e) = ge in
   g::gs, e
 }

sourced_expr:
| s = source? e = expression+ { s, e }

source:
| SOURCE id = LIDENT l = lane? { Destination(id, l) }

context_expr:
| cs = context* se = sourced_expr { cs, se }

// Formula

formula:
| l = literal { Literal l }
| id = LIDENT { ValueId id }
| f = formula TOTALISE { Integral f }
| f = formula l = lane { OnLane(f, l) }
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
| TYPE t = typ { t }

base_type:
| ENTIER { Integer }
| RATIONNEL { Rational }
| ARGENT { Money }

typ:
| t = base_type { Sample t }
| FLUX t = base_type { Flow t }

output_decl:
| SORTIE id = LIDENT { id }

destinataire:
| l = lane { Lane l }
| d = strict_destinataire { d }

strict_destinataire:
| VERS id = LIDENT l = lane? { Destination(id, l) }

lane:
| PAR COULOIR id = LIDENT { id }

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
| r = remuneration { Remuneration r }
| e = event_decl { Event e }
| c = constant_decl { Constant c }
| c = context_decl { Context c }
| i = input_decl { Input i }
| o = output_decl { Output o }
| s = section { Section s }

program: d = toplevel_decl* EOF { d }
