%{

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

%start<unit> program

%%

// Dispatch

remuneration:
| REMUNERATION destinataire? context_expr { () }

simple_expr:
| QUOTEPART formula strict_destinataire? { () }
| BONUS formula strict_destinataire? { () }

expression:
| simple_expr { () }
| event_guard expression { () }

sourced_expr:
| source? expression+ { () }

source:
| SOURCE LIDENT lane? { () }

context_expr:
| context* sourced_expr { () }

// Formula

formula:
| literal { () }
| LIDENT { () }
| formula TOTALISE { () }
| formula lane { () }
| formula binop formula { () }
| LPAR formula RPAR { () }

%inline binop:
| PLUS { () }
| MINUS { () }
| MULT { () }
| DIV { () }

literal:
| INT { () }
| FLOAT { () }
| MONEY { () }
| duration { () }
| DATE { () }

duration:
| duration_year { () }
| duration_month { () }

duration_year:
| INT ANS duration_month? { () }

duration_month:
| INT MOIS { () }

// Flow and IO

input_decl:
| ENTREE CALCULABLE? LIDENT input_context? input_type { () }

input_context:
| CONTEXTUALISEE PAR UIDENT+ { () }

input_type:
| TYPE typ { () }

typ:
| ENTIER { () }
| RATIONNEL { () }
| ARGENT{ () }
| FLUX typ { () }

output_decl:
| SORTIE LIDENT { () }

destinataire:
| lane { () }
| strict_destinataire { () }

strict_destinataire:
| VERS LIDENT lane? { () }

lane:
| PAR COULOIR LIDENT { () }

// Event

event_decl:
| EVENEMENT LIDENT ATTEINT QUAND event_expr { () }

event_expr:
| EVENEMENT LIDENT { () }
| formula EQ formula { () }
| event_expr ET event_expr { () }
| event_expr OU event_expr { () }

event_guard:
| AVANT event_expr { () }
| APRES event_expr { () }
| QUAND event_expr { () }

// Context

context_decl:
| CONTEXTE UIDENT COLON context_case+ { () }

context_case:
| MINUS UIDENT { () }

context:
| POUR UIDENT nonempty_list(UIDENT) { () }
| POUR TOUT UIDENT { () }

// Program

constant_decl:
| CONSTANTE LIDENT COLON literal { () }

section:
| SECTION UIDENT context* event_guard* toplevel_decl* FIN UIDENT? { () }

toplevel_decl:
| remuneration { () }
| event_decl { () }
| constant_decl { () }
| context_decl { () }
| input_decl { () }
| output_decl { () }
| section { () }

program: toplevel_decl* EOF { () }
