%{

%}

%token<Niagara.ratio> FLOAT
%token<Niagara.thune> THUNE
%token<string> ID SECTION_ID
%token SOURCES SOURCE CESSIONNAIRE CESSIONNAIRES SECTION TAUX EMET JUSQUA PUIS ET
%token EOF SEMI COLON

%nonassoc JUSQUA TAUX
%nonassoc PUIS
%nonassoc ET

%start<Niagara.reseau> program

%%


sources :
  | SOURCES COLON s = separated_list(SEMI, ID) { s }

cessionnaires :
  | CESSIONNAIRES COLON e = separated_list(SEMI, ID) { e }

section_kind :
  | SOURCE s = ID { Niagara.Source s }
  | SECTION s = SECTION_ID { Niagara.Intermediate s }

section :
  | kind = section_kind COLON partial_flux = flux
    { kind, partial_flux }

flux :
  | em = emission { em }
  | rfs = ratio_split df = dispatch_default
    { Niagara.Flux.split rfs df }
  | beff = flux JUSQUA lim = THUNE aftf = dispatch_default
    { Niagara.Flux.until beff lim aftf }

%inline dispatch_default :
  | PUIS f = flux { f }

emission :
  | EMET CESSIONNAIRE id = ID { Niagara.Flux.emission id }
  | EMET SECTION id = SECTION_ID { Niagara.Flux.emission id }

ratio_split :
  | rf = one_ratio_split { [rf] }
  | rf = one_ratio_split ET rfs = ratio_split { rf::rfs }

%inline one_ratio_split :
  | f = flux TAUX t = FLOAT { (f, t) }

program:
  | sources = sources cessionnaires = cessionnaires paliers = section* EOF
    {
      Niagara.reseau sources cessionnaires paliers
    }

%%
