open Surface
open Dataflow

type result = {
  infos : Ast.program_infos;
  aggr_eqs : Equ.aggregate_eqs;
  event_eqs : Equ.expr Variable.Map.t;
}

val translate_program : Ast.contextualized Ast.program -> result
