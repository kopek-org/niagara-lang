open Surface
open Dataflow

type result = {
  infos : ProgramInfo.t;
  aggr_eqs : Equ.aggregate_eqs;
  event_eqs : Equ.expr Variable.Map.t;
}

val translate_program : Ast.contextualized Ast.program -> result
