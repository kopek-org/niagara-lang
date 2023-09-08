open Surface
open Internal

(** From linear declaration to computation tree graph,
    with distinguished context variables *)
val translate_program : Ast.contextualized Ast.program -> Ir.program
