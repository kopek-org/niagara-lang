open Surface.Ast

val projection_of_context_refinement :
  Context.world -> context_refinement -> Context.Group.t

(** Propagate and store context constraints, and name resolution *)
val program : source program -> contextualized program
