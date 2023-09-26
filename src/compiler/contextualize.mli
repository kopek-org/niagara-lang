open Surface.Ast

(** Propagate and store context constraints, and name resolution *)
val program : source program -> contextualized program
