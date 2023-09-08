open Surface.Ast

(** Propagate and store context constraints, and hash-consign identifiers *)
val program : source program -> contextualized program
