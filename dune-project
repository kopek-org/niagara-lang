(lang dune 2.9)
(using menhir 2.1)

(name niagara-lang)

(package
  (name niagara-lang-lib)
  (synopsis "The Niagara language compiler library")
  (depends
    dune
    (ocaml (>= 4.13))
    (sedlex (>= 3.0))
    menhir
    ocamlgraph
    (calendar (>= 3.0)))
)

(package
  (name niagara-lang)
  (synopsis "A compiler for cinema revenue distribution language")
  (depends
    dune
    niagara-lang-lib)
)

(generate_opam_files true)
