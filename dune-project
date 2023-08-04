(lang dune 2.9)
(using menhir 2.1)

(name niagara-lang)

(package
  (name niagara-lang-lib)
  (synopsis "The Niagara language compiler library")
  (depends
    dune
    ocamldot
    (ocaml (>= 4.13))
    (sedlex (>= 3.0))
    menhir
    (calendar (>= 3.0))
    (logs (>= 0.7.0))
    (fmt (>= 0.8.9))
    (cmdliner (>= 1.0.4))
    ocamlgraph)
)

(package
  (name niagara-lang)
  (synopsis "A compiler for cinema revenue distribution language")
  (depends
    dune
    niagara-lang-lib)
)

(generate_opam_files true)
