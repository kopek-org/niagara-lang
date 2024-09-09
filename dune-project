(lang dune 2.9)

(using menhir 2.1)

(implicit_transitive_deps false)

(name niagara-lang)

(maintainers "keryan.didier@ocamlpro.com")

(authors "keyran.didier@ocamlpro.com" "julien.blond@ocamlpro.com")

(homepage "https://github.com/kopek-org/niagara-lang")

(bug_reports "https://github.com/kopek-org/niagara-lang/issues")

(package
 (name niagara-lang-lib)
 (synopsis "The Niagara language compiler library")
 (depends
  dune
  ocamldot
  (ocaml
   (>= 4.13))
  (sedlex
   (>= 3.0))
  menhir
  (calendar
   (>= 3.0))
  (logs
   (>= 0.7.0))
  (fmt
   (>= 0.8.9))
  (cmdliner
   (>= 1.0.4))
  ocamlgraph
  zarith))

(package
 (name niagara-lang)
 (synopsis "A compiler for cinema revenue distribution language")
 (depends dune niagara-lang-lib))

(generate_opam_files true)
