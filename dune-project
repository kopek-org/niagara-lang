(lang dune 2.9)

(using menhir 2.1)

(implicit_transitive_deps false)

(name niagara-lang)

(maintainers keryan.didier@ocamlpro.com)

(authors keryan.didier@ocamlpro.com julien.blond@ocamlpro.com)

(homepage https://github.com/kopek-org/niagara-lang)

(bug_reports https://github.com/kopek-org/niagara-lang/issues)

(license AGPL-3.0-only)

(package
 (name niagara-lang-lib)
 (synopsis "The Niagara language compiler library")
 (depends
  (menhirLib
   (>= 20250912))
  (gen
   (>= 1.1))
  dune
  (ocaml
   (>= 4.14))
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
   (>= 2.0.0))
  ocamlgraph
  (zarith
   (>= 1.14))))

(package
 (name niagara-lang)
 (synopsis "A compiler for cinema revenue distribution language")
 (depends
  (zarith
   (>= 1.14))
  (logs
   (>= 0.9.0))
  (fmt
   (>= 0.11.0))
  (cmdliner
   (>= 1.3.0))
  (calendar
   (>= 3.0.0))
  dune
  niagara-lang-lib))

(generate_opam_files true)

(cram enable)
