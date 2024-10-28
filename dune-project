(lang dune 3.16)
(using menhir 2.0)

(name eva)

(generate_opam_files true)

(source
 (github nlordell/eva))

(authors "Nicholas Rodrigues Lordello")

(maintainers "Nicholas Rodrigues Lordello")

(license GPL-3.0-only)

(documentation https://codeberg.org/nlordell/eva)

(package
 (name eva)
 (synopsis "An EVM Assembler")
 (description "An EVM assembler.")
 (depends ocaml dune menhir zarith (alcotest :with-test))
 (tags
  (assembler evm ethereum)))
