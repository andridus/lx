(lang dune 3.19)
(name lx)

(using menhir 2.1)

(package
 (name lx)
 (depends
  ocaml
  dune
  menhir
  (alcotest :with-test)))