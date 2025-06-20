(lang dune 3.19)
(name lxc)

(using menhir 2.1)

(package
 (name lxc)
 (depends
  ocaml
  dune
  menhir
  (alcotest :with-test)))