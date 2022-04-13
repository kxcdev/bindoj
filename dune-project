(lang dune 3.0)
(name bindoj)

(license Apache-2.0)
(maintainers
  "Hirotetsu Hongo <hg@kxc.io>"
  "Haochen Xie <hx@kxc.io>"
  "Shiya Yamaguchi <wd@kxc.io>"
  "KXC Members <info@kxc.io>")
(authors
  "Hirotetsu Hongo"
  "Haochen Xie"
  "Shiya Yamaguchi"
  "Yuta Sato"
  "KXC Members and Contributors")

(source
 (github kxcteam/bindoj))

(generate_opam_files true)
(package
 (name bindoj)
 (synopsis "A generative datatype toolkit for OCaml centric workflows")
 (depends
  (ppxlib (>= 0.26))
  (zed (>= 3.1.0))
  (odoc (and :dev (>= 2.1.0)))
  (jsonm (>= 1.0))
  (yojson (>= 1.7))
  (alcotest :with-test)
  (ppx_inline_test :with-test)
  (ocaml (>= 4.12))))
