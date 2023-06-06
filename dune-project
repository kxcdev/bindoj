(lang dune 3.0)
(using mdx 0.2)
(using directory-targets 0.1)
(name bindoj)

(license Apache-2.0)
(maintainers
  "Hirotetsu Hongo <hg@kxc.inc>"
  "Haochen Xie <hx@kxc.inc>"
  "Shiya Yamaguchi <wd@kxc.inc>"
  "Yuta Sato <cr@kxc.inc>"
  "KXC Members <info@kxc.inc>")
(authors
  "Hirotetsu Hongo"
  "Haochen Xie"
  "Shiya Yamaguchi"
  "Yuta Sato"
  "KXC Members and Contributors")

(source
 (github kxcdev/bindoj))

(generate_opam_files true)
(package
 (name bindoj)
 (synopsis "A generative datatype toolkit for OCaml centric workflows")
 (depends
  (ppxlib (>= 0.26))
  (ppx_deriving (>= 5.2.1))
  (yojson (>= 1.7))
  (ppx_yojson_conv (>= 0.15.0))
  (zed (>= 3.1.0))
  (odoc (and :dev (>= 2.1.0)))
  (jsonm (>= 1.0))
  (js_of_ocaml (and :with-test (>= 4.0.0) (< 5.1.0)))
  (js_of_ocaml-ppx (and :with-test (>= 4.0.0)))
  (gen_js_api (and :with-test (>= 1.1.1)))
  (mdx (and :with-test (>= 2.1.0)))
  (alcotest :with-test)
  (qcheck :with-test)
  (prr (>= 0.1.1))
  (ppx_inline_test :with-test)
  (lwt :with-test)
  (uri (>= 4.2.0))
  (ocaml (>= 4.12))))
