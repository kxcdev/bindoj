(lang dune 3.0)
(using mdx 0.2)
(using directory-targets 0.1)
(name bindoj)

(license Apache-2.0)
(maintainers
  "Hirotetsu Hongo <hg@kxc.inc>"
  "Haochen M. Kotoi-Xie <hx@kxc.inc>"
  "Shiya Yamaguchi <wd@kxc.inc>"
  "Yuta Sato <cr@kxc.inc>"
  "Wataru Kaji <wk@kxc.inc>"
  "KXC Members <info@kxc.inc>")
(authors
  "Hirotetsu Hongo"
  "Haochen M. Kotoi-Xie"
  "Shiya Yamaguchi"
  "Yuta Sato"
  "Wataru Kaji"
  "KXC Members and Contributors")

(source
 (github kxcdev/bindoj))

(generate_opam_files true)
(package
 (name bindoj)
 (synopsis "A generative datatype toolkit for OCaml centric workflows")
 (depends
  (fmt (and :with-test (>= 0.8.10)))
  (ppxlib (>= 0.31))
  (ppx_deriving (>= 5.2.1))
  (yojson (>= 1.7))
  (ppx_yojson_conv (>= v0.15.0))
  (zed (>= 3.1.0))
  (odoc (and :with-doc (>= 2.1.0)))
  (jsonm (>= 1.0))
  (js_of_ocaml (and :with-test (>= 5.1.0)))
  (js_of_ocaml-ppx (and :with-test (>= 4.0.0)))
  (gen_js_api (and :with-test (>= 1.1.1)))
  (mdx (and :with-test (>= 2.1.0)))
  (alcotest :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test)
  (prr (>= 0.1.1))
  (ppx_inline_test :with-test)
  (lwt :with-test)
  (uri (>= 4.2.0))
  (bisect_ppx (>= 2.7.1))
  (ocamlformat (and :with-test (= 0.26.0)))
  (ocaml (>= 4.14))
  (cohttp-lwt-unix :with-test)
  (melange (and :with-doc (>= 2.0.0)))))
