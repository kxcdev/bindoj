.PHONY: default info setup dune-gen build test _coverage coverage gen _doc doc doc-serve clean audit promote-audit promote-test promote

default: info build

info:
  $(info =========================== Building bindoj ===========================)
  $(info > Hint: you may need to setup git submodules and run 'make setup' first)
  $(info =======================================================================)

setup:
	opam install . --deps-only --with-test --with-doc
	opam install -y dune

	yarn --cwd with_js install
	yarn --cwd doc/app install

dune-gen:
	dune build @dune-gen --auto-promote

build: dune-gen
	dune build

audit:
	node ./scripts/audit_banner.js

promote-audit:
	node ./scripts/audit_banner.js update

promote-test:
	dune runtest --auto-promote

promote: promote-audit promote-test

gen: build
	dune build @gen

test: audit build gen
	yarn --cwd with_js clean || echo "yarn clean failed... you probably does not have a complete setup yet"
	dune runtest

_coverage:
	rm -rf _coverage
	-dune runtest -f --instrument-with bisect_ppx --no-buffer
	@(mkdir -p _coverage/ocaml/coverage-files && \
	  find _build/default | grep -E 'bisect.+[.]coverage$$' | xargs -I{} cp {} _coverage/ocaml/coverage-files)
	@(bisect-ppx-report html -o _coverage/ocaml/doc _coverage/ocaml/coverage-files/*)
	@echo ">>> OCaml coverage:"
	@(bisect-ppx-report summary _coverage/ocaml/coverage-files/*)

coverage: build _coverage

_doc:
	dune build @doc
	dune build @doc --root=vendors/kxclib

	mkdir -p doc/dist
	cp doc/tests_src/*.md doc/dist/

	rm -rf doc/app/public/html
	cp -r _build/default/_doc/_html doc/app/public/html
	cp -r vendors/kxclib/_build/default/_doc/_html/kxclib doc/app/public/html/kxclib

	rm -rf doc/example
	cp -r _build/default/example/docs doc/example

	rm -rf doc/app/public/coverage
	cp -r _coverage/ocaml/doc doc/app/public/coverage

doc: build coverage _doc

doc-serve: build _doc
	cd doc/app && yarn dev

clean:
	dune clean
	yarn --cwd with_js clean || echo "yarn clean failed... you probably does not have a complete setup yet"
	rm -rf with_js/node_modules
	rm -rf doc/app/node_modules
