.PHONY: default info setup build test doc clean audit promote-audit promote-test promote

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

build:
	dune build

audit:
	node ./scripts/audit_banner.js

promote-audit:
	node ./scripts/audit_banner.js update

promote-test:
	dune runtest --auto-promote

promote: promote-audit promote-test

test: audit
	yarn --cwd with_js clean || echo "yarn clean failed... you probably does not have a complete setup yet"
	dune runtest

doc:
	dune build @doc

	mkdir -p doc/dist
	cp doc/tests_src/*.md doc/dist/

	rm -rf doc/app/public/html
	cp -r _build/default/_doc/_html doc/app/public/html

	rm -rf doc/example
	cp -r _build/default/example/docs doc/example

clean:
	dune clean
	yarn --cwd with_js clean || echo "yarn clean failed... you probably does not have a complete setup yet"
	rm -rf with_js/node_modules
	rm -rf doc/app/node_modules
