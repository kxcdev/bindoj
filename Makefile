.PHONY: default info setup build test doc clean

default: info build

info:
  $(info =========================== Building bindoj ===========================)
  $(info > Hint: you may need to setup git submodules and run 'make setup' first)
  $(info =======================================================================)

setup:
	opam install . --deps-only --with-test
	opam install -y dune

	yarn --cwd with_js install
	yarn --cwd doc/app install

build:
	dune build

test:
	dune runtest

doc:
	dune build @doc

	mkdir -p doc/dist
	cp doc/tests_src/*.md doc/dist/

	rm -rf doc/app/public/html
	cp -r _build/default/_doc/_html doc/app/public/html

clean:
	dune clean
	yarn --cwd with_js clean || echo "yarn clean failed... you probably does not have a complete setup yet"
	rm -rf with_js/node_modules
	rm -rf doc/app/node_modules
