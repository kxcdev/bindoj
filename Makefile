.PHONY: doc
doc:
	mkdir -p doc/dist
	cp doc/tests_src/*.md doc/dist/

	rm -rf doc/app/public/html
	cp -r _build/default/_doc/_html doc/app/public/html
