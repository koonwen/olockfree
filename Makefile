.PHONY:test

run:
	dune exec -- bin/main.exe

switch: dune-project
	opam switch create . --deps-only --with-test
	
test:
	dune runtest
