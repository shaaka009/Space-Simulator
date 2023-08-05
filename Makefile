.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop lib

test:
	OCAMLRUNPARAM=b dune exec test/test.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean

loc:
	dune clean
	cloc --by-file --include-lang=OCaml .
	dune build

bisect:
	rm -rf _coverage bisect*.coverage
	dune exec --instrument-with bisect_ppx test/test.exe
	bisect-ppx-report html 

bisect-clean: 
	rm -rf _coverage bisect*.coverage

doc:
	dune build
	dune build @doc

opendoc: doc
	@bash opendoc.sh
