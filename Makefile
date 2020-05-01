# create opam switch with:
# opam switch create . opam-base-compiler.4.10.0

all:
	dune build app/main.exe
	./_build/default/app/main.exe -cpp clang++ test/example.clm

# install necessary packages
init:
	opam install merlin ocp-indent dune utop base core menhir

clean: 
	rm -rf ./_build/*
