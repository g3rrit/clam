
all:
	dune build app/main.exe
	./_build/default/app/main.exe -cpp clang++ test/example.clm