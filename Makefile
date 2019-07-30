.PHONY: all clean repl test

all:
	dune build
	dune build src/toploop-main/toploop.exe
	dune build src/translator-main/translator.exe
	rm -f toploop
	rm -f translator
	ln -s _build/default/src/toploop-main/toploop.exe toploop
	ln -s _build/default/src/translator-main/translator.exe translator

sandbox:
	dune build test/sandbox.exe

repl:
	dune utop src -- -require pdr-programming

test:
	dune runtest -f

clean:
	dune clean
	rm -f toploop
	rm -f translator
