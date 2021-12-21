.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

board: 
	OCAMLRUNPARAM=b dune exec src/board.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f chess-game.zip
	zip -r chess-game.zip . -x@exclude.lst

clean:
	dune clean
	rm -f adventure.zip

doc:
	dune build @doc