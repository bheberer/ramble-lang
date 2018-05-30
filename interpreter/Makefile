.PHONY = all,clean

all:
	rm -f *.native *.byte
	ocamlbuild -use-ocamlfind -cflag -g -lflag -g main.byte -pkgs str
	mv main.byte ramblevm

clean:
	rm ramblevm
	ocamlbuild -clean
