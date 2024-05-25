all: start ocamllex ocamlyacc ocamlc1 ocamlc2 ocamlc3 ocamlc4 ocamlc5 run

start:
	ocamlc -c a4_types.ml

ocamllex: start
	ocamllex a4_lexer.mll

ocamlyacc: ocamllex
	ocamlyacc a4_parser.mly

ocamlc1: ocamlyacc
	ocamlc -c a4_parser.mli

ocamlc2: ocamlc1
	ocamlc -c a4_parser.ml

ocamlc3: ocamlc2
	ocamlc -c a4_lexer.ml

ocamlc4: ocamlc3
	ocamlc -c a4_main.ml

ocamlc5: ocamlc4
	ocamlc -o main a4_types.cmo a4_parser.cmo a4_lexer.cmo a4_main.cmo

run: ocamlc5
	./main prolog.txt

clean:
	rm -f  a4_lexer a4_lexer.ml a4_parser a4_parser.ml *.cmi *.cmo *.mli main *.cmi *.cmo 

.PHONY: start all ocamllex ocamlyacc ocamlc1 ocamlc2 ocamlc3 ocamlc4 ocamlc5 run clean