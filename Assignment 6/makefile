all:
	ocamlc -c a6.mli
	ocamlc -c a6.ml
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c interpreter.ml
	ocamlc -o simluator a6.cmo lexer.cmo parser.cmo interpreter.cmo

clean:
	rm simluator *.cmo *.cmi parser.mli lexer.ml parser.ml