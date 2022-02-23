all: rtrans.native vgtrans.native fo2reg.native

rtrans.native: rtrans.ml trans.ml FO.ml Misc.ml db_lexer.mll db_parser.mly fo_lexer.mll fo_parser.mly verified.ml
	ocamlbuild -use-menhir -package zarith rtrans.native

vgtrans.native: vgtrans.ml trans.ml FO.ml Misc.ml db_lexer.mll db_parser.mly fo_lexer.mll fo_parser.mly verified.ml
	ocamlbuild -use-menhir -package zarith vgtrans.native

fo2reg.native: rtrans.ml FO.ml Misc.ml fo_lexer.mll fo_parser.mly
	ocamlbuild -use-menhir -package zarith fo2reg.native
