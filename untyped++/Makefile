all:lex yacc parser

lex:untyped.lex
	mllex untyped.lex

yacc:untyped.grm
	mlyacc untyped.grm

parser:
	mlton -output untyped++ sources.mlb

clean:
	rm untyped.lex.sml untyped.grm.sml untyped++
