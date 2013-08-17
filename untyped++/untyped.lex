structure Tokens = Tokens
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type svalue = Tokens.svalue
type lexresult = (svalue,pos) token
val line = ref 1
val debugFlag = ref false
val eof = fn () => Tokens.EOF(!line,!line)
val debug = fn s => if (!debugFlag) then print s else () 
(*
  Untyped_TOKENS defined using term declaration in grm
*)
%%
%header (functor UntypedLexFun (structure Tokens : Untyped_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
variable=({alpha}|"_")+{digit}*("'")*;
eol=("\n"|"\013\n"|"\013");
ws=[\ \t];
%%
<INITIAL>{eol} => (line := (!line)+1; lex());
<INITIAL>{ws}+ => (debug "whitespace"; lex());
<INITIAL>{variable} => (debug ("var: "^yytext^"\n"); Tokens.VAR(yytext,!line,yypos));
<INITIAL>("\\") => (debug "lam\n";Tokens.LAM(!line,yypos));
<INITIAL>(".") => (debug "dot\n"; Tokens.DOT(!line,yypos));
<INITIAL>("(") => (debug "lparen\n"; Tokens.LPAREN(!line,yypos));
<INITIAL>(")") => (debug "rparen\n"; Tokens.RPAREN(!line,yypos));
<INITIAL>("true") => (debug "true"; Tokens.TRUE(!line,yypos));
<INITIAL>("false") => (debug "false"; Tokens.FALSE(!line,yypos));
<INITIAL>("if") => (debug "if"; Tokens.IF(!line,yypos));
<INITIAL>("then") => (debug "then"; Tokens.THEN(!line,yypos));
<INITIAL>("else") => (debug "else"; Tokens.ELSE(!line,yypos));
<INITIAL>("0") => (debug "zero\n"; Tokens.ZERO(!line,yypos));
<INITIAL>("succ") => (debug "succ"; Tokens.SUCC(!line,yypos));
<INITIAL>("pred") => (debug "pred"; Tokens.PRED(!line,yypos));
<INITIAL>("iszero") => (debug "iszero"; Tokens.ISZERO(!line,yypos));
