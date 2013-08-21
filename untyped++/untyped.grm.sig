signature Untyped_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val EQUALOP:  'a * 'a -> (svalue,'a) token
val VAL:  'a * 'a -> (svalue,'a) token
val ISZERO:  'a * 'a -> (svalue,'a) token
val PRED:  'a * 'a -> (svalue,'a) token
val SUCC:  'a * 'a -> (svalue,'a) token
val ZERO:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val VAR: (string) *  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val LAM:  'a * 'a -> (svalue,'a) token
end
signature Untyped_LRVALS=
sig
structure Tokens : Untyped_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
