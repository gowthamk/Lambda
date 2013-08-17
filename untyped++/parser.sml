structure Parser:sig
  exception ParseError of string * int option * int option
  val parse : TextIO.instream -> Ast.top_level
  val parseString : string -> Ast.top_level
end = 
struct

  exception ParseError of string * int option * int option

  structure UntypedLrVals = UntypedLrValsFun(structure Token = LrParser.Token)
  structure UntypedLex = UntypedLexFun(structure Tokens = UntypedLrVals.Tokens)
  structure UntypedParser = Join (structure LrParser = LrParser
                                 structure ParserData = UntypedLrVals.ParserData
                                 structure Lex = UntypedLex)
  fun parse (instream) = 
    let val grab : int -> string = fn 
            n => if TextIO.endOfStream instream 
                 then ""
                 else TextIO.inputN (instream,n)
        val handleParseError : string * int * int -> unit = fn
            (msg,line,col) => 
            raise ParseError(msg,SOME(line),SOME(col))
        (* Position annotated Token stream *)
        val lexer = UntypedParser.makeLexer grab
        val (ast,_) = UntypedParser.parse
                    (15, lexer, handleParseError,())
                    handle UntypedParser.ParseError => raise ParseError 
                      ("Unknown Parse Error",NONE,NONE)
        val dummyEOF = UntypedLrVals.Tokens.EOF(0,0)
    in
      UntypedLex.UserDeclarations.line := 1;
      ast
    end

  fun parseString str = 
    let val sstream = TextIO.openString str
    in
      parse sstream
    end

end
