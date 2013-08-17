structure UntypedLambda = 
struct
  fun main () = 
    let val [fileName] = CommandLine.arguments()
        val fileStream = TextIO.openIn fileName handle Io =>
                  (TextIO.print ("Can't open "^fileName^".\n");
                  raise Fail "Exiting\n")
        val ast = Parser.parse fileStream
        val nf = Core.eval ast
    in
      Control.debugPrint ((Ast.layout ast)^"\n");
      print ((Ast.layout nf)^"\n")
    end
end
