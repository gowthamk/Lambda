functor UntypedLrValsFun (structure Token : TOKEN) : Untyped_LRVALS = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)(*  User declarations section for helper functions *)
open Ast

(*#line 10.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\020\000\003\000\019\000\005\000\018\000\006\000\017\000\
\\007\000\016\000\008\000\015\000\011\000\014\000\012\000\013\000\
\\013\000\012\000\014\000\011\000\000\000\
\\001\000\001\000\020\000\003\000\019\000\005\000\018\000\006\000\017\000\
\\007\000\016\000\008\000\015\000\011\000\014\000\012\000\013\000\
\\013\000\012\000\014\000\011\000\015\000\010\000\000\000\
\\001\000\002\000\036\000\000\000\
\\001\000\004\000\035\000\000\000\
\\001\000\005\000\024\000\000\000\
\\001\000\005\000\030\000\000\000\
\\001\000\009\000\034\000\000\000\
\\001\000\010\000\040\000\000\000\
\\001\000\015\000\010\000\000\000\
\\001\000\016\000\033\000\000\000\
\\001\000\017\000\022\000\000\000\
\\001\000\017\000\023\000\000\000\
\\001\000\018\000\000\000\000\000\
\\043\000\000\000\
\\044\000\000\000\
\\045\000\000\000\
\\046\000\000\000\
\\047\000\000\000\
\\048\000\000\000\
\\049\000\000\000\
\\050\000\000\000\
\\051\000\000\000\
\\052\000\000\000\
\\053\000\000\000\
\\054\000\003\000\019\000\005\000\018\000\006\000\017\000\007\000\016\000\
\\011\000\014\000\000\000\
\\055\000\000\000\
\\056\000\000\000\
\\057\000\000\000\
\\058\000\000\000\
\\059\000\000\000\
\\060\000\000\000\
\\061\000\000\000\
\"
val actionRowNumbers =
"\001\000\025\000\024\000\015\000\
\\017\000\010\000\011\000\013\000\
\\004\000\000\000\000\000\000\000\
\\031\000\000\000\030\000\029\000\
\\027\000\000\000\005\000\026\000\
\\008\000\000\000\009\000\023\000\
\\022\000\021\000\006\000\003\000\
\\002\000\016\000\014\000\000\000\
\\000\000\028\000\000\000\018\000\
\\007\000\019\000\000\000\020\000\
\\012\000"
val gotoT =
"\
\\001\000\040\000\002\000\007\000\003\000\006\000\004\000\005\000\
\\005\000\004\000\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\008\000\019\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\023\000\007\000\002\000\008\000\001\000\000\000\
\\006\000\024\000\007\000\002\000\008\000\001\000\000\000\
\\006\000\025\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\006\000\026\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\027\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\003\000\029\000\004\000\005\000\005\000\004\000\000\000\
\\006\000\030\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\035\000\007\000\002\000\008\000\001\000\000\000\
\\006\000\036\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\006\000\037\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\039\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 41
val numrules = 19
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | VAR of  (string) | atom of  (exp) | app_term of  (exp) | expr of  (exp) | decl of  (decl) | topdecl of  (top_decl) | topdecls of  (top_decl list) | prog of  (prog) | start of  (top_level)
end
type svalue = MlyValue.svalue
type result = top_level
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 17) => true | _ => false
val showTerminal =
fn (T 0) => "LAM"
  | (T 1) => "DOT"
  | (T 2) => "LPAREN"
  | (T 3) => "RPAREN"
  | (T 4) => "VAR"
  | (T 5) => "TRUE"
  | (T 6) => "FALSE"
  | (T 7) => "IF"
  | (T 8) => "THEN"
  | (T 9) => "ELSE"
  | (T 10) => "ZERO"
  | (T 11) => "SUCC"
  | (T 12) => "PRED"
  | (T 13) => "ISZERO"
  | (T 14) => "VAL"
  | (T 15) => "EQUALOP"
  | (T 16) => "SEMICOLON"
  | (T 17) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.prog prog, prog1left, prog1right)) :: rest671)) => let val  result = MlyValue.start ((*#line 52.15 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)prog(*#line 225.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, prog1left, prog1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.expr expr, _, expr1right)) :: _ :: ( _, ( MlyValue.topdecls topdecls, topdecls1left, _)) :: rest671)) => let val  result = MlyValue.prog ((*#line 54.34 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)Prog(topdecls,expr)(*#line 229.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, topdecls1left, expr1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.expr expr, expr1left, expr1right)) :: rest671)) => let val  result = MlyValue.prog ((*#line 55.15 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)Prog([],expr)(*#line 233.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, expr1left, expr1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.topdecls topdecls, _, topdecls1right)) :: _ :: ( _, ( MlyValue.topdecl topdecl, topdecl1left, _)) :: rest671)) => let val  result = MlyValue.topdecls ((*#line 57.40 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)topdecl :: topdecls(*#line 237.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, topdecl1left, topdecls1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.decl decl, decl1left, decl1right)) :: rest671)) => let val  result = MlyValue.topdecl ((*#line 59.17 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)decl(*#line 241.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, decl1left, decl1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.expr expr, _, expr1right)) :: _ :: ( _, ( MlyValue.VAR VAR, _, _)) :: ( _, ( _, VAL1left, _)) :: rest671)) => let val  result = MlyValue.decl ((*#line 61.30 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)Val(VAR,expr)(*#line 245.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, VAL1left, expr1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.expr expr, _, expr1right)) :: _ :: ( _, ( MlyValue.VAR VAR, _, _)) :: ( _, ( _, LAM1left, _)) :: rest671)) => let val  result = MlyValue.expr ((*#line 63.27 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)Abs(VAR,expr)(*#line 249.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, LAM1left, expr1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.expr expr3, _, expr3right)) :: _ :: ( _, ( MlyValue.expr expr2, _, _)) :: _ :: ( _, ( MlyValue.expr expr1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.expr ((*#line 64.38 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)Ite(expr1,expr2,expr3)(*#line 253.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, IF1left, expr3right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.expr expr, _, expr1right)) :: ( _, ( _, SUCC1left, _)) :: rest671)) => let val  result = MlyValue.expr ((*#line 65.20 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)Succ expr(*#line 257.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, SUCC1left, expr1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.expr expr, _, expr1right)) :: ( _, ( _, PRED1left, _)) :: rest671)) => let val  result = MlyValue.expr ((*#line 66.20 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)Pred expr(*#line 261.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, PRED1left, expr1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.expr expr, _, expr1right)) :: ( _, ( _, ISZERO1left, _)) :: rest671)) => let val  result = MlyValue.expr ((*#line 67.22 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)IsZero expr(*#line 265.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, ISZERO1left, expr1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.app_term app_term, app_term1left, app_term1right)) :: rest671)) => let val  result = MlyValue.expr ((*#line 68.19 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)app_term(*#line 269.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, app_term1left, app_term1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.atom atom, atom1left, atom1right)) :: rest671)) => let val  result = MlyValue.app_term ((*#line 70.19 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)atom(*#line 273.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, atom1left, atom1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.atom atom, _, atom1right)) :: ( _, ( MlyValue.app_term app_term, app_term1left, _)) :: rest671)) => let val  result = MlyValue.app_term ((*#line 71.28 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)App(app_term,atom)(*#line 277.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, app_term1left, atom1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.VAR VAR, VAR1left, VAR1right)) :: rest671)) => let val  result = MlyValue.atom ((*#line 73.14 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)Var(VAR)(*#line 281.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, VAR1left, VAR1right), rest671)
end
|  ( 15, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr expr, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.atom ((*#line 74.29 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)expr(*#line 285.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 16, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  result = MlyValue.atom ((*#line 75.16 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)True(*#line 289.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 17, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let val  result = MlyValue.atom ((*#line 76.16 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)False(*#line 293.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 18, ( ( _, ( _, ZERO1left, ZERO1right)) :: rest671)) => let val  result = MlyValue.atom ((*#line 77.15 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm"*)Zero(*#line 297.1 "/Users/gowtham/git/hacks/tapl/untyped++/untyped.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, ZERO1left, ZERO1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Untyped_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun LAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun VAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VAR i,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun ZERO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun SUCC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun PRED (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun ISZERO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun EQUALOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
end
end
