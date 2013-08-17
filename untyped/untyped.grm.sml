functor UntypedLrValsFun (structure Token : TOKEN) : Untyped_LRVALS = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "/Users/gowtham/git/hacks/tapl/untyped/untyped.grm"*)(*  User declarations section for helper functions *)
open Ast

(*#line 10.1 "/Users/gowtham/git/hacks/tapl/untyped/untyped.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\008\000\003\000\007\000\005\000\006\000\000\000\
\\001\000\002\000\013\000\000\000\
\\001\000\004\000\012\000\000\000\
\\001\000\005\000\011\000\000\000\
\\001\000\006\000\000\000\000\000\
\\016\000\000\000\
\\017\000\000\000\
\\018\000\003\000\007\000\005\000\006\000\000\000\
\\019\000\000\000\
\\020\000\000\000\
\\021\000\000\000\
\\022\000\000\000\
\"
val actionRowNumbers =
"\000\000\008\000\007\000\005\000\
\\010\000\000\000\003\000\009\000\
\\002\000\001\000\011\000\000\000\
\\006\000\004\000"
val gotoT =
"\
\\001\000\013\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\004\000\007\000\000\000\
\\000\000\
\\000\000\
\\002\000\008\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\012\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 14
val numrules = 7
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
datatype svalue = VOID | ntVOID of unit | VAR of  (string) | atom of  (exp) | non_lam_term of  (exp) | expr of  (exp) | prog of  (top_level)
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
fn (T 5) => true | _ => false
val showTerminal =
fn (T 0) => "LAM"
  | (T 1) => "DOT"
  | (T 2) => "LPAREN"
  | (T 3) => "RPAREN"
  | (T 4) => "VAR"
  | (T 5) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 5) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.expr expr, expr1left, expr1right)) :: rest671)) => let val  result = MlyValue.prog ((*#line 34.14 "/Users/gowtham/git/hacks/tapl/untyped/untyped.grm"*)expr(*#line 153.1 "/Users/gowtham/git/hacks/tapl/untyped/untyped.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, expr1left, expr1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.expr expr, _, expr1right)) :: _ :: ( _, ( MlyValue.VAR VAR, _, _)) :: ( _, ( _, LAM1left, _)) :: rest671)) => let val  result = MlyValue.expr ((*#line 36.27 "/Users/gowtham/git/hacks/tapl/untyped/untyped.grm"*)Abs(VAR,expr)(*#line 157.1 "/Users/gowtham/git/hacks/tapl/untyped/untyped.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, LAM1left, expr1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.non_lam_term non_lam_term, non_lam_term1left, non_lam_term1right)) :: rest671)) => let val  result = MlyValue.expr ((*#line 37.23 "/Users/gowtham/git/hacks/tapl/untyped/untyped.grm"*)non_lam_term(*#line 161.1 "/Users/gowtham/git/hacks/tapl/untyped/untyped.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, non_lam_term1left, non_lam_term1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.atom atom, atom1left, atom1right)) :: rest671)) => let val  result = MlyValue.non_lam_term ((*#line 39.23 "/Users/gowtham/git/hacks/tapl/untyped/untyped.grm"*)atom(*#line 165.1 "/Users/gowtham/git/hacks/tapl/untyped/untyped.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, atom1left, atom1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.atom atom, _, atom1right)) :: ( _, ( MlyValue.non_lam_term non_lam_term, non_lam_term1left, _)) :: rest671)) => let val  result = MlyValue.non_lam_term ((*#line 40.36 "/Users/gowtham/git/hacks/tapl/untyped/untyped.grm"*)App(non_lam_term,atom)(*#line 169.1 "/Users/gowtham/git/hacks/tapl/untyped/untyped.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, non_lam_term1left, atom1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.VAR VAR, VAR1left, VAR1right)) :: rest671)) => let val  result = MlyValue.atom ((*#line 42.14 "/Users/gowtham/git/hacks/tapl/untyped/untyped.grm"*)Var(VAR)(*#line 173.1 "/Users/gowtham/git/hacks/tapl/untyped/untyped.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, VAR1left, VAR1right), rest671)
end
|  ( 6, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr expr, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.atom ((*#line 43.29 "/Users/gowtham/git/hacks/tapl/untyped/untyped.grm"*)expr(*#line 177.1 "/Users/gowtham/git/hacks/tapl/untyped/untyped.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.prog x => x
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
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
end
end
