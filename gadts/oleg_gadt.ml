(*			
			GADTs in OCaml

We illustrate one simple, pure, magic-free implementation of a form of
GADTs in OCaml that is sufficient for the common applications of GADTs
such as data structures with embedded invariants, typed printf/scanf,
tagless interpreters.  The implementation is a simple module,
requiring no changes to the OCaml system. The implementation is so
trivial that it should work on any ML system (although, like nested
data types, GADTs aren't very useful on an SML system without support
for polymorphic recursion).

The GADT notation turns out lightweight. However, GADTs are often
inductive, and so we need polymorphic recursion to process them. That
adds some (relatively small) notational overhead.  Mainly, GADTs are
often used with existentials -- so often that Haskell makes
existential quantification implicit in GADT declarations.  OCaml, alas,
lacks such notational conveniences, and so existentials (which must be
encoded via double-negation) aren't pretty. Smart constructors --
which can be build mechanically, perhaps with a suitable camlp4
macro -- ease the pain.

*)

(*
As shown by Patricia Johann and Neil Ghani: 
    Foundations for Structured Programming with GADT. POPL 2008.
the essence of GADTs is the EQ GADT, which implements the following interface:

      type ('a,'b) eq
      val refl : ('a,'a) eq
      val apply_eq : ('a,'b) eq -> 'a -> 'b

The value of the type ('a,'b) eq witnesses the equality of two types.
The function apply_eq relies on the witness when performing type coercion.

To be precise, the genuine GADTs provide a more general function
for the Leibniz principle
      val apply_eq : ('a,'b) eq -> 'a tau -> 'b tau
for any type tau. Our implementation supports only those tau that are
functors (that is, admit a map operation). That seems sufficient however
for all the common examples.

The following implementation is genuinely safe, meaning it never leads 
to segmentation faults, even in principle.

*)

module EQ = struct
  type ('a,'b) eq = Refl of 'a option ref * 'b option ref

  let refl () = let r = ref None in Refl (r,r)

  let symm : ('a,'b) eq -> ('b,'a) eq = function 
      Refl (x,y) -> Refl (y,x)

  let apply_eq : ('a,'b) eq -> 'a -> 'b = function
      Refl (rx,ry) -> fun x ->
        rx := Some x;
        match !ry with
	| Some y -> rx := None; y
	|     _  -> failwith "Impossible"
end;;

(* That's all there is to it. 
   For a more efficient but perhaps more problematic
   implementation, see the end of this file.
*)

open EQ;;


(* ------------------------------------------------------------------------ *)
(* Example 0, too simple
   Using GADTs with phantom types to enforce invariants on data structures

   For example, suppose the data type node_t represents inline content
   of an HTML document.  We would like to statically enforce the
   invariant that a link node (such as Href or Mref) must not be an
   ancestor of another link node.
   We would like to map or fold over node_t, preserving the invariant.
   Without GADTs, writing maps over data structure with invariants
   turns out quite tricky.
 
   The problem and the problems with obvious solution have been described
   by Dario Teixeira in a message posted on the Caml-list
   on 15 Jan 2009:
   http://www.mail-archive.com/caml-list@yquem.inria.fr/msg02452.html

   This example is too simple: since the types in question a phantom, 
   no coercions are actually needed...
*)

type node_link = Node_Link		(* Labels for nodes *)
type node_nolink = Node_NoLink
;;

type 'a node_t =
   | Text of string
   | Bold of 'a node_t list
   | Href of (node_link,'a) eq * string
   | Mref of (node_link,'a) eq * string * node_nolink node_t list
;;

(* Smart constructors. Could be mechanically generated. *)
let text txt = Text txt;;
(* val text : string -> 'a node_t *)

let bold seq = Bold seq;;
(* val bold : 'a node_t list -> 'a node_t *)

let href lnk = Href (refl (), lnk);;
(* val href : string -> node_link node_t *)

let mref lnk seq = Mref (refl (),lnk, seq);;
(* val mref : string -> node_nolink node_t list -> node_link node_t *)

let test1 = bold [text "text1"; text "text2"];;
let test2 = bold [text "text1"; href "link1"];; 
(* val test2 : node_link node_t = ... *)

let test3 = mref "link2" [test1; test1];;
(* val test3 : node_link node_t = ... *)

(*
let test31 = mref "link2" [test1; test2];;

Type error:
  This expression [test2] has type node_link node_t but is here used with type
  node_nolink node_t
*)
let test4 = bold [text "text3"; test3];;
(* val test4 : node_link node_t = ... *)

(*
let test41 = mref "link3" [test1; test4];;

Type error:
  This expression [test4] has type node_link node_t but is here used with type
  node_nolink node_t

*)

(* Write the function map over node_t, capitalizing all text *)
type cap_sig = {cap: 'a. 'a node_t -> 'a node_t};;  (* polymorphic recursion *)
let rec capnode = {cap = 
 function 
   | Text x        -> Text (String.capitalize x)
   | Bold x        -> Bold (List.map capnode.cap x)
   | Href (eq,x)   -> Href (eq,String.capitalize x)
   | Mref (eq,l,x) -> Mref (eq,l,List.map capnode.cap x)
};;

let test1c = capnode.cap test1;;
let test3c = capnode.cap test3;;
let test4c = capnode.cap test4;;




(* ------------------------------------------------------------------------ *)
(* Example 1: typed printf and scanf sharing the same format descriptor

   This is straightforward re-implementation of Haskell code:
   http://okmij.org/ftp/typed-formatting/PrintScanI.txt
   http://okmij.org/ftp/typed-formatting/PrintScan.hs

The Haskell code supports an additional format descriptor
PrintScan.hs to allow printing and scanning of any value that can be
printed and read. We elide this extensibility here for clarity of the example.
*)

(* Please refer to Haskell code for comparison and explanation *)
type ('a,'b) fmt =
  | FLit of < m_flit : 'w. (('a,'b) eq -> string -> 'w) -> 'w >
  | FInt of < m_fint : 'w. ((int -> 'a,'b) eq -> 'w) -> 'w >
  | FChr of < m_fchr : 'w. ((char -> 'a,'b) eq -> 'w) -> 'w >
  | FCmp of < m_fcmp : 'w. ('a,'b,'w) fcmp_k -> 'w >
      (* The standard encoding of existentials *)
and ('a,'c,'w) fcmp_k = 
    {fcmp_k : 'b. ('b,'c) fmt * ('a,'b) fmt -> 'w}
;;

(* Smart constructors, as syntactic sugar.
   Method signatures are, alas, required. 
   I wonder if these smart constructors could be generated mechanically,
   by a suitable camlp4 macro.
*)
let f_lit x 
    = FLit (object method m_flit : 'w. (('a,'b) eq -> string -> 'w) -> 'w
	= fun k -> k (refl ()) x end);;
(* val f_lit : string -> ('a, 'a) fmt *)

let f_int 
    = FInt (object method m_fint : 'w. ((int -> 'a,'b) eq -> 'w) -> 'w
	= fun k -> k (refl ()) end);;
(* val f_int : ('a, int -> 'a) fmt *)

let f_char
    = FChr (object method m_fchr : 'w. ((char -> 'a,'b) eq -> 'w) -> 'w
	= fun k -> k (refl ()) end);;
(* val f_char : ('a, char -> 'a) fmt *)

let (^^) f1 f2 = FCmp (object method m_fcmp : 'w. ('a,'b,'w) fcmp_k -> 'w
   = fun k -> k.fcmp_k (f1,f2) end);;
(* val ( ^^ ) : ('a, 'b) fmt -> ('c, 'a) fmt -> ('c, 'b) fmt *)

(* Two interpreters, for printf and scanf. 
   We need polymorphic recursion -- as is typical with GADTs*)

type print_sig = {pr: 'a 'b. ('a,'b) fmt -> (string -> 'a) -> 'b};;
let rec printer = {pr = 
 function 
   | FLit x -> fun k -> x#m_flit (fun eq x -> apply_eq eq (k x))
   | FInt x -> fun k -> x#m_fint (fun eq   -> apply_eq eq 
	 (fun x -> k (string_of_int x)))
   | FChr x -> fun k -> x#m_fchr (fun eq   -> apply_eq eq 
	 (fun x -> k (String.make 1 x)))
   | FCmp x -> fun k ->
       x#m_fcmp {fcmp_k = 
	  fun (a,b) -> printer.pr a (fun sa ->
	                printer.pr b (fun sb -> k (sa ^ sb)))}
};;
let sprintf fmt = printer.pr fmt (fun x -> x);;
(* val sprintf : (string, 'a) fmt -> 'a *)


exception Scan_error of string;;
(* primitive parsers: return the parsed value and the remainder of inp *)
let prefixp str = fun inp -> 
  if String.length str <= String.length inp &&
     str = String.sub inp 0 (String.length str)
  then String.sub inp (String.length str)
      (String.length inp - String.length str)
  else raise (Scan_error "lit")
;;
(* val intS : string -> int * string *)
let intS = fun inp ->
  let n = String.length inp in
  let rec loop acc i =
    if i >= n then (acc,"") else 
    let c = inp.[i] in
    if c >= '0' && c <= '9' then
      loop (acc * 10 + (int_of_char c - int_of_char '0')) (succ i)
    else if i = 0 then raise (Scan_error "int") 
    else (acc, String.sub inp i (n-i)) in
  if n = 0 then raise (Scan_error "int")
  else loop 0 0
;;
(* val charS : string -> char * string *)
let charS = fun inp ->
  if String.length inp = 0 
  then raise (Scan_error "char")
  else (inp.[0], String.sub inp 1 (String.length inp - 1));;

type scan_sig = {sc: 'a 'b. ('a,'b) fmt -> string -> 'b -> 'a * string};;
let rec scanner = {sc = 
 function 
   | FLit x -> fun inp consumer -> 
       x#m_flit (fun eq str -> (apply_eq (symm eq) consumer, prefixp str inp))
   | FInt x -> fun inp consumer ->
       let (v,rest) = intS inp in
       x#m_fint (fun eq -> (apply_eq (symm eq) consumer v, rest))
   | FChr x -> fun inp consumer ->
       let (v,rest) = charS inp in
       x#m_fchr (fun eq -> (apply_eq (symm eq) consumer v, rest))
   | FCmp x -> fun inp consumer ->
       x#m_fcmp {fcmp_k = 
	  fun (a,b) -> 
	    let (va,ra) = scanner.sc a inp consumer in
	    scanner.sc b ra va}
};;
let sscanf inp fmt f = scanner.sc fmt inp f;;
(* val sscanf : string -> ('a, 'b) fmt -> 'b -> 'a * string *)

(* Examples *)

let tp1 = sprintf (f_lit "Hello world");;
(* val tp1 : string = "Hello world" *)

let ts1 = sscanf tp1 (f_lit "Hello world") ();;
(* val ts1 : unit * string = ((), "") *)

let tp2 = sprintf (f_lit "Hello " ^^ f_lit "world" ^^ f_char) '!';;
(* val tp2 : string = "Hello world!" *)

let ts2 = sscanf tp2 (f_lit "Hello " ^^ f_lit "world" ^^ f_char) (fun x -> x);;
(* val ts2 : char * string = ('!', "") *)

(* Formats are first-class and can be constructed incrementally *)
let fmt31 () = f_lit "The value of " ^^ f_char ^^ f_lit " is ";;
(* val fmt31 : unit -> ('a, char -> 'a) fmt *)
let fmt3 () = fmt31 () ^^ f_int;;
(* val fmt3 : unit -> ('a, char -> int -> 'a) fmt *)

let tp3 = sprintf (fmt3 ()) 'x' 3;;
(* val tp3 : string = "The value of x is 3" *)

(* What we print, we can parse back *)
let ts3 = sscanf tp3 (fmt3 ()) (fun x n -> (x,n));;
(* val ts3 : (char * int) * string = (('x', 3), "") *)



(* ------------------------------------------------------------------------ *)
(* Example 2:
   Simply-typed lambda-calculus with constants and higher-order
   abstract syntax. This is essentially the example of Hongwei Xi's et al
   POPL2003 paper.
*)
(* We would love to be able to define the type of expressions 'a exp
as follows:

type 'a exp = 
   | Int of (int, 'a) eq * int         (* Integer literal *)
   | Inc of (int->int, 'a) eq          (* Increment *)
   | Lft of ('a,'a) eq * 'a            (* Lifting any constant *)
   | Lam of exists 'u 'v.
       ('u -> 'v,'a) eq * ('u exp -> 'v exp)  (* Lambda, HOAS *)
   | App of exists 'u 'v.              (* Application *)
       ('v,'a) eq * ('u -> 'v) exp * 'u exp

Alas, OCaml syntax -- the lack of existential quantification --
prevents this. We have to encode existentials as universals, using
double-negation. 
*)

type 'a exp =
   | Int of < m_int : 'w. ((int,'a) eq -> int -> 'w) -> 'w >
   | Inc of < m_inc : 'w. ((int->int,'a) eq -> 'w) -> 'w >
   | Lft of < m_lft : 'w. (('a,'a) eq -> 'a -> 'w) -> 'w >
   | Lam of < m_lam : 'w. ('a,'w) lam_k -> 'w >
   | App of < m_app : 'w. ('a,'w) app_k -> 'w >
      (* The standard encoding of existentials *)
and ('a,'w) lam_k = 
    {lam_k : 'u 'v. (('u -> 'v),'a) eq * ('u exp -> 'v exp) -> 'w}
and ('a,'w) app_k = 
    {app_k : 'u 'v. ('v, 'a) eq * ('u -> 'v) exp * 'u exp -> 'w};;


(* smart constructors, as syntactic sugar *)
(* Method signatures are, alas, required. *)
let e_int x = Int (object method m_int : 'w. ((int,'a) eq -> int -> 'w) -> 'w
   = fun k -> k (refl ()) x end);;
(* val e_int : int -> int exp *)

let e_inc = Inc (object method m_inc : 'w. ((int->int,'a) eq -> 'w) -> 'w
   = fun k -> k (refl ()) end);;
(* val e_inc : (int -> int) exp *)

let e_lft x = Lft (object method m_lft : 'w. (('a,'a) eq -> 'a -> 'w) -> 'w
   = fun k -> k (refl ()) x end);;
(* val e_lft : 'a -> 'a exp *)

let e_lam f = Lam (object method m_lam : 'w. ('a,'w) lam_k -> 'w
   = fun k -> k.lam_k (refl (),f) end);;
(* val e_lam : ('a exp -> 'b exp) -> ('a -> 'b) exp *)

let e_app f x = App (object method m_app : 'w. ('a,'w) app_k -> 'w 
   = fun k -> k.app_k (refl (),f,x) end);;
(* val e_app : ('a -> 'b) exp -> 'a exp -> 'b exp *)


(* Test expressions *)
(* Note their inferred types given in comments *)
let test1 = e_app e_inc (e_int 1);;
(* val test1 : int exp *)

let test2 = e_lam (fun x -> e_app e_inc (e_app e_inc x));;
(* val test2 : (int -> int) exp *)

let test3 = e_lam (fun f -> e_lam (fun x -> e_app f x));;
(* val test3 : (('_a -> '_b) -> '_a -> '_b) exp *)

let test4 = e_app (e_app test3 test2) (e_int 3);;
(* val test4 : int exp *)

(* The interpreter. We need polymorphic recursion -- as is
   typical with GADTs*)

type eval_sig = {ev: 'a. 'a exp -> 'a};;
let rec eval = {ev = 
 function 
   | Int x -> x#m_int (fun eq x -> apply_eq eq x)
   | Inc x -> x#m_inc (fun eq   -> apply_eq eq succ)
   | Lft x -> x#m_lft (fun eq x -> x)
   | Lam x -> 
      x#m_lam {lam_k = 
	  fun (eq,f) -> apply_eq eq (fun x -> eval.ev (f (e_lft x)))}
   | App x ->
       x#m_app {app_k = 
	  fun (eq,f,x) -> apply_eq eq ((eval.ev f) (eval.ev x))}
};;

let test1_ev = eval.ev test1;; (* int = 2 *)
let test2_ev = eval.ev test2;; (* int -> int = <fun> *)
let test3_ev = eval.ev test3;; (* (int -> int) -> int -> int = <fun> *)
let test4_ev = eval.ev test4;; (* int = 5 *)

(* A different interpreter: counts the number of constructors *)
(* We need a different Lft here, or parameterize the interpreter
   by the type of `hypothetical environment'.
   See the general solution in
      http://okmij.org/ftp/tagless-final/InFin.hs
  This is becoming too complex for an example. We just skip this test.

type size_sig = {sv: 'a. 'a exp -> int};;
let rec size = {sv =
 function 
   | Lam x -> 
       x#m_lam {lam_k = fun (eq,f) -> size.sv (f ???) + 1}
   | App x -> 
       x#m_app {app_k = fun (eq,f,x) -> (size.sv f) + (size.sv x) + 1}
   | _ -> 1
   };;

let test1_sv = size.sv test1;; (* 3 *)
let test2_sv = size.sv test2;; (* 6 *)
let test3_sv = size.sv test3;; (* 5 *)
let test4_sv = size.sv test4;; (* 14 *)
*)



(* ------------------------------------------------------------------------ *)
(* A more optimal implementation of EQ GADT
   It is commented out.

(* Here is the principal component, the kernel of trust.
   Only this module uses Obj.magic. One has to be careful here.
*)
module EQ : sig 
  type ('a,'b) eq
  val refl : ('a,'a) eq
  val symm : ('a,'b) eq -> ('b,'a) eq
  val apply_eq : ('a,'b) eq -> 'a -> 'b
  module EQF1(X:sig type 'a r end) : sig		(* Leibniz principle *)
    val feq : ('a,'b) eq -> ('a X.r, 'b X.r) eq
  end
end = struct
  type ('a,'b) eq = Refl
  let refl = Refl
  let symm Refl = Refl
  let apply_eq Refl = Obj.magic
  module EQF1(X:sig type 'a r end) = struct
    let feq Refl = Refl			(* Just like in Agda *)
  end
end;;
*)
