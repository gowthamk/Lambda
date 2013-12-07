exception Fail;;

type empty

type nonempty

type (_,_) tree = 
  | Empty : ('a,empty) tree
  | Branch : ('a,'b) tree * 'a * ('a,'b) tree -> ('a,nonempty) tree

let root : ('a,nonempty) tree -> 'a = function
  | Branch (l,n,r) -> n;;

let max x y = if y>x then y else x;;

let rec height : type s. ('a,s) tree -> int = function
  | Empty -> 0
  | Branch (l,_,r) -> 1 + (max (height l) (height r));;

(*let x = root Empty;;*)

(******** length-indexed *******)
type zero
type 'a succ

type (_,_) vec =
  | Nil : ('a,zero) vec
  | Cons : 'a * ('a,'b) vec -> ('a,'b succ) vec

let rec map2 : type s. ('a*'b -> 'c) -> ('a,s) vec -> ('b,s) vec
  -> ('c,s) vec = fun f -> fun v1 -> fun v2 -> match (v1,v2) with
    | (Nil, Nil) -> Nil
    | (Cons(x1,xs1), Cons(x2,xs2)) -> Cons (f(x1,x2), map2 f xs1 xs2);;

let rec listmap2 f l1 l2 = match (l1,l2) with
  | ([],[]) -> []
  | (x1::xs1, x2::xs2) -> (f(x1,x2))::(listmap2 f xs1 xs2);;

let l1 = Cons (1, Cons(2, Nil)) in
let l2 = Cons (3, Cons (2, Cons(1, Nil))) in
  map2 (fun x -> x) l1 l2;;

(******** even-odd **********)

type even
type odd

type (_,_) opp =
  | Even : (odd,even) opp
  | Odd : (even,odd) opp

type _ nat = 
  | Zero : even nat
  | Succ : 'a nat * ('a,'b) opp -> 'b nat

let even_succ (n : even nat) = Succ (n,Odd);;
let odd_succ (n : odd nat) = Succ (n,Even);;

(*
 * If we have a type level function F, we could simply write
 * Succ : 'a nat -> (F 'a) nat
 * Giving haskell promotion?
 *)

(************* Typed printf-scanf **********)
(*
 * TODO : 1. Read variants, rank-2 poly in ocaml
 * 2. Implement empty/non-empty list in ocaml without gadts, but with
 * polymorphic variants
 * Talk : 1. Simple intro to GADTs
 * 2. Examples : empty list, indexed list, even-odd, 
 * 3. A brief deviation to explain Haskell promotions
 * 4. Typed printf/scanf
 * 5. HOAS
 * 6. Simulating GADTs in Ocaml - to demonstrate explicit type 
 * equality proofs.
 *)
