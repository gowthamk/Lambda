(*
 * This example demonstrates module and type equality in 
 * presence of functors.
 *)
module type BAR = 
sig
  type t
end
module type FOO = 
sig
  module Bar : BAR
  type t
  val cast : Bar.t -> t
end

module BarToFoo = functor (B:BAR) -> 
(struct
   module Bar = B
  type t = Bar.t
  let cast x = x
end (*<ann1>*) : FOO (*<ann2>*)with module Bar = B(*</ann2>*) 
                       (*<ann3>*) and type t = B.t (*</ann3>*)  (*</ann1>*))

module OnlyBar = 
(
struct
  type t = int
end : (*<ann4>*)BAR (*<ann5>*)with type t = int(*</ann5>*)(*</ann4>*)
)
module Foo1 = BarToFoo (struct
                          include OnlyBar
                        end)
module Foo2 = BarToFoo (struct
                          include OnlyBar
                        end);;

(* 
 * If ~ann1 /\ ~ann2 /\ ~ann3 /\ ~ann4 /\ ~ann5 then
 *   Typecheck Success
 * If ann1 /\ ~ann2 /\ ~ann3 /\ ~ann4 /\ ~ann5 then
 *   Error: This expression (`1`) has type int but an 
 *   expression was expected of type Foo1.Bar.t.
 *   To make sense of this error, erase the definition of BarFoo,
 *   and treat it as a blackbox accepting B:BAR, and generating 
 *   a module containing Bar:BAR.
 * If ann1 /\ ann2 /\ ~ann3 /\ ~ann4 /\ ~ann5 then
 *   Error: This expression (argument `x`) has type Foo1.t but an 
 *   expression was expected of type Foo2.Bar.t = int.
 * If ann1 /\ ann2 /\ ann3 /\ ~ann4 /\ ~ann5 then
 *   Typecheck Success.
 * If ann1 /\ ann2 /\ ann3 /\ ann4 /\ ~ann5 then
 *   Error: This expression (`1`) has type int but an expression 
 *   was expected of type Foo1.Bar.t = OnlyBar.t.
 * If ann1 /\ ann2 /\ ann3 /\ ann4 /\ ann5 then
 *   Typecheck Success.
 *
 * Q. Why does typecheck succeed in 1st, 4th and 6th cases? 
 * A. In these cases, we know that 
 *    Foo1.Bar.t = OnlyBar.t = int, (hence, Foo1.cast 1 typechecks) 
 *    Foo1.t = OnlyBar.t = int, and (hence, we know x:int)
 *    Foo2.Bar.t = OnlyBar.t = int = Foo1.Bar.t. (hence, Foo2.cast x typechecks)
 *)

let _ = 
  let x = Foo1.cast 1  in
  let y = Foo2.cast x in
    y;;
