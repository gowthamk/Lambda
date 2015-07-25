module type VAR = 
sig
  type t
end
(*
 * This example demonstrates how "with type" construct can be useful.
 * In particular, it shows that it is not necessary to define a functor 
 * type to define the interface of the structure computed by the functor.
 * We can define a simple module type, and then use "with type" construct
 * to perform the required substitution.
 * In a core-ml program:
 *   type 'el list = Nil | Cons of 'el * 'el list
 *   val nil : 'el list 
 *   val cons : 'el -> 'el list -> 'el list
 * An analogue in ML's module language would be:
 *)
module type LIST =
 sig
   type el
   type t
   val nil : t
   val cons : el -> t -> t
 end
 module Listify (Var:VAR) : LIST with type el = Var.t =
 struct
   type el = Var.t
   type t = el list
   let nil = []
   let cons (x:Var.t) (xs:Var.t list) = x::xs
 end
