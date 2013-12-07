type _ t = 
    | IntLit : int -> int t
    | BoolLit : bool -> bool t
    | Pair : 'a t * 'b t -> ('a * 'b) t
    | App : ('a -> 'b) t * 'a t -> 'b t
    | Abs : ('a -> 'b) -> ('a -> 'b) t 

(* ('a,'b) App : ('a -> 'b) t * 'a t -> 'b t*)

let rec g : type s . s t -> s = function
  | IntLit a -> a
  | BoolLit b -> b
  | Pair (c,d) -> (g c, g d)
  | App (ft,x) ->  g ft (g x)
  | Abs f -> f

(*
 * Type annotation on last line is needed.
 * Doesn't typecheck otherwise.
 * Possible reason : failure of type propagation.
 *)
let rec g3 : 'a . 'a t -> 'a =
  fun (type s) -> ((function
  | IntLit a -> a
  | BoolLit b -> b
  | Pair (c,d) -> (g3 c, g3 d)
  | App (ft,x) ->  g3 ft (g3 x)
  | Abs f -> f) : s t -> s)

(*
 * following does not typecheck (BoolLit case, characters 16-17):
 * Error: This expression has type int but an expression 
 * was expected of type s t -> s
 * Possible reason : GADT in argument not being assigned
 * polymorphic type.
 *)
let rec g2 : 'a . 'a t -> 'a =
  fun (type s) (x : s t) -> ((match x with
  | IntLit a -> a
  | BoolLit b -> b
  | Pair (c,d) -> (g2 c, g2 d)
  | App (ft,x) ->  g2 ft (g2 x)
  | Abs f -> f) : s t -> s)

(*
 * Following doesn't typecheck as well (Pair case).
 * Error: This expression has type ex#10 t
 *   but an expression was expected of type ex#10 t
 *   The type constructor ex#10 would escape its scope
 * Possible reason : Polymorphic recursion not being
 * recognized.
 *)
let rec g' (type s) (x : s t) : s = match x with
  | IntLit a -> a
  | BoolLit b -> b
  | Pair (c,d) -> (g' c, g' d)
  | App (ft,x) ->  g' ft (g' x)
  | Abs f -> f

(* Following fails exhaustiveness check *)
let f x (*(x: int t) : int*) = (match x with
  | IntLit y -> y)

