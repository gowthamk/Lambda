
datatype 'a seq = Nil | Cons of 'a * 'a seq

fun id x = x
and map f Nil = Nil
  | map f (Cons (x,xs)) = Cons (f x, map f xs)


(*
 * Is this typable?
 * How many type arguments of id?  
 *)
fun test () =
  let
    val x' = id "hello"
    val l' = map id (Cons (1,Cons(2,Nil)))
  in
    l'
  end;;

test();;
