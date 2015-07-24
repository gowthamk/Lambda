
datatype 'a seq = Nil | Cons of 'a * 'a seq

fun id x = x
and map f l = 
  let
    val x' = id 2 
  in
    case l of Nil => Nil
    | Cons (x,xs) => Cons (f x, map f xs)
  end 


fun test () =
  let
    (*
     * The following doesn't typecheck.
     * Due to how id was used in its mutually recursive 
     * definition, its type was inferred to be monomorphic.
     *)
    val x' = id "hello"
    val l' = map id (Cons (1,Cons(2,Nil)))
  in
    l'
  end;;

test();;
