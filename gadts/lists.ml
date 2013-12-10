(********* Empty vs Non-empty list *****)
module List1 = struct
  type empty

  type nonempty

  type (_,_) vec = 
    | Nil: ('a,empty) vec
    | Cons : 'a * ('a,'b) vec -> ('a,nonempty) vec

  (* Head expects its input list to be non-empty as
     its pre-condition 
   *)
  let head : ('a,nonempty) vec -> 'a = function
  (* Due to the specified pre-condition, the following
     match is exhaustive
   *)
    | Cons (x,xs) -> x;;

  (* Pre-condition of head function is statically enforced.
     The following doesn't type-check.  
   *)
  (*let x = root Empty;;*)

  let rec length : type s. ('a,s) vec -> int = function
    | Nil -> 0
    | Cons (x,xs) -> 1 + (length xs);;

end;;

(******** length-indexed *******)
module LengthIndexedList = struct
  type zero
  type 'a succ

  type (_,_) vec =
    | Nil : ('a,zero) vec
    | Cons : 'a * ('a,'b) vec -> ('a,'b succ) vec

  (* map2 requires that its input lists be of same length.
   *)
  let rec map2 : type s. ('a*'b -> 'c) -> ('a,s) vec -> ('b,s) vec
    -> ('c,s) vec = fun f -> fun v1 -> fun v2 -> match (v1,v2) with
        (* Due to pre-condition, the following case-match is
           exhaustive *)
      | (Nil, Nil) -> Nil
      | (Cons(x1,xs1), Cons(x2,xs2)) -> Cons (f(x1,x2), 
        map2 f xs1 xs2);;

  let rec listmap2 f l1 l2 = match (l1,l2) with
    (* Similar case-match with regular lists is non-exhaustive. *)
    (* Compiler throws a warning here. *)
    | ([],[]) -> []
    | (x1::xs1, x2::xs2) -> (f(x1,x2))::(listmap2 f xs1 xs2);;

  (* We reap benifits of strong type of map2 *)
  (* Following code doesn't typecheck *)
  (*
  let l1 = Cons (1, Cons(2, Nil)) in
  let l2 = Cons (3, Cons (2, Cons(1, Nil))) in
    map2 (fun x -> x) l1 l2;;
  *)
end;;
