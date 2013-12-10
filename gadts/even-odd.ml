(******** even-odd **********)
module EvenOdd = struct
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
   * Succ : 'a nat -> (F 'a) nat, where 
   * F even = odd, and F odd = even.
   * Ocaml doesn't allow that. We, therefore, move to Haskell.
   *)
end;;
