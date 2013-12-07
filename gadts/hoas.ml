(*
 * HOAS
 *)
module HOAS = struct
  exception Fail
  type _ t = 
    | Lit : 'a -> 'a t
    | Pair : 'a t * 'b t -> ('a * 'b) t
    (* Recall that our primary goal is to use meta language (here,
    ocaml) functions to encode object language binders *)
    | Abs : ('a t -> 'b t) -> ('a -> 'b) t 
    | App : ('a -> 'b) t * 'a t -> 'b t

  (*
   * We can define a (not quite practical) eval function
   * without alpha-conversion and substitution.
   *)
  let rec eval : type s. s t -> s = function
    | Lit x -> x
    | Pair (e1,e2) -> (eval e1, eval e2)
    | Abs f -> (fun x -> eval (f (Lit x)))
    | App (f,e) -> (eval f) (eval e)
  (*
   * HOAS encoding (and its GADT implementation) prevents
   * syntactically incorrect substitutions.
   * Following code attempts
   * [(x1,x2)/\x.x] ((\x.x) y) -~-> (x1,x2) y
   * and promptly fails typecheck.
   *)
  let subst : type s. s t -> 'a t -> s t = (fun e e' -> 
    match e with 
      | App (e1,e2) -> App (e',e2) 
      | _ -> e);;
  let newe = 
    subst (App (Abs (fun x -> x))) (Pair (Lit "x1",Lit "x2"));;
end;;
