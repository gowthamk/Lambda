module type NDet = sig

  (* We will be talking about integer lists, so we need at least
     integer literals
   *)
  type int_t
  val int: int -> int_t
  (* We can add the standard operations on integers, but we don't need
     them for our task here.
     We can always add them later. Remember: extensibility is the
     strong suit of the tagless-final embedding.
  *)

  (* We also need integer lists *)
  type ilist_t
  (* Constructors *)
  val nil:  ilist_t
  val cons: int_t -> ilist_t -> ilist_t
  val list: int list -> ilist_t   (* Not necessary but still convenient *)
  (* Deconstructor. The syntax is ungainly *)
  val decon: ilist_t -> 
    (unit -> ilist_t) ->             (* if nil *)
    (int_t -> ilist_t -> ilist_t) -> (* if cons h t *)
    ilist_t

  (* Although the latter is expressible, it is convenient to have as 
     a primitive *)
  val foldr: (int_t -> ilist_t -> ilist_t) -> ilist_t -> ilist_t -> ilist_t

  (* What, no functions? *)

  (* Finally, we need operations for non-determinism *)
  val fail: ilist_t
  val (|||): ilist_t -> ilist_t -> ilist_t
end


(* We now can write our permutation code *)
module Perm(S:NDet) = struct
  open S

  let rec insert x l = 
    cons x l |||
    decon l
     (fun () -> fail)
     (fun h t -> cons h (insert x t))

 let perm = foldr insert nil

 let test1 = perm (list [1;2;3])
end

(* First Implementation *)
module NDetL = struct
  type int_t = int list
  let int x = [x]

  let concatmap: ('a -> 'b list) -> 'a list -> 'b list =
    fun f l -> List.concat @@ List.map f l

  let liftm2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list =
    fun f l1 l2 -> concatmap (fun x -> List.map (f x) l2) l1

  type ilist_t = int list list
  (* Constructors *)
  let nil = [[]]
  let cons: int_t -> ilist_t -> ilist_t = liftm2 (fun h t -> h::t)
  let list x = [x]

  (* Deconstructor. Syntax is ungainly *)
  let decon: ilist_t -> 
    (unit -> ilist_t) -> (int_t -> ilist_t -> ilist_t) -> ilist_t =
      fun l onnil oncons ->
        concatmap (function
        | []   -> onnil ()
        | h::t -> oncons (int h) [t]) l

  (* Although the latter is expressible, it is convenient to have. *)
  let rec foldr: (int_t -> ilist_t -> ilist_t) -> 
                   ilist_t -> ilist_t -> ilist_t =
    fun f z l ->
      let go = function
        | []   -> z
        | h::t -> f (int h) @@ foldr f z [t]
      in
      concatmap go l

  (* Finally, non-determinism *)
  let fail: ilist_t = []
  let (|||): ilist_t -> ilist_t -> ilist_t = (@)
end


let [[1; 2; 3]; [1; 3; 2]; [2; 1; 3]; [2; 3; 1]; [3; 1; 2]; [3; 2; 1]]
    = let module M = Perm(NDetL) in  M.test1
;;

(* Adding more tests, and showing off the extensibility *)

let (120, 1) =
 let module M = Perm(NDetL) in
 let open M in let open NDetL in
 let test2 = perm (list [1;2;3;4;5]) in
 let test3 = perm nil in
 (List.length test2, List.length test3)
;;

(* Second implementation, using delimcc
   No explicit monads any more
*)

(*
#load "delimcc.cma";;
*)
(* **************
module NDetD = struct
  open Delimcc

  type int_t = unit -> int
  let int x = fun () -> x

  type ilist_t = unit -> int list
  (* Constructors *)
  let nil:  ilist_t = fun () -> []
  let cons: int_t -> ilist_t -> ilist_t = fun h t -> fun () -> h () :: t ()
  let list: int list -> ilist_t = fun l -> fun () -> l
  (* Deconstructor. The syntax is ungainly *)
  let decon: ilist_t -> 
    (unit -> ilist_t) ->             (* if nil *)
    (int_t -> ilist_t -> ilist_t) -> (* if cons h t *)
    ilist_t
    = fun l onnil oncons -> fun () -> 
        match l () with
        | [] -> onnil () ()
        | h::t -> oncons (fun () -> h) (fun () -> t) ()
        
  let rec foldr: 
      (int_t -> ilist_t -> ilist_t) -> ilist_t -> ilist_t -> ilist_t =
    fun f z l -> fun () ->
      match l () with
      | [] -> z ()
      | h::t -> f (fun () -> h) (foldr f z (fun () -> t)) ()

  (* Finally, we need operations for non-determinism *)
  let p = new_prompt ()
  let fail: ilist_t = fun () -> shift0 p (fun _ -> [])
  let (|||): ilist_t -> ilist_t -> ilist_t = fun l1 l2 -> fun () ->
    shift p (fun k -> k l1 @ k l2) ()
  (* what if a non-deterministic expression produces an infinite
     stream of answers?
   *)
  let run : ilist_t -> int list list = fun l ->
    push_prompt p (fun () -> [l ()])
end


let [[1; 2; 3]; [1; 3; 2]; [2; 1; 3]; [2; 3; 1]; [3; 1; 2]; [3; 2; 1]]
    = let module M = Perm(NDetD) in  NDetD.run M.test1
;;
(* The same results, in the same order *)

(* Adding more tests, and showing off the extensibility *)

let (120,1) =
 let module M = Perm(NDetD) in
 let open M in let open NDetD in
 let test2 = run @@ perm (list [1;2;3;4;5]) in
 let test3 = run @@ perm nil in
 (List.length test2, List.length test3)
;;

(*

Questions for the interested reader:
  We said that fold is not actually needed and can be written
  using the other features of the language.
  Write it.

type ilist_t: need nondet in tail

Typically tagless-final presentation features a type 'a repr
that corresponds to a target language phrase at type 'a.
We have avoided it in our presentation. Can you tell what
would be the benefit of using 'a repr?

Generalize the NDet signature using 'a repr and implement it.

Does it make sense to define a separate type for values and expressions
of our DSL? What benefits it may confer?

Add yet another implementation of NDet: e.g., freer monad implementation
*)

(* Committed choice *)
module type NDetComm = sig
  include NDet
  val rId : (int list -> bool) -> ilist_t -> ilist_t (* As in Curry std lib *)
  val once : ilist_t -> ilist_t
end

(* Slow sort: the standard test of non-determinism *)
module Sort(Nd:NDetComm) = struct
  open Nd
  include Perm(Nd)

  let rec sorted = function
    | [] -> true
    | [_] -> true
    | h1 :: h2 :: t -> h1 >= h2 && sorted (h2::t)

  (* _a_ sorted permutation *)
  let sort l = once @@ rId sorted (perm l)

  let tests = sort (list [3;1;4;1;5;9;2])
end

(* Extending the NDetL with committed choice *)
module NDetLComm = struct
  include NDetL
  let rId : (int list -> bool) -> ilist_t -> ilist_t = List.filter
  let once : ilist_t -> ilist_t = function
    | [] -> []
    | h::_ -> [h]
end

let [[9; 5; 4; 3; 2; 1; 1]] = let module M = Sort(NDetLComm) in M.tests
;;


(*
Question: why this slow sort is really slow in our implementation.
Hint: this web site points to an article on this very topic
*)

(*
More examples:
http://www.informatik.uni-kiel.de/~mh/curry/examples/
*)
 ************** *)

(* Lightening the notation *)

(* First, writing perm specialized no one implementation, NDetL *)

let perm : int list -> int list list = fun l ->
  let open NDetL in
  let rec insert x l = 
    cons x l |||
    decon l
     (fun () -> fail)
     (fun h t -> cons h (insert x t))
  in foldr insert nil (list l)


let [[1; 2; 3]; [1; 3; 2]; [2; 1; 3]; [2; 3; 1]; [3; 1; 2]; [3; 2; 1]]
    = perm [1;2;3]

;;

(* Second, abstracting from the implementation *)

(* Add the observation function. Should have added it from the very
   beginning. But it is never too late.
*)
module type NDetO = sig
  include NDet
  val run : ilist_t -> int list list
end

let perm : (module NDetO) -> int list -> int list list = 
  fun (module S:NDetO) l ->
  let open S in
  let rec insert x l = 
    cons x l |||
    decon l
     (fun () -> fail)
     (fun h t -> cons h (insert x t))
  in run @@ foldr insert nil (list l)
;;

let [[1; 2; 3]; [1; 3; 2]; [2; 1; 3]; [2; 3; 1]; [3; 1; 2]; [3; 2; 1]]
    = perm (module struct include NDetL let run x = x end) [1;2;3]

;;

(* **************
let [[1; 2; 3]; [1; 3; 2]; [2; 1; 3]; [2; 3; 1]; [3; 1; 2]; [3; 2; 1]]
    = perm (module NDetD) [1;2;3]

;;


(* Generating code for list permutations *)

open Print_code;;

let genletrec : (('a->'b) code -> 'a code -> 'b code) -> ('a->'b) code = 
  fun f -> genlet .<let rec g x = .~(f .<g>. .<x>.) in g>.

(* Note the error

  Characters 93-116:
    .<let rec g = .~(f .<g>.) in g>.
      ^^^^^^^^^^^^^^^^^^^^^^^
Error: Recursive let binding must be to a function

So we have to statically see that let rec produces something that is
acceptable
*)


module NDetLCode = struct
  type int_t = int list code
  let int x = .<[x]>.

  let concatmap: ('a -> 'b list) code -> 'a list code -> 'b list code =
    fun f l -> .<List.concat @@ List.map .~f .~l>.

  let liftm2 : ('a code -> 'b code -> 'c code) -> 
    'a list code -> 'b list code -> 'c list code =
    fun f l1 l2 -> 
      concatmap .<fun x -> List.map (fun y -> .~(f .<x>. .<y>.)) .~l2>. l1

  type ilist_t = int list list code
  (* Constructors *)
  let nil = .<[[]]>.
  let cons: int_t -> ilist_t -> ilist_t = liftm2 (fun h t -> .<.~h :: .~t>.)
  let list x = .<[x]>.

  type decon_loc = string
  let memtable = 
    ref ([] : (decon_loc * (int list list -> int list list) code) list)

  (* Deconstructor. Syntax is ungainly *)
  let decon: ilist_t -> 
    (unit -> ilist_t) -> (int_t -> ilist_t -> ilist_t) -> ilist_t =
      fun l onnil oncons ->
      let key = "XXX" in                (* for now *)
      match List.assoc key !memtable with
      | deconcode -> .<.~deconcode .~l>.
      | exception Not_found ->
          let fn = genletrec @@ fun fn l ->
            let mtold = !memtable in
            let () = memtable := (key,fn) :: !memtable in
            let r =
              concatmap .<function
              | []   -> .~(onnil ())
              | h::t -> .~(oncons .<[h]>. .<[t]>.)>. l in
            let () = memtable := mtold in
            r
          in .<.~fn .~l>.
  (* Although the latter is expressible, it is convenient to have. *)
  let foldr: (int_t -> ilist_t -> ilist_t) -> 
                   ilist_t -> ilist_t -> ilist_t =
    fun f z l ->
     .<
      let rec go = function
        | []   -> .~z
        | h::t -> let gt = go t in .~(f .<[h]>. .<gt>.)
      in
      .~(concatmap .<go>. l)
     >.

  (* Finally, non-determinism *)
  let fail: ilist_t = .<[]>.
  let (|||): ilist_t -> ilist_t -> ilist_t = 
    fun l1 l2 -> .<.~l1 @ .~l2>.
end

let pcode = let module M = Perm(NDetLCode) in  M.test1;;

(*
val pcode : NDetLCode.ilist_t = .<
  let rec go_75 =
    function
    | [] -> [[]]
    | h_76::t_77 ->
        let lv_87 =
          let rec g_79 x_80 =
            List.concat @@
              (List.map
                 (function
                  | [] -> []
                  | h_81::t_82 ->
                      List.concat @@
                        (List.map
                           (fun x_85  ->
                              List.map (fun y_86  -> x_85 :: y_86)
                                ((List.concat @@
                                    (List.map
                                       (fun x_83  ->
                                          List.map
                                            (fun y_84  -> x_83 :: y_84)
                                            [t_82]) [h_76]))
                                   @ (g_79 [t_82]))) [h_81])) x_80)
             in
          g_79  in
        let gt_78 = go_75 t_77  in
        (List.concat @@
           (List.map
              (fun x_88  -> List.map (fun y_89  -> x_88 :: y_89) gt_78)
              [h_76]))
          @ (lv_87 gt_78)
     in
  List.concat @@ (List.map go_75 [(* CSP x *)])>. 
*)


let [[1; 2; 3]; [1; 3; 2]; [2; 1; 3]; [2; 3; 1]; [3; 1; 2]; [3; 2; 1]]
  = Runcode.run pcode;;

(*
#use "nondet.ml";;
*)
 ************** *)
