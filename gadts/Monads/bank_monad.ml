(*
 * Ignore the code under List module below. My OCaml stdlib is not up
 * to date, so I have to write few List library functions myself.
 *)
module List = struct
  include List

  let rec fold_left_map (f:('a -> 'b -> ('a *'c))) (a:'a) (l:'b list) : ('a * 'c list) = match l with
  | [] -> (a,[])
  | b::bs -> 
      let (a',b') = f a b in
      let (a'',bs') = fold_left_map f a' bs in
      (a'',b'::bs')
end


(*
 * Start reading from here.
 *)
module BankMonad: sig
  (* 
   * Here we are defining a bank monad. Every monad hides a state, and
   * the monad type denotes the computation over that state. Our bank
   * monad hides the bank state, which we later define as a mapping
   * from acc_nos to bals (implemented as a list). It is very
   * important to note that the monad type is NOT the type of the
   * hidden state; rather it is the type of the COMPUTATION over that
   * hidden state. A monad type is usually polymorphic (i.e., it has a
   * type variable) because the result of a computation can be
   * anything.*)
  type 'a t
  (*
   * add_amt takes the acc_no and amt, and returns a computation on
   * hidden bank state. The computation, when executed on any bank
   * state, returns a unit (since deposits always suceed).
   *)
  val add_amt: int -> int -> unit t
  (*
   * sub_amt is like add_amt: it too takes acc_no and amt, and returns
   * a computation on the hidden bank state. However, the computation
   * is expected to subtract amt if and only if there is enough
   * balance, so the computation, when executed, returns bool denoting
   * if the subtraction was made or not.
   *)
  val sub_amt: int -> int -> bool t
  (*
   * get_amt takes acc_no and returns a computation on a hidden bank
   * state. The computation, when run, returns an integer denoting the
   * current balance in the bank account.
   *)
  val get_amt: int -> int t
  (*
   * bind lets us compose two computations on a hidden bank state. The
   * first computation can be run on any bank state, and returns a
   * value of type 'a. The second computation on the other hand
   * expects a value of type 'a (i.e., result of first computation).
   * Hence we denote it as a function that takes 'a and returns a 'b
   * t. The result of bind is a new computation on bank state that
   * returns a 'b (i.e., the result of second computation.)
   *)
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  (*
   * Return lets us "lift" normal values into monad values. Given a
   * value of type 'a, you can always create a bank computation that
   * returns the same value. The computation will simply not make any
   * change to the bank state, and return the given value of type 'a.
   *)
  val return : 'a -> 'a t
  (*
   * The above functions only let you build and compose computations.
   * How are you going to execute the computation? This is why we
   * provide a function run_against_initial_state that takes a bank
   * computation ('a t), runs it against an "initial state", and
   * returns the result ('a). You can define initial bank state to be
   * anything; see below for our definition.
   *)
  val run_against_initial_state : 'a t -> 'a
end = 
(*
 * Let us now define an implementation for the above signature.
 *)
struct
  (*
   * We now define bank state as a mapping from acc_nos to bals
   * implemented simply as a list. Let me emphasize again that this is
   * NOT a monad; this is simply (type of) the state that we hide
   * behind a monad. A monad is always a computation (commit this to
   * memory).
   *)
  type st = (int*int) list
  (*
   * Below we define the bank monad. Bank monad is a computation on
   * bank state, so it is a function that takes a bank state, and
   * returns a new bank state. Along with the new bank state, it also
   * returns thew result of the computation, which can be anything
   * (hence the type 'a).
   *)
  type 'a t = st -> (st*'a)

  (*
   * add_amt takes acc_no and amt, and returns a bank monad
   * computation. What is a computation? It is a function, hence the
   * body of add_amt starts wth keyword fun.
   *)
  let add_amt acc_no amt = fun st ->
    (*
     * We define new bank state as same as old bank state, except
     * for when account number = acc_no, bal becomes bal+amt. We
     * implement this using List.map function (find out its type, and
     * convince yourself the following typechecks).
     * https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
     *)
    let st' = List.map (fun (a,b) -> if a=acc_no 
                          then (a,b+amt) else (a,b)) st in
    (st',())

  let sub_amt acc_no amt = fun st ->
    (*
     * Like add_amt, we map old bank state to new bank state, but we
     * also carry a boolean value (res) as we do so. The bool value
     * res denotes whether we have (so far) been successful in making
     * the subtraction. We start with the initial value false because
     * we haven't made the subtraction in the beginning. We return
     * true whenever we encounter the required account with sufficient
     * balance.
     *)
    let (res, st') = List.fold_left_map (fun (res:bool) ((a:int),(b:int)) -> 
                      (*
                       * Here we are folding (and mapping) from left
                       * to right in the list. res is the result for
                       * the left part of the list (i.e., if the
                       * subtraction was made in the left part of the
                       * list). (a,b) is the current element in the
                       * list. Once (a,b) is processed, fold_left_map
                       * goes over the next element (to the right) in
                       * the list.
                       *)
                      if a=acc_no && b>=amt then (true, (a,b-amt)) 
                      else (res, (a,b))) false st in
    (st', res)

  (*
   * get_amt also returns a computation, hence the body is a fun.
   *)
  let get_amt acc_no = fun st ->
    (*
     * See the type of List.assoc at
     * https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
     *)
    let res = List.assoc acc_no st in
    (*
     * Since get_amt is not supposed to change the bank state, we
     * return the st unchanged.
     *)
    (st,res)

  (*
   * bind and return are boilerplate
   *)
  let bind m1 f = fun st ->
    let (st', res1) = m1 st in
    let m2 = f res1 in
    let (st'', res2) = m2 st' in
    (st'', res2)

  let return x = (fun st -> (st,x)) 

  (*
   * Let's define initial state as containing three bank accounts with
   * ids 1, 2, and 3, each having $100.
   *)

  let init_state = [(1,100); (2,100); (3,100)]
  
  (*
   * Now, we can easily define run_against_initial_state:
   *)
  let run_against_initial_state m = 
    let (final_state, res) = m init_state in
    (*
     * If you want to print final state, print it here. Else ignore
     * final_state to avoid warnings of unused variable.
     *)
    let _ = ignore final_state in
    res
end


(*
 * Now let's play around with BankMonad.
 *)
open BankMonad


(*
 * Let's define a convenient infix operator for bind.
 *)
let (>>=) m f = bind m f


let m1 = add_amt 2 50 
  >>= fun _ -> sub_amt 1 50 
  >>= fun res -> (if res then sub_amt 3 25 else return false) (* note
  the use of return here. We are ignoring res, yet want to do bank
  computation, so we use return.*)
  >>= fun res -> return @@ Printf.printf "Result of m1 computation is %b\n" res;;

(*
 * We have only created the m1 computation, but haven't executed it.
 * Therefore the above print statement wouldn't have been executed.
 *)

Printf.printf "This must be the first print stmt you see\n";;

(*
 * Now let's execute m1 against initial bank state.
 *)

run_against_initial_state m1;;

Printf.printf "Has the print stmt inside m1 executed?\n";;

