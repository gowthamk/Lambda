(*
 * The traditional way to express the print format.
 * This approach couples format with objects being printed.
 * Consequently, 1. It can be used only as a print format descriptor.
 * Scanf requires a different format datatype,
 * 2. Different format objects are required to print different set of 
 * values, even though they share the same format.
 * 
 *)
module SimpleFmt = struct

type fmt = 
  | FStr of string
  | FInt of int
  | FBool of bool
  | FSeq of fmt list

let fmt1 = FSeq [FStr "The values are ";
                 FInt 2;
                 FStr " and ";
                 FBool true;
                 FStr "\n"];;
let rec sprintf : fmt -> string = function
  | FStr str -> str
  | FInt i -> string_of_int i
  | FBool b -> string_of_bool b
  | FSeq fmts -> List.fold_right (fun fmt acc -> (sprintf fmt)^acc)
      fmts "";;

let _ = print_string (sprintf fmt1);;

end;;
(*
 * Type-safe sprintf, scanf.
 * Uses GADTs
 * Version 0.
 *)
module TypedPrintf0 = struct
  type _ fmt = 
    (* Strings are static. They are part of the format.*)
    | FStr : string -> string fmt
    (* FInt(Fbool) is our %d (%b) *)
    | FInt : int fmt
    | FBool : bool fmt
    (* Lists are homogenous. We wish to combine 'a fmt and 'b fmt.
       So we combine two fmts at a time. *)
    | FComb : 'a fmt * 'b fmt -> ('a*'b) fmt

  (* lets try constructing a format descriptor *)
  let myfmt =
    let init : string fmt = Fstr "\n" in
    let bfmt : (bool * string) fmt = FComb (FBool, init) in
    let bfmt' : (string * (bool * string)) fmt = 
      FComb (FStr " and ", bfmt) in
    let ifmt : (int * (string * (bool * string))) fmt = 
      FComb (FInt, bfmt') in
    let myfmt : (string * (int * (string * (bool * string)))) fmt =
      FComb (FStr "The values are ", ifmt) in
    myfmt;;
   (*
    * Unfortunately, the type 
    * (string * (int * (string * (bool * string)))) is rather large
    * and not what we intended, as sprintf function with type
    * sprintf : s fmt -> (s -> string)
    * is hardly useful in this case.
    *)

end;;
(*
 * Type-safe sprintf, scanf.
 * Uses GADTs
 * Version 1.
 *)
module TypedPrintf1 = struct
  type _ fmt = 
    | FEmpty : unit fmt
    | FStatic : string * 'a fmt -> 'a fmt
    | FInt : int fmt
    | FBool : bool fmt
    | FComb : 'a fmt * 'b fmt -> ('a * 'b) fmt

  let myfmt : (int * (bool * unit)) fmt =
    let init : unit fmt = FStatic ("\n",FEmpty) in
    let bfmt : (bool * unit) fmt = FComb (FBool, init) in
    let bfmt' : (bool * unit) fmt= FStatic (" and ",bfmt) in
    let ifmt : (int * (bool * unit)) fmt = FComb (FInt, bfmt') in
      FStatic ("TypedPrintf1 : The values are ", ifmt) ;;

  let rec sprintf : type s . s fmt -> s -> string = function
    | FEmpty -> (fun () -> "")
    | FStatic (s,afmt) -> (fun a -> s ^ (sprintf afmt a))
    | FInt -> (fun i -> string_of_int i)
    | FBool -> (fun b -> string_of_bool b)
    | FComb (afmt,bfmt) -> (fun (a,b) -> (sprintf afmt a)
        ^ (sprintf bfmt b));;

  let _ = print_string (sprintf myfmt (2,(true,())));; (* meh! *)

end;;
(*
 * Type-safe sprintf, scanf.
 * Uses GADTs
 * Version 2.
 *)
module TypedPrintf2 = struct
  type _ fmt = 
    | FEmpty : string fmt
    | FInt : 'a fmt -> (int -> 'a) fmt
    | FBool : 'a fmt -> (bool -> 'a) fmt
    | FStatic : string * 'a fmt -> 'a fmt

  let myfmt : (int -> bool -> string) fmt =
    let init : string fmt = FStatic ("\n",FEmpty) in
    let bfmt : (bool -> string) fmt = FBool init in
    let bfmt' : (bool -> string) fmt = FStatic (" and ",bfmt) in
    let ifmt : (int -> bool -> string) fmt = FInt bfmt' in
      FStatic ("TypedPrintf2 : The values are ", ifmt) ;;
  (*
   * Failed attempt at sprintf
   *)
  (*
  let rec sprintf : type s . s fmt -> s = function
    | FEmpty -> (fun s -> s)
    | FInt suf -> (fun i -> (* recursive call returns 'a -> string fn.
        In order to construct an expression of type int -> 'a -> string, 
        we need to make a tail-recursive call. *))
  *)

  let sprintf : type s. s fmt -> s = 
    let rec helper : type s. s fmt -> string -> s = fun fmt str ->
      match fmt with
      | FEmpty -> str
      | FInt suf -> (fun i -> helper suf (str^(string_of_int i)))
      | FBool suf -> (fun b -> helper suf (str^(string_of_bool b)))
      | FStatic (stat,suf) -> helper suf (str^stat) in
    fun fmt -> helper fmt "";;

  let _ = print_string (sprintf myfmt 2 true);;
end;;
