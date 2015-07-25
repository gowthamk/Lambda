(*
 * This example demonstrates how "with type" construct can be useful.
 * In particular, it shows that it is not necessary to define a functor 
 * type to define the interface of the structure computed by the functor.
 * We can define a simple module type, and then use "with type" construct
 * to perform the required substitution.
 *)
module type ORDERED_KEY = sig
  type t
  val compare : t -> t -> int
end
module type ASSOC_MAP = sig 
  type key 
  type 'value t
  val empty_map : 'value t
  val add_entry : key -> 'value -> 'value t -> 'value t
  val lookup : key -> 'value t -> 'value
end
module TreeMap (Key : ORDERED_KEY) : ASSOC_MAP (*<annot1>*)with type key = Key.t (*</annot1>*) = 
struct 
  type key = Key.t
  type 'a t = 
    | Empty
    | Leaf of Key.t * 'a * 'a t * 'a t
                
  let empty_map = Empty
  let rec add_entry key value t = match t with 
    | Empty -> Leaf (key,value,Empty,Empty)
    | Leaf (k,v,t1,t2) ->
      if (Key.compare key k = 0) then t
      else 
        (if (Key.compare key k < 0) then
           Leaf (key,v,add_entry key value t1, t2)
         else 
           Leaf (key,v,t1,add_entry key value t2))
        
  let rec lookup key = function
    | Empty -> failwith "no matching key"
    | Leaf (k,v,t1,t2) ->
      if (Key.compare k key = 0) then v else
        (lookup key (if (Key.compare key k < 0) then t1 else t2))
end
module IntPairKey : ORDERED_KEY with type t = int*int = struct
  type t = int * int
  let compare (x1,y1) (x2,y2) =
    if x1 >= x2 then y1 - y2
    else - (y2 - y1)
end

module TM = TreeMap(IntPairKey)

(* If annot1 wasn't written, then the following code will throw
 * this Error: This expression has type 'a * 'b
 *        but an expression was expected of type
 *                 TM.key = TreeMap(IntPairKey).key
 *)
let () = ignore @@ TM.add_entry (1,2) "hello" TM.empty_map;;
