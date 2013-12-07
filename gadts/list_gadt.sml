structure EQ: 
  sig
    type ('a,'b) eq
    val refl : unit -> ('a,'a) eq
    val symm : ('a,'b) eq -> ('b,'a) eq
  end =
struct
  type ('a,'b) eq = Refl of 'a option * 'b option
  val refl = fn _ => Refl (None,None)
  val symm = fn (Refl (x,y)) = Refl (y,x)
end

datatype empty = Empty
datatype nonempty = Nonempty

datatype ('a,'b) list = Nil | Cons of 'c. 'a * ('a,'c) list * 
                                  ('b,nonempty) eq
