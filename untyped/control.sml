structure Control:
sig
  val debugPrint : string -> unit
end = 
struct
  val debugFlag = ref false
  val debugPrint = fn str =>
    if !debugFlag then print str else ()
end
