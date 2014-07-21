signature Globals = 
sig
   val debug : bool
   val quiet : bool
   val ntests : int
end

structure Globals :> Globals =
struct
   val debug = false
   val quiet = true
   val ntests = 64 (* e.g. ntests = 64 Gives 64 * 64 = 4096 pairs *)
end
