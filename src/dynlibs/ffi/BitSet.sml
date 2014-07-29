signature Enum = sig
   type enum
   val fromWord : word -> enum
   val toWord : enum -> word
   val fromString : string -> enum
   val toString : enum -> string
   val flags : enum list
end

signature BitEnum = sig
   include Enum
   val fromWordBits : word -> enum list
   val toWordBits : enum list -> word
end

functor BitSet(structure Enum : Enum) : BitEnum where type enum = Enum.enum =
struct
   open Enum
   val fromWordBits =
      let fun iter a [] = (fn w => List.rev a)
            | iter a (c::cs) = fn w =>
                if Word.andb(w,toWord c) <> 0w0
                   then iter (c::a) cs w
                   else iter a cs w
      in iter [] flags
      end
   val toWordBits =
      let fun iter w [] = w
            | iter w (c::cs) = iter (Word.orb(toWord c,w)) cs
      in iter 0w0
      end
end
