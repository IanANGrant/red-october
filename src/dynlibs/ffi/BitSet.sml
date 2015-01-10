signature Enum = sig
   type enum
   val fromWord : word -> enum
   val toWord : enum -> word
   val fromString : string -> enum
   val toString : enum -> string
   val flags : enum list
end

signature BitEnum =
sig
   structure Enum : Enum
   val fromWord : word -> Enum.enum list
   val toWord : Enum.enum list -> word
   val fromString : string -> Enum.enum list
   val toString : Enum.enum list -> string
end

functor BitSet(structure Enum : Enum)
   :> BitEnum
       where type Enum.enum = Enum.enum =
struct
   structure Enum = Enum : Enum
   val toWord =
      let fun iter w [] = w
            | iter w (c::cs) = iter (Word.orb(Enum.toWord c,w)) cs
      in iter 0w0
      end
   val toString =
       let fun iter s [] = s
             | iter s (e::es) =
                 iter (s^(if s = "" then "" else "|")^(Enum.toString e)) es
       in iter ""
       end
   val fromWord =
      let fun iter a [] = (fn w => List.rev a)
            | iter a (c::cs) = fn w =>
                if Word.andb(w,Enum.toWord c) <> 0w0
                   then iter (c::a) cs w
                   else iter a cs w
      in iter [] Enum.flags
      end
   local
         val optws = "[ \\t]*"
         val bar = "|"
         val anchor = "^"
         val sep = optws^bar^optws
         val fieldsre = Regex.regcomp sep []
         val scanflags = Regex.fields fieldsre
         fun getscannedflags s = 
               let val v = scanflags s
               in List.filter (fn s => s <> "") (List.map Substring.string v)
               end
   in
         val fromString = (List.map Enum.fromString) o getscannedflags
   end
end

(*
load "BitSet";
load "IPv4Consts";

structure BitSet = BitSet(structure Enum = IPv4Consts);
val bs1 = BitSet.fromString "DF|RF|MF";
val bs1' = BitSet.toString bs1;

*)
