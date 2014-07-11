prim_val fromCA : CharArray.array -> string ref = 1 "identity";

(* Caml Light "channels" *)

prim_type in_channel;
prim_val fast_input : in_channel -> string -> int -> int -> int = 4 "input";

fun input (is : TextIO.instream) (buff : CharArray.array) offs len =
   let val ref {closed : bool, ic : in_channel, name : string} = Obj.magic is
   in if closed then
           0
         else
            let val ref sbuff = fromCA buff
            in if len < 0 orelse offs < 0 orelse offs+len > size sbuff
                  then
                     raise Fail "Unix.input"
                  else
                     fast_input ic sbuff offs len
            end
   end;
