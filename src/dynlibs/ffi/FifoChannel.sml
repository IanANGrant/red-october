signature DuplexChannel =
sig
   type chan
   type msg
   type state
   val open : chan -> state
   val close : state -> unit
   val send : msg * state -> unit
   val receive : state -> msg
end

