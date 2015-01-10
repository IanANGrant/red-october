signature DuplexChannel =
sig
   type chan
   type state
   type tx_msg
   type rx_msg
   val open_chan : chan -> state
   val close_chan : state -> unit
   val send : tx_msg * state -> unit
   val receive : state -> rx_msg
   val get_buffs : state -> chan
end

functor SplitFifoChannel
  (type tx_msg
   type rx_msg
   structure Fifo : SplitFifo
      where type elem = Word8.word
      and type vector = Word8Vector.vector)
  :> DuplexChannel
        where type tx_msg = tx_msg
          and type rx_msg = rx_msg
          and type chan = Fifo.fifo * Fifo.fifo =
struct
   type chan = Fifo.fifo * Fifo.fifo
   type tx_msg = tx_msg
   type rx_msg = rx_msg
   type state = { tx_buf : Fifo.fifo,
                  rx_buf : Fifo.fifo,
                  is_open : bool ref }
   local
      structure ObjRepr =
         ObjRepr
           (type state = Fifo.fifo
            val readByte = Fifo.readByte
            val writeByte = Fifo.writeByte)
      fun open_chan (tx_buf : Fifo.fifo,rx_buf : Fifo.fifo) =
         {is_open = ref true, tx_buf = tx_buf, rx_buf = rx_buf} : state
      fun get_buffs ({is_open = ref isopn, tx_buf = tx_buf, rx_buf = rx_buf} : state) =
         if isopn then (tx_buf : Fifo.fifo,rx_buf : Fifo.fifo) else raise Fail "Channel is not open."
      fun close_chan (state:state) =
         (#is_open state) := false
      fun send (msg : tx_msg,state:state) =
         if !(#is_open state)
            then ignore (ObjRepr.encode (msg,#tx_buf state))
            else raise Fail "DuplexChannel: send: channel is closed"
      fun receive (state:state) =
         if !(#is_open state)
            then ObjRepr.decode (#rx_buf state)
            else raise Fail "DuplexChannel: receive: channel is closed"
   in
      val open_chan : chan -> state
        = open_chan
      val get_buffs : state -> chan 
        = get_buffs
      val close_chan : state -> unit
        = close_chan
      val send : tx_msg * state -> unit
        = send
      val receive : state -> rx_msg
        = receive
   end
end
