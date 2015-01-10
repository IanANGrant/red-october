fun mapShm (shmname,npages,address) =
  ShmMap.shm_open 
    (shmname,
     [Fcntl.O_RDWR],
     [Fcntl.S_IRWXU],
     [MMap.MAP_SHARED,MMap.MAP_FIXED],
     [MMap.PROT_READ,MMap.PROT_WRITE,MMap.PROT_EXEC],
     npages,address)

structure Msg = Event

structure Channel =
  SplitFifoChannel
    (type tx_msg = Msg.tx_msg
     type rx_msg = Msg.rx_msg
     structure Fifo = MappedFifo.Fifo)


datatype endpt = End0 | End1

fun openChannel 
        (endpt, array,
         txregoffs,rxregoffs,
         txbufoffs,rxbufoffs,
         txbufsize,rxbufsize) =
   let val slice = MappedWord8ArraySlice.slice
       val txregslc = slice (array,txregoffs,SOME (ValRepr.wordBytes * 2))
       val rxregslc = slice (array,rxregoffs,SOME (ValRepr.wordBytes * 2))
       fun bufsize bits = Word.toInt (Word.<<(0wx1,Word.fromInt bits))
       val txbufslc = slice (array,txbufoffs,SOME (bufsize(txbufsize)))
       val rxbufslc = slice (array,rxbufoffs,SOME (bufsize(rxbufsize)))
       val tx_buf = MappedFifo.create(txbufslc,txregslc)
       val rx_buf = MappedFifo.create(rxbufslc,rxregslc)
   in Channel.open_chan (case endpt of End0 => (tx_buf,rx_buf) | End1 => (rx_buf,tx_buf))
   end

local open SigAction
in
   val ignoreSignals =
     fn (sigs) =>
         List.app
          (fn s =>
             ignore 
              (sigaction
               (s,
               {sa_flags = [],
                sa_mask = SigSet.sigset [],
                sa_handler = SIG_IGN}))) sigs

   fun unBlockSignals sigs = SigSet.sigprocmask
                             (SigSet.SIG_UNBLOCK,
                                SOME(SigSet.sigset (sigs)));
   fun blockSignals sigs = SigSet.sigprocmask
                             (SigSet.SIG_BLOCK,
                                SOME(SigSet.sigset (sigs)));
end

local open SigAction
   val restore = ref [] :
      (Signal.signal *
       SigAction.sigaction) list ref
in
   fun push_sigact ssa =
      restore := (ssa::(!restore))
   fun restore_sigacts () =
      let fun iter [] = ()
            | iter ((signal,oldact)::rest) =
                      (sigaction (signal,oldact);
                       iter rest)
      in iter (!restore)
      end
   fun setSigaction hndlrs (s,n) =
         push_sigact (s, 
                 sigaction
                   (s,
                   {sa_flags = [SA_SIGINFO],
                    sa_mask = SigSet.sigset [],
                    sa_handler = SA_sigaction
                                     (SigHandler.get_cptr
                                     (hndlrs,n))}))
end

fun startProcess (shmname,npages,reqaddress) =
   let val pagesbytes = MMap.PAGE_SIZE * (Word.fromInt npages)
       val (array,array') = mapShm (shmname,npages,reqaddress)
       val address = ValRepr.wordFromCptr(MappedWord8Array.get_cptr array)
       val address' = ValRepr.wordFromCptr(MappedWord8Array.get_cptr array')
       val addroffs = address - reqaddress
       val addroffs' = address' - reqaddress - pagesbytes
       val chan = openChannel (End1,array,0,8,128,256,7,7)
                    handle SysErr.SysErr (s,err)
                        => raise Fail ("SysErr: "^s^"SysErr.toString err")
       val _ = print "channel is open\n"
       val (txfifo,rxfifo) = Channel.get_buffs chan
       val _ = Channel.send (Event.String ("test","Hello, World."),chan)
                       handle Size => print "Channel.send failed with Size\n"
       val avail = MappedFifo.Fifo.avail txfifo
       val _ = print ("sent: avail = "^(Int.toString avail)^"\n")
    (* val _ = MMap.msync(MappedWord8Array.get_cptr array,pagesbytes,[MMap.MS_INVALIDATE,MMap.MS_SYNC]) *)
       val misc_hndlrs = SigHandler.sigaction 2
       val usr1_idx = 0
       val usr2_idx = 1
       val misc_sigs = [Signal.usr1, Signal.usr2]
       val oldss = blockSignals misc_sigs
       val _ = setSigaction misc_hndlrs (Signal.usr1,usr1_idx)
       val _ = setSigaction misc_hndlrs (Signal.usr2,usr2_idx)
       val ppid = SigAction.getppid ()
       val _ = SigAction.sigqueue (ppid, Signal.usr1, ValRepr.longFromWord (address'))
   in while
         case SigHandler.sigtimedwait (SigSet.sigset misc_sigs) (TimeSpec.fromMilliseconds 500)
           of NONE => false
            | SOME (signo,si) => not (signo = Signal.usr2)
      do ();
      restore_sigacts();
      ignore (SigSet.sigprocmask
               (SigSet.SIG_SETMASK,
                  SOME(oldss)));
      Process.success
   end

fun main () =
   case CommandLine.arguments ()
     of [shmname,pages,address] =>
         let val npages =
               case Int.fromString pages
                 of SOME n => n
                  | NONE => (print ("npages is invalid: "^pages^"\n");
                             Process.exit Process.failure)
             val address =
               case Word.fromString address
                 of SOME n => n
                  | NONE => (print ("address is invalid: "^address^"\n");
                             Process.exit Process.failure)
             val _ = print ("testcmd: Address is: "^(Word.toString address)^"\n")
             val rv = startProcess(shmname,npages,address)
         in Process.exit rv
         end
      | _ => (print "usage: testcmd /shmname npages\n";
              Process.exit Process.failure)

val () = main ();
