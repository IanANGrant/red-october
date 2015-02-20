val dprint = print (* fn _ => ();*)

val SysErrString = fn (s,e) => (s^": "^(SysErr.toString e)^"")

fun mapShm (shmname,npages,address) =
   let val array = ShmMap.shm_create
                     (shmname,
                      [Fcntl.O_RDWR],
                      [Fcntl.S_IRWXU],
                      [MMap.MAP_SHARED],
                      [MMap.PROT_WRITE,MMap.PROT_READ,MMap.PROT_EXEC], npages, address)
   in array
   end     

structure Msg = Event

structure Channel =
  SplitFifoChannel
    (type tx_msg = Msg.tx_msg
     type rx_msg = Msg.rx_msg
     structure Fifo = MappedFifo.Fifo)

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
   in Channel.open_chan endpt (tx_buf,rx_buf)
   end

local open SigAction
in val ignoreSignals =
     fn (sigs) =>
      List.app
       (fn s =>
          ignore 
           (sigaction
            (s,
            {sa_flags = [],
             sa_mask = SigSet.sigset [],
             sa_handler = SIG_IGN}))) sigs
   fun blockSignals sigs = SigSet.sigprocmask
                          (SigSet.SIG_BLOCK,
                             SOME (SigSet.sigset (sigs)))
   fun unBlockSignals sigs = SigSet.sigprocmask
                          (SigSet.SIG_UNBLOCK,
                             SOME (SigSet.sigset (sigs)))
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

local
   val bufStr : Word8Vector.vector -> string = Obj.magic 
   val strBuf : string -> Word8Vector.vector = Obj.magic 

   val bufsize = 1024
   val buf = Word8Vector.tabulate (bufsize,fn _ => 0w0)
   fun readBuf buf n =
     fn fd => 
       let val nofds = FDSet.fromList[]
           val (nfds,rfds,_,_) =
               PSelect.pselect
                   {maxfd = 1 + FDSet.toInt fd,
                    rfds = FDSet.fromList[fd],
                    wfds = nofds,
                    efds = nofds,
                    tout = TimeSpec.fromSeconds 0,
                    ss = SigSet.sigset []}
           val nr = case nfds
                      of 1 => Socket.readVec
                                (fd, {buf = buf,
                                      ofs = 0,
                                      size = SOME n})
                       | _ => 0
       in nr
       end
in 
   fun readStr fd =
      let val nr = readBuf buf bufsize fd
      in String.substring(bufStr buf,0,nr)
      end
   fun writeStr fd s =
      (Socket.writeVec
        (fd, {buf = strBuf s,
              ofs = 0,
              size = NONE}))
end

local
   val nofds = FDSet.fromList[]
in
   fun waitOnRead (fd,ss,tout) =
      let val (nfds,rfds,_,_) =
               PSelect.pselect 
                   {maxfd = 1 + FDSet.toInt fd,
                    rfds = FDSet.fromList[fd],
                    wfds = nofds,
                    efds = nofds,
                    tout = tout,
                    ss = ss}
      in case nfds
           of 1 => (case readStr fd
                      of "" => NONE
                       | s => SOME s)
            | _ => NONE
      end
end

datatype loop_flag =
   SIGNALS
 | CHLD
 | IOWAIT
 | STOP

fun startProcess (shmname,npages) =
   let val array = mapShm (shmname,npages,0w0)
                       handle SysErr.SysErr p =>
                                   raise Fail ("testcmd_loader: mapShm: SysErr: "^(SysErrString p))
       val _ = dprint ("array size = "^(Int.toString(MappedWord8Array.length array))^"\n")
       val chan = openChannel (Channel.End0,array,0,8,128,256,7,7)
       val _ = dprint ("channel is open\nStarting child process ...\n")
       val (txfifo,rxfifo) = Channel.get_buffs chan
       val hndlrs = SigHandler.sigaction 2
       val address = Word.toString(ValRepr.wordFromCptr (MappedWord8Array.get_cptr array))
       val _ = dprint ("Address: "^(address)^"\n")
       val usr1_idx = 0
       val chld_idx = 1
       val sigs = [Signal.usr1, Signal.chld]
       val _ = ignoreSignals sigs
       val oldss = unBlockSignals sigs
       val mask = SigSet.sigprocmask
                     (SigSet.SIG_BLOCK,
                      SOME (SigSet.sigset sigs))
       val _ = setSigaction hndlrs (Signal.usr1,usr1_idx)
       val _ = setSigaction hndlrs (Signal.chld,chld_idx)
       val proc = Unix.execute ("./testcmd", [shmname, (Int.toString npages), address])
       val cpid = Word.fromInt (Unix.pid proc)
       val (infd,_) = Unix.unixFDsOf proc
       fun process_input s = dprint (">> "^s^"\n")
       fun event_loop STOP =
             (dprint ("testcmd_loader: STOP\n");
              (if not (OS.Process.isSuccess(Unix.reap proc))
                     then dprint ("child failed\n")
                     else ();
               restore_sigacts();
               ignore (SigSet.sigprocmask
                         (SigSet.SIG_SETMASK,
                            SOME(oldss)))))
         | event_loop IOWAIT =
             (dprint ("testcmd_loader: IOWAIT\n");
              event_loop 
                  (case waitOnRead (infd,mask,TimeSpec.fromMilliseconds 1000)
                     of SOME s => (process_input s; IOWAIT)
                      | NONE => SIGNALS))
         | event_loop SIGNALS =
             (dprint ("testcmd_loader: SIGNALS\n");
              event_loop
                  (if SigHandler.isset_sig (hndlrs,usr1_idx)
                      then (SigAction.sigqueue (cpid, Signal.usr2,
                                                ValRepr.longFromWord (0wx2a));
                            SigHandler.reset_sig (hndlrs,usr1_idx);
                            dprint ("testcmd_loader: reset sig1 handler\n");
                            let val avail = MappedFifo.Fifo.avail rxfifo
                                val strb = if avail > 0
                                              then Event.toString (Channel.receive chan)
                                              else "<zip>"
                            in dprint ("testcmd_loader: received event: "^strb^"\n")
                            end;
                            CHLD)
                      else CHLD))
         | event_loop CHLD =
             (dprint ("testcmd_loader: CHLD\n");
              event_loop
                  (if SigHandler.isset_sig (hndlrs,chld_idx)
                      then let val code = Option.valOf(SigHandler.si_code(hndlrs,chld_idx))
                           in if code = SigAction.CLD_EXITED
                                 then STOP
                                 else (SigHandler.reset_sig (hndlrs,chld_idx);
                                       IOWAIT)
                           end
                      else IOWAIT))
   in event_loop IOWAIT
   end

fun main () =
   case CommandLine.arguments ()
     of [shmname,npages] =>
        (dprint ("shmname = "^shmname^
                ", npages = "^npages^"\n");
        let val npages =
           case Int.fromString npages
             of SOME n => n
              | NONE => (print ("npages is invalid: "^npages^"\n");
                         Process.exit Process.failure)
        in startProcess (shmname,npages);
           Process.exit Process.success
        end handle Fail s => print ("startProcess failed: "^s^"\n")) 
      | _ =>(print "usage: testcmd_loader /shmname npages\n";
             Process.exit Process.failure)

val () = main ();
