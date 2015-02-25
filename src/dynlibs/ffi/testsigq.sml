val _ = List.app load
   ["MemDebug",    "Vector",               "PolyRedBlackMap",
    "Array",       "ArraySlice",           "RealRepr",
    "SigQFifo",    "GenericArray",         "GenericArraySlice",
    "ObjRepr",     "MappedNativeRegister", "DoubleMappedWord8Array",
    "SigAction",   "SigHandler",           "PSelect",
    "MappedNativeWordRegister"];

structure DMappedWord8Array :> GenericArray
   where type array = MappedWord8Array.array
     and type elem = Word8.word
     and type vector = Word8Vector.vector =
struct
   open MappedWord8Array
   val array = DoubleMappedWord8Array.array
end

structure WordStruct
   :> GenericWord
        where type largeword = Word.word
          and type word = Word.word =
struct
   type largeword = Word.word
   open Word
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
   val nofds = FDSet.fromList[]
in
   fun waitOnSigs (reg,value,hndlrs,sigs,sigidx,ss,tout) =
      let fun iter () =
         let val _ = blockSignals sigs
         in if MappedNativeWordRegister.! reg >= value
                then ()
                else
                  (ignore 
                    (PSelect.pselect 
                     {maxfd = 0,
                      rfds = nofds,
                      wfds = nofds,
                      efds = nofds,
                      tout = tout,
                      ss = ss});
                   if SigHandler.isset_sig (hndlrs,sigidx)
                      then if MappedNativeWordRegister.! reg >= value
                              then SigHandler.reset_sig (hndlrs,sigidx)
                              else (SigHandler.reset_sig (hndlrs,sigidx);
                                    iter ())
                      else iter ())
         end

      in iter ()
      end
end

fun mkHandlers (sig1,sig2,pid1,pid2) =
   let val hndlrs = SigHandler.sigaction 2
       val sig1_idx = 0
       val sig2_idx = 1
       val sigs = [sig1, sig2]
       val _ = ignoreSignals sigs
       val oldss = unBlockSignals sigs
       val mask = blockSignals sigs
       val _ = setSigaction hndlrs (sig1,sig1_idx)
       val _ = setSigaction hndlrs (sig2,sig2_idx)
       val reg1 = SigHandler.si_int_reg (hndlrs,sig1_idx)
       val reg2 = SigHandler.si_int_reg (hndlrs,sig2_idx)
       fun block_on_read n =
          let val value = Word.fromInt n
          in waitOnSigs (reg1,value,hndlrs,sigs,sig1_idx,mask,TimeSpec.fromMilliseconds 1000)
          end
       fun block_on_write n =
          let val value = Word.fromInt n
          in waitOnSigs (reg2,value,hndlrs,sigs,sig2_idx,mask,TimeSpec.fromMilliseconds 1000)
          end
       fun signal_read n =
          let val value = Word.fromInt n
          in SigAction.sigqueue (pid2, sig2, ValRepr.longFromWord (value))
          end
       fun signal_write n =
          let val value = Word.fromInt n
          in SigAction.sigqueue (pid1, sig1, ValRepr.longFromWord (value))
          end
   in (reg1,reg2,block_on_read,block_on_write,signal_read,signal_write)
   end

val (writereg,readreg,block_on_read,block_on_write,signal_read,signal_write) = 
      mkHandlers (Signal.usr1,Signal.usr2,SigAction.getpid (),SigAction.getpid ())

structure Fifo =
 SigQFifo
   (structure WordStruct = WordStruct
    structure ArrayStruct = DMappedWord8Array
    structure ArraySliceStruct = MappedWord8ArraySlice
    val block_on_read : int -> unit = block_on_read
    val block_on_write : int -> unit = block_on_write
    val signal_read : int -> unit  = signal_read
    val signal_write : int -> unit = signal_write
    val length = Word8Vector.length
    val appi = Word8Vector.appi
    val get_cptr : ArrayStruct.array -> Dynlib.cptr = MappedWord8Array.get_cptr
    val zero : ArrayStruct.elem = 0w0)

structure ObjRepr =
   ObjRepr
     (type state = Fifo.fifo
      val readByte = Fifo.readByte
      val writeByte = Fifo.writeByte)

fun testCodec (buff : Fifo.fifo) (obj : 'a) : 'a =
   let val _ = ObjRepr.encode (obj,buff)
       val _ = MemDebug.gc_full_major()
   in ObjRepr.decode buff 
   end

val fifo = Fifo.fifo (12,(readreg,writereg));

val s : string = testCodec fifo "abcdefg"; 

val ("abcdefg",42.0,0wx7fffffff) = testCodec fifo ("abcdefg",42.0,0wx7fffffff);

