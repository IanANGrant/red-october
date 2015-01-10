signature SigAction =
sig
   datatype handler =
         SIG_DFL
       | SIG_IGN
       | SA_handler of Dynlib.cptr
       | SA_sigaction of Dynlib.cptr
   datatype saflag =
         SA_NODEFER
       | SA_ONSTACK
       | SA_RESTART
       | SA_SIGINFO
       | SA_NOCLDSTOP
       | SA_INTERRUPT
       | SA_NOCLDWAIT
       | SA_RESETHAND
   type sigaction =
        {sa_handler : handler,
         sa_mask : SigSet.sigset,
         sa_flags : saflag list}

   val sigaction : Signal.signal * sigaction -> sigaction
   val getpid : unit -> word
   val getppid : unit -> word
   val sigqueue : word * Signal.signal * Dynlib.cptr -> unit

   val SI_USER : word
   val SI_KERNEL : word
   val SI_QUEUE : word
   val SI_TIMER : word
   val SI_MESGQ : word
   val SI_ASYNCIO : word
   val SI_SIGIO : word
   val SI_TKILL : word
   val ILL_ILLOPC : word
   val ILL_ILLOPN : word
   val ILL_ILLADR : word
   val ILL_ILLTRP : word
   val ILL_PRVOPC : word
   val ILL_PRVREG : word
   val ILL_COPROC : word
   val ILL_BADSTK : word
   val FPE_INTDIV : word
   val FPE_INTOVF : word
   val FPE_FLTDIV : word
   val FPE_FLTOVF : word
   val FPE_FLTUND : word
   val FPE_FLTRES : word
   val FPE_FLTINV : word
   val FPE_FLTSUB : word
   val SEGV_MAPERR : word
   val SEGV_ACCERR : word
   val BUS_ADRALN : word
   val BUS_ADRERR : word
   val BUS_OBJERR : word
   val BUS_MCEERR_AR : word
   val BUS_MCEERR_AO : word
   val TRAP_BRKPT : word
   val TRAP_TRACE : word
   val TRAP_BRANCH : word
   val TRAP_HWBKPT : word
   val CLD_EXITED : word
   val CLD_KILLED : word
   val CLD_DUMPED : word
   val CLD_TRAPPED : word
   val CLD_STOPPED : word
   val CLD_CONTINUED : word
   val POLL_IN : word
   val POLL_OUT : word
   val POLL_MSG : word
   val POLL_ERR : word
   val POLL_PRI : word
   val POLL_HUP : word

   val siginfo_size : word
   val si_signo_offs : word
   val si_errno_offs : word
   val si_code_offs : word
   val si_pid_offs : word
   val si_uid_offs : word
   val si_status_offs : word
   val si_utime_offs : word
   val si_stime_offs : word
   val si_value_offs : word
   val si_int_offs : word
   val si_ptr_offs : word
   val si_overrun_offs : word
   val si_timerid_offs : word
   val si_addr_offs : word
   val si_band_offs : word
   val si_fd_offs : word
end

structure SigAction :> SigAction =
struct
   local open Dynlib

      val dlih = Dynlib.dlopen {lib = "libcinfo.so",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }
      fun syminf s = Dynlib.dlsym dlih s;

      fun siginfo_constants () = 
            Dynlib.app1 (syminf "siginfo_constants") () : 
               (word * word * word * word * word * word * word * word *
                word * word * word * word * word * word * word * word *
                word)

      val si_code_values =
         Dynlib.app1 (syminf "si_code_values") : unit ->
           (word * word * word * word * word * word * word * word *
            word * word * word * word * word * word * word * word *
            word * word * word * word * word * word * word * word *
            word * word * word * word * word * word * word * word *
            word * word * word * word * word * word * word * word *
            word * word * word * word * word * word * word)

      val dlxh = Dynlib.dlopen {lib = "",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }

      fun sym s = Dynlib.dlsym dlxh s
      fun symp s = Dynlib.cptr (sym s)

      open Lightning32

      val getpid : unit -> word
        = ValRepr.wordFromLong o (app1 (symp "getpid") : unit -> Dynlib.cptr)

      val getppid : unit -> word
        = ValRepr.wordFromLong o (app1 (symp "getppid") : unit -> Dynlib.cptr)

      val () = init_jit();

      val jit_ = jit_new_state();

      fun jit_fprolog jit_ =
         jit_note (jit_,NULL,0w0)
         before jit_prolog (jit_);

      val wsz = Word.fromInt (WORDSIZE div 8);

      val sigaction_call = jit_fprolog (jit_);
      val signal = jit_arg (jit_);
      val newsap = jit_arg (jit_);
      val oldsap = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, signal);
      val _  = jit_rshi (jit_, V0, V0, 0w1); (* V0=Long_val(signal) *)
      val () = jit_getarg (jit_, V1, newsap);
      val () = jit_getarg (jit_, V2, oldsap);

      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V0);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_pushargr (jit_, V2);
      val _ = jit_finishi (jit_, symp "sigaction");
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val sigqueue_call = jit_fprolog (jit_);
      val pid = jit_arg (jit_);
      val signal = jit_arg (jit_);
      val sigval = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, pid);
      val _  = jit_rshi (jit_, V0, V0, 0w1); (* V0=Long_val(pid) *)
      val () = jit_getarg (jit_, V1, signal);
      val _  = jit_rshi (jit_, V1, V1, 0w1); (* V1=Long_val(signal) *)
      val () = jit_getarg (jit_, V2, sigval);

      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V0);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_pushargr (jit_, V2);
      val _ = jit_finishi (jit_, symp "sigqueue");
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val _ = jit_emit (jit_);

      val sigactioncalladd = jit_address (jit_,sigaction_call);
      val sigqueuecalladd = jit_address (jit_,sigqueue_call);

      val () = jit_clear_state (jit_);

      val sigaction_ : word -> cptr -> cptr -> int =
          fn a => fn b => fn c => 
          let val jitref = jit_ (* to prevent the machine code from being GCed *)
          in app3 sigactioncalladd a b c
          end;

      val sigqueue_ : word -> word -> cptr -> int =
          fn a => fn b => fn c => 
          let val jitref = jit_ (* to prevent the machine code from being GCed *)
          in app3 sigqueuecalladd a b c
          end;

      val dlih = Dynlib.dlopen {lib = "libcinfo.so",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }
      fun syminf s = Dynlib.dlsym dlih s;

      val consts = Dynlib.app1 (syminf "sigaction_constants")
           : unit -> word * word * word * word * word * word * word * word * word * word;

      val (SA_NOCLDSTOP_val,
           SA_NOCLDWAIT_val,
           SA_SIGINFO_val,
           SA_ONSTACK_val,
           SA_RESTART_val,
           SA_INTERRUPT_val,
           SA_NODEFER_val,
           SA_RESETHAND_val,
           SIG_DFL_val,
           SIG_IGN_val) = consts ()
   in
   datatype handler =
      SIG_DFL
    | SIG_IGN
    | SA_handler of Dynlib.cptr
    | SA_sigaction of Dynlib.cptr
   datatype saflag =
      SA_NODEFER
    | SA_ONSTACK
    | SA_RESTART
    | SA_SIGINFO
    | SA_NOCLDSTOP
    | SA_INTERRUPT
    | SA_NOCLDWAIT
    | SA_RESETHAND
   val saflags =
         [(SA_NOCLDSTOP, SA_NOCLDSTOP_val ),
          (SA_NOCLDWAIT, SA_NOCLDWAIT_val ),
          (SA_SIGINFO,   SA_SIGINFO_val   ),
          (SA_ONSTACK,   SA_ONSTACK_val   ),
          (SA_RESTART,   SA_RESTART_val   ),
          (SA_INTERRUPT, SA_INTERRUPT_val ),
          (SA_NODEFER,   SA_NODEFER_val   ),
          (SA_RESETHAND, SA_RESETHAND_val )]
   type sigaction =
         {sa_handler : handler,
          sa_mask : SigSet.sigset,
          sa_flags : saflag list}
   fun lookup f =
      let fun iter [] = raise Fail "SigAction: lookup: impossible"
            | iter ((f',w)::rest) = if f' = f then w else iter rest
      in iter saflags
      end
   fun saflagsList (h,l) =
      let fun iter acc [] = List.rev acc
            | iter acc ((f',w)::rest) =
                if Word.andb(h,w) <> 0w0 orelse
                   Word.andb(l,w) <> 0w0
                   then iter (f'::acc) rest
                   else iter acc rest
      in iter [] saflags
      end
    fun listSaflags fs =
      let fun iter acc [] = acc
            | iter (acc as (h,l)) (f::rest) =
                let val w = lookup f
                    val (acc as (h,l)) =
                           if w > 0wxFF
                              then (Word.orb(w,h),l)
                              else (h,Word.orb(w,l))
                in iter acc rest
                end
      in iter (0w0,0w0) fs
      end
      local (* So as not to allocate a new buffer on each call *)
         open MappedStruct
         val size = 2 * (SigSet.size + 12)
         fun nbits n 0w0 = n
           | nbits n w = nbits (n+1) (Word.>>(w,0w1))
         val bits = nbits 0 (Word.fromInt (size - 1))
         val buff = fifo bits
      in
         val getpid = getpid
         val getppid = getppid

         val (SI_USER,       SI_KERNEL,     SI_QUEUE,    SI_TIMER,   SI_MESGQ,
              SI_ASYNCIO,    SI_SIGIO,      SI_TKILL,    ILL_ILLOPC,
              ILL_ILLOPN,    ILL_ILLADR,    ILL_ILLTRP,  ILL_PRVOPC,
              ILL_PRVREG,    ILL_COPROC,    ILL_BADSTK,  FPE_INTDIV,
              FPE_INTOVF,    FPE_FLTDIV,    FPE_FLTOVF,  FPE_FLTUND,
              FPE_FLTRES,    FPE_FLTINV,    FPE_FLTSUB,  SEGV_MAPERR,
              SEGV_ACCERR,   BUS_ADRALN,    BUS_ADRERR,  BUS_OBJERR,
              BUS_MCEERR_AR, BUS_MCEERR_AO, TRAP_BRKPT,  TRAP_TRACE,
              TRAP_BRANCH,   TRAP_HWBKPT,   CLD_EXITED,  CLD_KILLED,
              CLD_DUMPED,    CLD_TRAPPED,   CLD_STOPPED, CLD_CONTINUED,
              POLL_IN,       POLL_OUT,      POLL_MSG,    POLL_ERR,
              POLL_PRI,      POLL_HUP) = si_code_values ();

         val (siginfo_size,    si_signo_offs,  si_errno_offs,  si_code_offs,
              si_pid_offs,     si_uid_offs,    si_status_offs, si_utime_offs,
              si_stime_offs,   si_value_offs,  si_int_offs,    si_ptr_offs,
              si_overrun_offs, si_timerid_offs,si_addr_offs,   si_band_offs,
              si_fd_offs) = siginfo_constants ();

         fun sigqueue (pid : word,
                       signal : Signal.signal,
                       sigval : Dynlib.cptr) =
            let val rv = sigqueue_ pid (Signal.toWord signal) sigval
            in if rv = 0
               then ()
               else SysErr.raiseSysErr "sigqueue"
            end
         fun sigaction (sgnl : Signal.signal,
                        sa as ({sa_handler = hnd,
                                sa_mask = ss,
                                sa_flags = fs} : sigaction)) =
            let val (h,l) = listSaflags fs
                val hndp =
                       case hnd
                         of SIG_DFL => ValRepr.cptrFromWord (SIG_DFL_val)
                          | SIG_IGN => ValRepr.cptrFromWord (SIG_IGN_val)
                          | SA_handler p => p
                          | SA_sigaction p => p
                val () = MappedStruct.zeroBuffer buff
                val newsap = MappedStruct.writePtr buff
                val () = MappedStruct.writeAddr (buff,hndp)
                val () = SigSet.writeSigset (buff,ss)
                val (h,l) = if ValRepr.littleendian then (h,l) else (l,h)
                val () = MappedStruct.writeWord16 (buff,l)
                val () = MappedStruct.writeWord16 (buff,h)
                val () = MappedStruct.writeAddr (buff,ValRepr.cptrFromWord 0w0)
                val sasize = MappedStruct.writeCount buff

                val oldsap = MappedStruct.writePtr buff
                val () = MappedStruct.skipWrite (buff,sasize)

                val rv = sigaction_ (Signal.toWord sgnl) newsap oldsap
                val () = MappedStruct.skipRead (buff,sasize)

                val cp = MappedStruct.readAddr buff
                val ss' = SigSet.readSigset buff
                val l' = MappedStruct.readWord16 buff
                val h' = MappedStruct.readWord16 buff
                val cpw = ValRepr.wordFromCptr cp
                val hnd' = if Word.andb(l',lookup SA_SIGINFO) = 0w0
                              then SA_sigaction cp
                              else if cpw = SIG_DFL_val
                                      then SIG_DFL
                                      else if cpw = SIG_IGN_val
                                              then SIG_IGN
                                              else SA_handler cp
                val fs' = saflagsList (h',l')
            in if rv = 0
                then {sa_handler = hnd',
                      sa_mask = ss',
                      sa_flags = fs'}
                else SysErr.raiseSysErr "sigaction"
            end
      end    
   end
end
