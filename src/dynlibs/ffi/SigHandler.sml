signature SigHandler =
sig
   type handler
   type siginfo = {addr : Dynlib.cptr, band : int, code : word, errno : int, fd : int, int : int,
     overrun : int, pid : int, ptr : Dynlib.cptr, signo : int, status : word,
     stime : word, timerid : int, uid : word, utime : word, value : word}
   val sigwaitinfo : SigSet.sigset -> Signal.signal * siginfo
   val sigtimedwait : SigSet.sigset -> TimeSpec.timespec -> (Signal.signal * siginfo) option
   val sighandler : int -> handler
   val sigaction : int -> handler
   val isset : handler -> bool
   val set_sigs : handler -> word
   val sig_last : handler * int -> word
   val sig_signal : handler * int -> word option
   val si_count : handler * int -> word
   val reset_count : handler * int -> unit
   val si_code : handler * int -> word option
   val si_pid : handler * int -> word option
   val si_ptr : handler * int -> Dynlib.cptr option
   val si_ptr_reg : handler * int -> MappedNativeAddressRegister.register
   val si_int : handler * int -> word option
   val si_int_reg : handler * int -> MappedNativeWordRegister.register
   val si_status : handler * int -> word option
   val si_value : handler * int -> word option
   val si_addr : handler * int -> Dynlib.cptr option
   val si_utime : handler * int -> word option
   val si_stime : handler * int -> word option
   val si_overrun : handler * int -> word option
   val si_timerid : handler * int -> word option
   val si_uid : handler * int -> word option
   val si_fd : handler * int -> int option
   val reset : handler -> unit
   val isset_sig : handler * int -> bool
   val reset_sig : handler * int -> unit
   val set_sig : handler * int -> unit
   val get_cptr : handler * int -> Dynlib.cptr
end

structure SigHandler :> SigHandler =
struct
   datatype sigaction =
          SigHandler of MappedNativeWordRegister.register *
                      (MappedNativeWordRegister.register *
                       (Lightning32.state * Dynlib.cptr)) Vector.vector
        | SigAction of  MappedNativeWordRegister.register *
                      ((MappedNativeWordRegister.register *
                        MappedNativeWordRegister.register *
                        MappedNativeWordRegister.register *
                        MappedNativeWordRegister.register *
                        MappedNativeWordRegister.register *
                        MappedNativeWordRegister.register *
                        MappedNativeWordRegister.register *
                        MappedNativeWordRegister.register *
                        MappedNativeWordRegister.register *
                        MappedNativeWordRegister.register *
                        MappedNativeWordRegister.register *
                        MappedNativeWordRegister.register *
                        MappedNativeWordRegister.register *
                        MappedNativeWordRegister.register) *

 (* This started out as just two fields, would you believe?
    Might as well just blit the whole structure across ... The problem
    is, only a small proportion of these fields are relevant to any
    one particular interrupt class. So we need a general class of
    handler that sub-types to particular instances such as SIGSEGV,
    SIGINT, SIGRTn etc., Interestingly, the field values also depend
    on the manner in which the signal was dispatched: a signal sent by
    sigqueue(2) will have different fields set compared to the same
    signal sent by an interval-timer or by kill(2).

    Anyway, grouping handlers for different signals together as
    elements of a uniform array was a stupid idea. The grouping needs
    to be able to reflect their inter-dependencies and these are in
    terms of the far more general process context, which typically
    changes dynamically in response to other signals.

    So we need some sort of nested context-closure-like
    structure. This could automate the management of restoring
    handlers and dispositions too: on closing a context we restore the
    handlers to their state in the parent context. Perhaps this would
    be useful for the context switches under fork(2) that occur in
    parent and child processes too?

 *)

                       (Lightning32.state * Dynlib.cptr)) Vector.vector
   type handler = sigaction
   type siginfo = {addr : Dynlib.cptr, band : int, code : word, errno : int, fd : int, int : int,
     overrun : int, pid : int, ptr : Dynlib.cptr, signo : int, status : word,
     stime : word, timerid : int, uid : word, utime : word, value : word}
   local
      val dlxh = Dynlib.dlopen {lib = "",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }

      fun sym s = Dynlib.dlsym dlxh s
      fun symp s = Dynlib.cptr (sym s)
      open Lightning32
      open MappedNativeWordRegister
      infix 2 :=

      val () = init_jit();

      fun jit_fprolog jit_ =
         jit_note (jit_,NULL,0w0)
         before jit_prolog (jit_);

      val wsz = Word.fromInt (WORDSIZE div 8);

      val jit_ = jit_new_state();

      val sigwaitinfo_call = jit_fprolog (jit_);
      val sigset = jit_arg (jit_);
      val siginfo = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, sigset);
      val () = jit_getarg (jit_, V1, siginfo);

      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V0);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_finishi (jit_, symp "sigwaitinfo");
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val sigtimedwait_call = jit_fprolog (jit_);
      val sigset = jit_arg (jit_);
      val siginfo = jit_arg (jit_);
      val timeout = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, sigset);
      val () = jit_getarg (jit_, V1, siginfo);
      val () = jit_getarg (jit_, V2, timeout);

      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V0);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_pushargr (jit_, V2);
      val _ = jit_finishi (jit_, symp "sigtimedwait");
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val _ = jit_emit (jit_);

      val sigwaitinfocalladd = jit_address (jit_,sigwaitinfo_call);
      val sigtimedwaitcalladd = jit_address (jit_,sigtimedwait_call);

      val () = jit_clear_state (jit_);

      val sigwaitinfo_ :  Dynlib.cptr -> Dynlib.cptr -> int =
          fn a => fn b =>
          let val jitref = jit_ (* to prevent the machine code from being GCed *)
          in app2 sigwaitinfocalladd a b
          end;

      val sigtimedwait_ :  Dynlib.cptr -> Dynlib.cptr -> Dynlib.cptr -> int =
          fn a => fn b => fn c => 
          let val jitref = jit_ (* to prevent the machine code from being GCed *)
          in app3 sigtimedwaitcalladd a b c
          end;

      local (* So as not to allocate a new buffer on each call *)
         open MappedStruct
         val siginfo_size = Word.toInt (SigAction.siginfo_size)
         val size = SigSet.size + siginfo_size + TimeSpec.size
         fun nbits n 0w0 = n
           | nbits n w = nbits (n+1) (Word.>>(w,0w1))
         val bits = nbits 0 (Word.fromInt (size - 1))
         val buff = fifo bits
         fun cptrFromOffs (slc,offs) =
                ValRepr.word8VectorCptr
                  (MappedWord8ArraySlice.vector
                    (MappedWord8ArraySlice.subslice (slc,Word.toInt offs,SOME ((Word.wordSize + 7) div 8))))
         val wordFromOffs = ValRepr.wordFromLong o cptrFromOffs
         val intFromOffs = Word.toInt o wordFromOffs
      in
         fun sigwaitinfo ss =
            let val () = MappedStruct.zeroBuffer buff
                val sigset = MappedStruct.writePtr buff
                val () = SigSet.writeSigset (buff,ss)
                val sinfo = MappedStruct.writePtr buff
                val () = MappedStruct.skipWrite (buff,siginfo_size)

                val rv = sigwaitinfo_  sigset sinfo
                val () = MappedStruct.skipRead (buff,SigSet.size)

                val slc = MappedStruct.readSlice (buff,siginfo_size)
                local open SigAction
                in
                   val res =
                     { signo = intFromOffs (slc, si_signo_offs),
                       errno = intFromOffs (slc, si_errno_offs),
                       code = wordFromOffs (slc, si_code_offs),
                       pid = intFromOffs (slc, si_pid_offs),
                       uid = wordFromOffs (slc, si_uid_offs),
                       status = wordFromOffs (slc, si_status_offs),
                       utime = wordFromOffs (slc, si_utime_offs),
                       stime = wordFromOffs (slc, si_stime_offs),
                       value = wordFromOffs (slc, si_value_offs),
                       int = intFromOffs (slc, si_int_offs),
                       ptr = cptrFromOffs (slc, si_ptr_offs),
                       overrun = intFromOffs (slc, si_overrun_offs),
                       timerid = intFromOffs (slc, si_timerid_offs),
                       addr = cptrFromOffs (slc, si_addr_offs),
                       band = intFromOffs (slc, si_band_offs),
                       fd = intFromOffs (slc, si_fd_offs)}
               end
            in if rv >= 0
                then (Signal.fromWord(Word.fromInt rv),res)
                else SysErr.raiseSysErr "sigwaitinfo"
            end
         fun sigtimedwait ss tout =
            let val () = MappedStruct.zeroBuffer buff
                val sigset = MappedStruct.writePtr buff
                val () = SigSet.writeSigset (buff,ss)
                val sinfo = MappedStruct.writePtr buff
                val () = MappedStruct.skipWrite (buff,siginfo_size)
                val timeout = MappedStruct.writePtr buff
                val () = TimeSpec.writeTimespec (buff,tout)

                val rv = sigtimedwait_  sigset sinfo timeout
                val () = MappedStruct.skipRead (buff,SigSet.size)

                val slc = MappedStruct.readSlice (buff,siginfo_size)
                local open SigAction
                in
                   val res =
                     { signo = intFromOffs (slc, si_signo_offs),
                       errno = intFromOffs (slc, si_errno_offs),
                       code = wordFromOffs (slc, si_code_offs),
                       pid = intFromOffs (slc, si_pid_offs),
                       uid = wordFromOffs (slc, si_uid_offs),
                       status = wordFromOffs (slc, si_status_offs),
                       utime = wordFromOffs (slc, si_utime_offs),
                       stime = wordFromOffs (slc, si_stime_offs),
                       value = wordFromOffs (slc, si_value_offs),
                       int = intFromOffs (slc, si_int_offs),
                       ptr = cptrFromOffs (slc, si_ptr_offs),
                       overrun = intFromOffs (slc, si_overrun_offs),
                       timerid = intFromOffs (slc, si_timerid_offs),
                       addr = cptrFromOffs (slc, si_addr_offs),
                       band = intFromOffs (slc, si_band_offs),
                       fd = intFromOffs (slc, si_fd_offs)}
               end
            in if rv >= 0
                then SOME (Signal.fromWord(Word.fromInt rv),res)
                else if SysErr.errno() = SysErr.EAGAIN
                        then NONE
                        else SysErr.raiseSysErr "sigtimedwait"
            end
      end
      fun mkHandler flagp sigp sigidx =
         let val jit_ = jit_new_state();
             val handler_call = jit_fprolog (jit_);
             val signal = jit_arg (jit_);
             val () = jit_getarg (jit_, V0, signal);
             val _ = jit_sti_i (jit_, ValRepr.cptr_offs sigp ((sigidx + 0) * (Word.toInt wsz)), V0);
             val _ = jit_movi (jit_, R0, Word.<<(0w1,Word.fromInt sigidx));
             val _ = jit_ldi_i (jit_, R1, flagp);
             val _ = jit_orr (jit_, R0, R0, R1);
             val _ = jit_sti_i (jit_, flagp, R0);
             val _ = jit_ret (jit_);
             val _ = jit_epilog (jit_);
             val _ = jit_emit (jit_);
             val () = jit_clear_state (jit_);
         in (jit_,jit_address (jit_,handler_call))
         end
      fun mkSigaction flagp sigp sigidx nsigvs =
         let val jit_ = jit_new_state();
             val sigaction_call = jit_fprolog (jit_);
             val signal = jit_arg (jit_);
             val siginfo = jit_arg (jit_);
             val sigval = jit_arg (jit_);
             val () = jit_getarg (jit_, V0, signal);
             val signalptr = ValRepr.cptr_offs sigp ((sigidx * nsigvs + 0) * (Word.toInt wsz)) 
             val countptr  = ValRepr.cptr_offs sigp ((sigidx * nsigvs + 1) * (Word.toInt wsz))
             val _ = jit_sti_i (jit_,signalptr, V0);
             val _ = jit_ldi_i (jit_, V1, countptr);
             val _ = jit_addi (jit_, V1, V1, 0w1);
             val _ = jit_sti_i (jit_, countptr, V1);

             val _ = jit_movi (jit_, R0, Word.<<(0w1,Word.fromInt sigidx));
             val _ = jit_ldi_i (jit_, R1, flagp);
             val _ = jit_orr (jit_, R0, R0, R1);
             val _ = jit_sti_i (jit_, flagp, R0);

             val () = jit_getarg (jit_, V0, siginfo);
             val _  = jit_ldxi (jit_, V1, V0, SigAction.si_code_offs); (* V1 = v->si_code *)
             val _  = jit_sti_i (jit_,
                           ValRepr.cptr_offs sigp ((sigidx * nsigvs + 2) * (Word.toInt wsz)), V1);
             val _  = jit_ldxi (jit_, V1, V0, SigAction.si_pid_offs); (* V1 = v->si_pid *)
             val _  = jit_sti_i (jit_,
                           ValRepr.cptr_offs sigp ((sigidx * nsigvs + 3) * (Word.toInt wsz)), V1);
             val _  = jit_ldxi (jit_, V1, V0, SigAction.si_ptr_offs); (* V1 = v->si_ptr *)
             val _  = jit_sti_i (jit_,
                           ValRepr.cptr_offs sigp ((sigidx * nsigvs + 4) * (Word.toInt wsz)), V1);
             val _  = jit_ldxi (jit_, V1, V0, SigAction.si_utime_offs); (* V1 = v->si_utime *)
             val _  = jit_sti_i (jit_,
                           ValRepr.cptr_offs sigp ((sigidx * nsigvs + 5) * (Word.toInt wsz)), V1);
             val _  = jit_ldxi (jit_, V1, V0, SigAction.si_stime_offs); (* V1 = v->si_stime *)
             val _  = jit_sti_i (jit_,
                           ValRepr.cptr_offs sigp ((sigidx * nsigvs + 6) * (Word.toInt wsz)), V1);
             val _  = jit_ldxi (jit_, V1, V0, SigAction.si_overrun_offs); (* V1 = v->si_overrun *)
             val _  = jit_sti_i (jit_,
                           ValRepr.cptr_offs sigp ((sigidx * nsigvs + 7) * (Word.toInt wsz)), V1);
             val _  = jit_ldxi (jit_, V1, V0, SigAction.si_timerid_offs); (* V1 = v->si_timerid *)
             val _  = jit_sti_i (jit_,
                           ValRepr.cptr_offs sigp ((sigidx * nsigvs + 8) * (Word.toInt wsz)), V1);
             val _  = jit_ldxi (jit_, V1, V0, SigAction.si_status_offs); (* V1 = v->si_status *)
             val _  = jit_sti_i (jit_,
                           ValRepr.cptr_offs sigp ((sigidx * nsigvs + 9) * (Word.toInt wsz)), V1);
             val _  = jit_ldxi (jit_, V1, V0, SigAction.si_value_offs); (* V1 = v->si_value *)
             val _  = jit_sti_i (jit_,
                           ValRepr.cptr_offs sigp ((sigidx * nsigvs + 10) * (Word.toInt wsz)), V1);
             val _  = jit_ldxi (jit_, V1, V0, SigAction.si_addr_offs); (* V1 = v->si_addr *)
             val _  = jit_sti_i (jit_,
                           ValRepr.cptr_offs sigp ((sigidx * nsigvs + 11) * (Word.toInt wsz)), V1);
             val _  = jit_ldxi (jit_, V1, V0, SigAction.si_uid_offs); (* V1 = v->si_uid *)
             val _  = jit_sti_i (jit_,
                           ValRepr.cptr_offs sigp ((sigidx * nsigvs + 12) * (Word.toInt wsz)), V1);
             val _  = jit_ldxi (jit_, V1, V0, SigAction.si_fd_offs); (* V1 = v->si_fd *)
             val _  = jit_sti_i (jit_,
                           ValRepr.cptr_offs sigp ((sigidx * nsigvs + 13) * (Word.toInt wsz)), V1);

             val _ = jit_ret (jit_);
             val _ = jit_epilog (jit_);

             val _ = jit_emit (jit_);

             val () = jit_clear_state (jit_);
         in (jit_,jit_address (jit_,sigaction_call))
         end
      fun mkRegisterVector n slc =
         let open MappedWord8ArraySlice
         in if length slc < n*(Word.toInt wsz)
               then raise Size
               else Vector.tabulate (n,fn i => new (subslice(slc,i*(Word.toInt wsz),SOME (Word.toInt wsz))))
         end
   in
      val sigwaitinfo = sigwaitinfo
      val sigtimedwait = sigtimedwait
      fun sighandler n =
         if n >= WORDSIZE
            then raise Size
            else let val arr = MappedWord8Array.array((n+1)*(Word.toInt wsz),0w0)
                     val regsslc = MappedWord8ArraySlice.slice
                                        (arr,0,SOME ((n+1)*(Word.toInt wsz)))
                     val regsv = mkRegisterVector (n+1) regsslc
                     val arrp = MappedWord8Array.get_cptr arr
                     val flagp = arrp
                     val sigsp = ValRepr.cptr_offs arrp  (1*(Word.toInt wsz))
                     val vec = Vector.tabulate 
                                  (n,fn i => (Vector.sub(regsv,i+1),mkHandler flagp sigsp i))
                  in SigHandler(Vector.sub(regsv,0),vec)
                  end
      fun sigaction n =
         if n >= WORDSIZE
            then raise Size
            else let val arr = MappedWord8Array.array((14*n+1)*(Word.toInt wsz),0w0)
                     val regsslc = MappedWord8ArraySlice.slice
                                        (arr,0,SOME ((14*n+1)*(Word.toInt wsz)))
                     val regsv = mkRegisterVector (14*n+1) regsslc
                     val arrp = MappedWord8Array.get_cptr arr
                     val flagp = arrp
                     val sigsp = ValRepr.cptr_offs arrp  (1*(Word.toInt wsz))
                     val vec = Vector.tabulate
                                  (n,fn i => ((Vector.sub(regsv,14*i+1),
                                               Vector.sub(regsv,14*i+2),
                                               Vector.sub(regsv,14*i+3),
                                               Vector.sub(regsv,14*i+4),
                                               Vector.sub(regsv,14*i+5),
                                               Vector.sub(regsv,14*i+6),
                                               Vector.sub(regsv,14*i+7),
                                               Vector.sub(regsv,14*i+8),
                                               Vector.sub(regsv,14*i+9),
                                               Vector.sub(regsv,14*i+10),
                                               Vector.sub(regsv,14*i+11),
                                               Vector.sub(regsv,14*i+12),
                                               Vector.sub(regsv,14*i+13),
                                               Vector.sub(regsv,14*i+14)),
                                               mkSigaction flagp sigsp i 14))
                  in SigAction(Vector.sub(regsv,0),vec)
                  end
      fun isset (SigHandler(reg,_)) = if !reg = 0w0 then false else true
        | isset (SigAction(reg,_)) = if !reg = 0w0 then false else true
      fun set_sigs (SigHandler(reg,_)) = !reg
        | set_sigs (SigAction(reg,_)) = !reg
      fun reset (SigHandler(reg,_)) = reg := 0w0
        | reset (SigAction(reg,_)) = reg := 0w0
      fun isset_sig (SigHandler(reg,_),i) =
         if Word.andb(Word.<<(0w1,Word.fromInt i),!reg) = 0w0
            then false
            else true
        | isset_sig (SigAction(reg,_),i) =
         if Word.andb(Word.<<(0w1,Word.fromInt i),!reg) = 0w0
            then false
            else true
      fun sig_signal (hnd as (SigHandler(_,vec),i)) =
         if isset_sig hnd
            then SOME (!(#1(Vector.sub(vec,i))))
            else NONE
        | sig_signal (hnd as (SigAction(_,vec),i)) =
         if isset_sig hnd
            then SOME (!(#1(#1(Vector.sub(vec,i)))))
            else NONE
      fun si_count (SigHandler _,_) = raise Fail "si_count: not a sigaction"
        | si_count (hnd as (SigAction(_,vec),i)) =
             (!(#2(#1(Vector.sub(vec,i)))))
      fun si_code (SigHandler _,_) = raise Fail "si_code: not a sigaction"
        | si_code (hnd as (SigAction(_,vec),i)) =
         if isset_sig hnd
            then SOME (!(#3(#1(Vector.sub(vec,i)))))
            else NONE
      fun si_pid (SigHandler _,_) = raise Fail "si_pid: not a sigaction"
        | si_pid (hnd as (SigAction(_,vec),i)) =
         if isset_sig hnd
            then SOME (!(#4(#1(Vector.sub(vec,i)))))
            else NONE
      fun si_ptr (SigHandler _,_) = raise Fail "si_ptr: not a sigaction"
        | si_ptr (hnd as (SigAction(_,vec),i)) =
         if isset_sig hnd
            then SOME (MappedNativeAddressRegister.!(#5(#1(Vector.sub(vec,i)))))
            else NONE
      fun si_ptr_reg (SigHandler _,_) = raise Fail "si_ptr_reg: not a sigaction"
        | si_ptr_reg (hnd as (SigAction(_,vec),i)) =
              #5(#1(Vector.sub(vec,i)))
      fun si_int (SigHandler _,_) = raise Fail "si_int: not a sigaction"
        | si_int (hnd as (SigAction(_,vec),i)) =
         if isset_sig hnd
            then SOME (!(#5(#1(Vector.sub(vec,i)))))
            else NONE
      fun si_int_reg (SigHandler _,_) = raise Fail "si_int_reg: not a sigaction"
        | si_int_reg (hnd as (SigAction(_,vec),i)) =
              #5(#1(Vector.sub(vec,i)))
      fun si_utime (SigHandler _,_) = raise Fail "si_utime: not a sigaction"
        | si_utime (hnd as (SigAction(_,vec),i)) =
         if isset_sig hnd
            then SOME (!(#6(#1(Vector.sub(vec,i)))))
            else NONE
      fun si_stime (SigHandler _,_) = raise Fail "si_stime: not a sigaction"
        | si_stime (hnd as (SigAction(_,vec),i)) =
         if isset_sig hnd
            then SOME (!(#7(#1(Vector.sub(vec,i)))))
            else NONE
      fun si_overrun (SigHandler _,_) = raise Fail "si_overrun: not a sigaction"
        | si_overrun (hnd as (SigAction(_,vec),i)) =
         if isset_sig hnd
            then SOME (!(#8(#1(Vector.sub(vec,i)))))
            else NONE
      fun si_timerid (SigHandler _,_) = raise Fail "si_timerid: not a sigaction"
        | si_timerid (hnd as (SigAction(_,vec),i)) =
         if isset_sig hnd
            then SOME (!(#9(#1(Vector.sub(vec,i)))))
            else NONE
      fun si_status (SigHandler _,_) = raise Fail "si_status: not a sigaction"
        | si_status (hnd as (SigAction(_,vec),i)) =
         if isset_sig hnd
            then SOME (!(#10(#1(Vector.sub(vec,i)))))
            else NONE
      fun si_value (SigHandler _,_) = raise Fail "si_value: not a sigaction"
        | si_value (hnd as (SigAction(_,vec),i)) =
         if isset_sig hnd
            then SOME (!(#11(#1(Vector.sub(vec,i)))))
            else NONE
      fun si_addr (SigHandler _,_) = raise Fail "si_addr: not a sigaction"
        | si_addr (hnd as (SigAction(_,vec),i)) =
         if isset_sig hnd
            then SOME (ValRepr.longFromWord(!(#12(#1(Vector.sub(vec,i))))))
            else NONE
      fun si_uid (SigHandler _,_) = raise Fail "si_uid: not a sigaction"
        | si_uid (hnd as (SigAction(_,vec),i)) =
         if isset_sig hnd
            then SOME (!(#13(#1(Vector.sub(vec,i)))))
            else NONE
      fun si_fd (SigHandler _,_) = raise Fail "si_fd: not a sigaction"
        | si_fd (hnd as (SigAction(_,vec),i)) =
         if isset_sig hnd
            then SOME (Word.toInt(!(#14(#1(Vector.sub(vec,i))))))
            else NONE
      fun sig_last (SigHandler(_,vec),i) = !(#1(Vector.sub(vec,i)))
        | sig_last (SigAction(_,vec),i) = !(#1(#1(Vector.sub(vec,i))))
      fun set_sig (SigHandler(reg,_),i) =
                     reg := (Word.orb(Word.<<(0w1,Word.fromInt i),!reg))
        | set_sig (SigAction(reg,_),i) =
                     reg := (Word.orb(Word.<<(0w1,Word.fromInt i),!reg))
      fun reset_sig (SigHandler(reg,_),i) =
                     reg := (Word.andb(Word.notb(Word.<<(0w1,Word.fromInt i)),!reg))
        | reset_sig (SigAction(reg,_),i) =
                     reg := (Word.andb(Word.notb(Word.<<(0w1,Word.fromInt i)),!reg))
      fun reset_count (SigHandler(reg,_),i) =
                     raise Fail "reset_count: not a sigaction"
        | reset_count (SigAction(_,vec),i) =
                     (#2(#1(Vector.sub(vec,i)))) := 0w0
      fun get_cptr (SigHandler(_,vec) : handler,i) = (#2 (#2 (Vector.sub(vec,i))))
        | get_cptr (SigAction(_,vec) : handler,i) = (#2 (#2 (Vector.sub(vec,i))))
   end
end 
