signature SigSet =
sig
   type sigset
   datatype procmask =
      SIG_BLOCK
    | SIG_UNBLOCK
    | SIG_SETMASK
   val sigprocmask : (procmask * sigset option) -> sigset
   val sigpending : sigset -> sigset
   val sigsuspend : sigset -> unit
   val fromSlice : MappedWord8ArraySlice.slice -> sigset
   val writeSigset : (MappedStruct.fifo * sigset) -> unit
   val readSigset : MappedStruct.fifo -> sigset
   val sigset : Signal.signal list -> sigset
   val toList : sigset -> Signal.signal list
   val sigemptyset : sigset -> sigset
   val sigfillset : sigset -> sigset
   val sigaddset : (sigset * Signal.signal) -> sigset
   val sigdelset : (sigset * Signal.signal) -> sigset
   val sigismember : (sigset * Signal.signal) -> bool
   val size : int
   val maxsig : int
end

structure SigSet :> SigSet =
struct
   local open Dynlib

      val dlxh = Dynlib.dlopen {lib = "",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }

      fun sym s = Dynlib.dlsym dlxh s;
      fun symp s = Dynlib.cptr (sym s);

      val dlih = Dynlib.dlopen {lib = "libcinfo.so",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }
      fun syminf s = Dynlib.dlsym dlih s;

      val (sigsetSize,maxsig') =
                       case (Dynlib.app1 (syminf "sigset_constants") : unit -> int Vector.vector) ()
                         of #[n,m] => (n,m) | _ => raise Fail "SigSet: sigset_constants failed"

      open Lightning32

      val () = init_jit();

      val jit_ = jit_new_state();

      fun jit_fprolog jit_ =
         jit_note (jit_,NULL,0w0)
         before jit_prolog (jit_);

      val wsz = Word.fromInt (WORDSIZE div 8);

      val sig1arg_call = jit_fprolog (jit_);
      val sigfnp = jit_arg (jit_);
      val sigsetp = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, sigfnp);
      val () = jit_getarg (jit_, V1, sigsetp);

      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_finishr (jit_, V0);
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val sig2arg_call = jit_fprolog (jit_);
      val sigfnp = jit_arg (jit_);
      val sigsetp = jit_arg (jit_);
      val signal = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, sigfnp);
      val () = jit_getarg (jit_, V1, sigsetp);
      val () = jit_getarg_i (jit_, V2, signal);
      val _  = jit_rshi (jit_, V2, V2, 0w1); (* V2=Long_val(signal) *)

      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_pushargr (jit_, V2);
      val _ = jit_finishr (jit_, V0);
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val procmask_call = jit_fprolog (jit_);
      val how = jit_arg (jit_);
      val sigsetp1 = jit_arg (jit_);
      val sigsetp2 = jit_arg (jit_);
      val () = jit_getarg_i (jit_, V0, how);
      val _  = jit_rshi (jit_, V0, V0, 0w1); (* V0=Long_val(how) *)
      val () = jit_getarg (jit_, V1, sigsetp1);
      val () = jit_getarg (jit_, V2, sigsetp2);

      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V0);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_pushargr (jit_, V2);
      val _ = jit_finishi (jit_, symp "sigprocmask");
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val _ = jit_emit (jit_);

      val sig1argcalladd = jit_address (jit_,sig1arg_call);
      val sig2argcalladd = jit_address (jit_,sig2arg_call);
      val procmaskcalladd = jit_address (jit_,procmask_call);

      val () = jit_clear_state (jit_);

      val sig1arg_call : cptr -> cptr -> int =
          fn a => fn b => 
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in app2 sig1argcalladd a b
          end;
      val sig2arg_call : cptr -> cptr -> word -> int =
          fn a => fn b => fn c => 
          let val jitref = jit_
          in app3 sig2argcalladd a b c
          end;
      val procmask_call : word -> cptr -> cptr -> int =
          fn a => fn b => fn c => 
          let val jitref = jit_
          in app3 procmaskcalladd a b c
          end;
   in
   type sigset = MappedWord8ArraySlice.slice
   datatype procmask =
      SIG_BLOCK
    | SIG_UNBLOCK
    | SIG_SETMASK
   fun maskval SIG_BLOCK = 0w0
     | maskval SIG_UNBLOCK = 0w1
     | maskval SIG_SETMASK = 0w2
   fun fromSlice s =
      if MappedWord8ArraySlice.length s = sigsetSize
         then s
         else raise Size
   fun sigemptyset ss =
      let val (arr,i,n) = MappedWord8ArraySlice.base ss
          val ssp = ValRepr.cptr_offs (MappedWord8Array.get_cptr arr) i
          val rv = sig1arg_call (symp "sigemptyset") ssp
      in if rv = 0 then ss else raise Fail "sigemptyset error"
      end
   fun sigfillset ss =
      let val (arr,i,n) = MappedWord8ArraySlice.base ss
          val ssp = ValRepr.cptr_offs (MappedWord8Array.get_cptr arr) i
          val rv = sig1arg_call (symp "sigfillset") ssp
      in if rv = 0 then ss else raise Fail "sigfillset error"
      end
   fun sigaddset (ss, sgnl) =
      let val (arr,i,n) = MappedWord8ArraySlice.base ss
          val ssp = ValRepr.cptr_offs (MappedWord8Array.get_cptr arr) i
          val sgnl = Signal.toWord sgnl
          val rv = sig2arg_call (symp "sigaddset") ssp sgnl
      in if rv = 0 then ss else raise Fail "sigaddset error"
      end
   fun sigset l =
      let val slc = MappedWord8ArraySlice.full (MappedWord8Array.array (sigsetSize,0w0))
          val _ = sigemptyset (slc)
          fun iter [] = slc
            | iter (sgnl::rest) =
                 (sigaddset (slc,sgnl);
                  iter rest)
      in iter l
      end
   fun writeSigset (b,slc) =
      MappedStruct.write (b,slc)
   fun readSigset b = MappedStruct.readSlice (b,sigsetSize)
   fun sigpending ss =
      let val (arr,i,n) = MappedWord8ArraySlice.base ss
          val ssp = ValRepr.cptr_offs (MappedWord8Array.get_cptr arr) i
          val rv = sig1arg_call (symp "sigpending") ssp
      in if rv = 0 then ss else SysErr.raiseSysErr "sigpending"
      end
   fun sigsuspend ss =
      let val (arr,i,n) = MappedWord8ArraySlice.base ss
          val ssp = ValRepr.cptr_offs (MappedWord8Array.get_cptr arr) i
          val rv = sig1arg_call (symp "sigsuspend") ssp
      in if rv = ~1
            then if SysErr.errno() = SysErr.EINTR
                    then ()
                    else SysErr.raiseSysErr "sigsuspend"
            else ()
      end
   fun sigprocmask (how,ssopt) =
      let val buff = MappedStruct.fifo 8
          val newssp = case ssopt
                     of NONE => (MappedStruct.skipWrite (buff,sigsetSize);
                                 MappedStruct.writePtr buff)
                      | SOME ss => (MappedStruct.write (buff,ss);
                                    ValRepr.cptrFromWord 0w0)
          val oldssp = MappedStruct.writePtr buff
          val () = MappedStruct.skipWrite (buff,sigsetSize)
          val rv = procmask_call (maskval how) newssp oldssp
          val () = MappedStruct.skipRead (buff,sigsetSize)
      in if rv < 0
            then raise Fail "sigprocmask failed"
            else readSigset buff
      end
   fun sigdelset (ss, sgnl) =
      let val (arr,i,n) = MappedWord8ArraySlice.base ss
          val ssp = ValRepr.cptr_offs (MappedWord8Array.get_cptr arr) i
          val sgnl = Signal.toWord sgnl
          val rv = sig2arg_call (symp "sigdelset") ssp sgnl
      in if rv = 0 then ss else raise Fail "sigdelset error"
      end
   fun sigismember (ss, sgnl) =
      let val (arr,i,n) = MappedWord8ArraySlice.base ss
          val ssp = ValRepr.cptr_offs (MappedWord8Array.get_cptr arr) i
          val sgnl = Signal.toWord sgnl
          val rv = sig2arg_call (symp "sigismember") ssp sgnl
      in case rv
           of 0 => false
            | 1 => true
            | _ => SysErr.raiseSysErr "sigismember"
      end
   val size = sigsetSize
   val maxsig = maxsig' - 1
   fun toList ss =
      let fun iter (0,acc) = acc
            | iter (i,acc) =
               let val signal = Signal.fromWord (Word.fromInt i)
                   val acc' = if sigismember (ss,signal)
                                 then signal::acc
                                 else acc
               in iter (i-1,acc')
               end
      in iter (maxsig-1,[])
      end
   end
end

(*

load "SigSet";
val ss = SigSet.sigset [Signal.int,Signal.usr1];
val ssl = SigSet.toList ss handle SysErr.SysErr (s,err) => raise Fail (s^": "^(SysErr.toString err));
val sswl = List.map Signal.toWord ssl;

*)
