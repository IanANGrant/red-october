structure PSelect =
struct
   local
      open MappedStruct
      open Dynlib

      val dlxh = Dynlib.dlopen {lib = "",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }

      fun sym s = Dynlib.dlsym dlxh s;
      fun symp s = Dynlib.cptr (sym s);

      open Lightning32

      val () = init_jit();

      val jit_ = jit_new_state();

      fun jit_fprolog jit_ =
         jit_note (jit_,NULL,0w0)
         before jit_prolog (jit_);

      val wsz = Word.fromInt (WORDSIZE div 8);

      val pselect_call = jit_fprolog (jit_);
      val v = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, v);

      val _  = jit_ldxi (jit_, V1, V0, wsz * 0w0); (* V1 = Field(v,0) *)
      val _  = jit_rshi (jit_, V1, V1, 0w1); (* Long_val(V1) *)
      val _  = jit_ldxi (jit_, V2, V0, wsz * 0w1); (* V2 = Field(v,1) *)
      val _  = jit_ldxi (jit_, R0, V0, wsz * 0w2); (* R0 = Field(v,2) *)
      val _  = jit_ldxi (jit_, R1, V0, wsz * 0w3); (* R1 = Field(v,3) *)
      val _  = jit_ldxi (jit_, R2, V0, wsz * 0w4); (* R2 = Field(v,4) *)
      val _  = jit_ldxi (jit_, V0, V0, wsz * 0w5); (* V0 = Field(v,5) *)
      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_pushargr (jit_, V2);
      val _ = jit_pushargr (jit_, R0);
      val _ = jit_pushargr (jit_, R1);
      val _ = jit_pushargr (jit_, R2);
      val _ = jit_pushargr (jit_, V0);
      val _ = jit_finishi (jit_, (symp "pselect"));
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val _ = jit_emit (jit_);

      val pselectcalladd = jit_address (jit_,pselect_call);

      val () = jit_clear_state (jit_);

      val pselect_call : (int * cptr * cptr * cptr * cptr * cptr) -> int =
          fn a =>
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in app1 pselectcalladd a
          end
   in
      local (* So as not to allocate a new static fifo buffer on each call *)
         open MappedStruct
         val size = FDSet.size * 3 + TimeSpec.size + SigSet.size
         fun nbits n 0w0 = n
           | nbits n w = nbits (n+1) (Word.>>(w,0w1))
         val bits = nbits 0 (Word.fromInt (size - 1))
         val buff = fifo bits
      in fun pselect (args as {maxfd = lubfd : int,
                               rfds = rfds : FDSet.fdset,
                               wfds = wfds : FDSet.fdset,
                               efds = efds : FDSet.fdset,
                               tout = tout : TimeSpec.timespec,
                               ss = ss : SigSet.sigset}) =
         let val () = zeroBuffer buff
             val rfdsp = writePtr buff
             val () = FDSet.writeFdset (buff,rfds)
             val wfdsp = writePtr buff
             val () = FDSet.writeFdset (buff,wfds)
             val efdsp = writePtr buff
             val () = FDSet.writeFdset (buff,efds)
             val toutp = writePtr buff
             val () = TimeSpec.writeTimespec (buff,tout)
             val ssp = writePtr buff
             val () = SigSet.writeSigset (buff,ss)
             val rv = pselect_call (lubfd,rfdsp,wfdsp,efdsp,toutp,ssp)
         in if rv >= 0
               then (rv,FDSet.readFdset buff,FDSet.readFdset buff,FDSet.readFdset buff)
               else SysErr.raiseSysErr "pselect"
         end
      end
   end
end
