signature Shm =
sig
    val shm_open : string * Fcntl.flags list * Fcntl.mode list -> Socket.unixfd
    val shm_unlink : string -> unit
    val mk_tmpshm : string * Fcntl.flags list * Fcntl.mode list -> string * Socket.unixfd
end

structure Shm :> Shm =
struct
   local
      open MappedStruct
      open Dynlib

      val dlh = Dynlib.dlopen {lib = "librt.so",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }

      fun sym s = Dynlib.dlsym dlh s;
      fun symp s = Dynlib.cptr (sym s);

      open Lightning32

      val () = init_jit();

      val jit_ = jit_new_state();

      fun jit_fprolog jit_ =
         jit_note (jit_,NULL,0w0)
         before jit_prolog (jit_);

      val wsz = Word.fromInt (WORDSIZE div 8);

      val shm_open_call = jit_fprolog (jit_);
      val v = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, v);

      val _  = jit_ldxi (jit_, V1, V0, wsz * 0w0); (* V1 = Field(v,0) *)
      val _  = jit_ldxi (jit_, V2, V0, wsz * 0w1); (* V2 = Field(v,1) *)
      val _  = jit_rshi (jit_, V2, V2, 0w1); (* Long_val(V2) *)
      val _  = jit_ldxi (jit_, R0, V0, wsz * 0w2); (* R0 = Field(v,2) *)
      val _  = jit_rshi (jit_, R0, R0, 0w1); (* Long_val(R0) *)
      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_pushargr (jit_, V2);
      val _ = jit_pushargr (jit_, R0);
      val _ = jit_finishi (jit_, (symp "shm_open"));
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val shm_unlink_call = jit_fprolog (jit_);
      val v = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, v);

      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V0);
      val _ = jit_finishi (jit_, (symp "shm_unlink"));
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val _ = jit_emit (jit_);

      val shm_open_add = jit_address (jit_,shm_open_call);
      val shm_unlink_add = jit_address (jit_,shm_unlink_call);

      val () = jit_clear_state (jit_);

      val shm_open_ : (string * word * word) -> int =
          fn a =>
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in app1 shm_open_add a
          end
      val shm_unlink_ : string -> int =
          fn a =>
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in app1 shm_unlink_add a
          end
   in fun shm_open (name,flags,mode) =
          let val flags = Fcntl.flagsWord flags
              val mode = Fcntl.modesWord mode
              val rv = shm_open_ (name,flags,mode)
          in if rv >= 0
                then FDSet.fromInt rv
                else SysErr.raiseSysErr "shm_open"
          end
      fun shm_unlink name =
          let val rv = shm_unlink_ name
          in if rv = 0
                then ()
                else SysErr.raiseSysErr "shm_unlink"
          end
      fun mk_tmpshm (s,flags,fmode) =
          let val flags' = Fcntl.O_CREAT::Fcntl.O_EXCL::flags
              val pidstr = Int.toString (Word.toInt (SigAction.getpid ()))
              fun iter n =
                 let val ns = Int.toString n
                     val name = "/tmpshm_"^s^"_"^pidstr^"_"^ns
                     val fdopt = SOME (shm_open (name,flags',fmode))
                                    handle SysErr.SysErr _ => NONE
                 in case fdopt
                      of SOME fd => (name, fd)
                       | NONE => iter (n + 1)
                 end
          in iter 1
          end
   end
end

(*
val _ = load "Shm";
val fd = Shm.shm_open ("/shmname",[Fcntl.O_CREAT,Fcntl.O_RDWR],[Fcntl.S_IRUSR,Fcntl.S_IWUSR]);
val _ = Shm.shm_unlink "/shmname";
*)
