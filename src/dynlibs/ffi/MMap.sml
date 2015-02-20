signature MMap =
sig
   datatype prot =
        PROT_EXEC
      | PROT_READ
      | PROT_WRITE
      | PROT_NONE
   datatype flags =
        MAP_SHARED
      | MAP_PRIVATE
      | MAP_ANONYMOUS
      | MAP_FIXED
      | MAP_GROWSDOWN
      | MAP_LOCKED
      | MAP_HUGETLB
      | MAP_NONBLOCK
      | MAP_NORESERVE
      | MAP_POPULATE
      | MAP_STACK
      | MAP_UNINITIALIZED
   datatype sync_flags =
        MS_ASYNC
      | MS_SYNC
      | MS_INVALIDATE
   val mmap : (Dynlib.cptr * word * prot list * flags list * Socket.unixfd * word) -> Dynlib.cptr
   val mmapMappedArray : (Dynlib.cptr * word * prot list * flags list * Socket.unixfd * word) ->
          MappedWord8Array.array
   val munmap : (Dynlib.cptr * word) -> unit
   val mprotect : (Dynlib.cptr * word * prot list) -> unit
   val msync : (Dynlib.cptr * word * sync_flags list) -> unit
   val PAGE_SIZE : word
end

structure MMap :> MMap =
struct
   datatype prot =
        PROT_EXEC
      | PROT_READ
      | PROT_WRITE
      | PROT_NONE
   datatype flags =
        MAP_SHARED
      | MAP_PRIVATE
      | MAP_ANONYMOUS
      | MAP_FIXED
      | MAP_GROWSDOWN
      | MAP_LOCKED
      | MAP_HUGETLB
      | MAP_NONBLOCK
      | MAP_NORESERVE
      | MAP_POPULATE
      | MAP_STACK
      | MAP_UNINITIALIZED
   datatype sync_flags =
        MS_ASYNC
      | MS_SYNC
      | MS_INVALIDATE
   local
      open Dynlib

      val dlh = Dynlib.dlopen {lib = "libcinfo.so",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }

      val dlxh = Dynlib.dlopen {lib = "",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }

      fun syminf s = Dynlib.dlsym dlh s;
      fun symp s = Dynlib.cptr (syminf s);

      fun ssym s = Dynlib.dlsym dlxh s;
      fun ssymp s = Dynlib.cptr (ssym s);

      val get_constants = 
           Dynlib.app1
              (syminf "mmap_constants")
                 : unit -> (Dynlib.cptr * word * word * word * word)

      val get_prot = 
           Dynlib.app1
              (syminf "mmap_prot")
                 : unit -> (word * word * word * word)

      val get_flags = 
           Dynlib.app1
              (syminf "mmap_flags")
                 : unit -> (word * word * word * word * 
                            word * word * word * word *
                            word * word * word * word)

      val (MAP_FAILED, PAGE_SIZE, MS_ASYNC_val, MS_SYNC_val, MS_INVALIDATE_val) = get_constants ()

      val (MAP_SHARED_val,
           MAP_PRIVATE_val,
           MAP_ANONYMOUS_val,
           MAP_FIXED_val,
           MAP_GROWSDOWN_val,
           MAP_LOCKED_val,
           MAP_HUGETLB_val,
           MAP_NONBLOCK_val,
           MAP_NORESERVE_val,
           MAP_POPULATE_val,
           MAP_STACK_val,
           MAP_UNINITIALIZED_val) = get_flags ()

      val sync_flagvals =
            [(MS_ASYNC_val,MS_ASYNC),
             (MS_SYNC_val,MS_SYNC),
             (MS_INVALIDATE_val,MS_INVALIDATE)];

      val flagvals = 
             [(MAP_SHARED_val,MAP_SHARED),
              (MAP_PRIVATE_val,MAP_PRIVATE),
              (MAP_ANONYMOUS_val,MAP_ANONYMOUS),
              (MAP_FIXED_val,MAP_FIXED),
              (MAP_GROWSDOWN_val,MAP_GROWSDOWN),
              (MAP_LOCKED_val,MAP_LOCKED),
              (MAP_HUGETLB_val,MAP_HUGETLB),
              (MAP_NONBLOCK_val,MAP_NONBLOCK),
              (MAP_NORESERVE_val,MAP_NORESERVE),
              (MAP_POPULATE_val,MAP_POPULATE),
              (MAP_STACK_val,MAP_STACK),
              (MAP_UNINITIALIZED_val,MAP_UNINITIALIZED)]

      val    (PROT_EXEC_val,
              PROT_READ_val,
              PROT_WRITE_val,
              PROT_NONE_val) = get_prot ()

      val protvals =
             [(PROT_EXEC_val,PROT_EXEC),
              (PROT_READ_val,PROT_READ),
              (PROT_WRITE_val,PROT_WRITE),
              (PROT_NONE_val,PROT_NONE)]
      fun lookup_ vs =
          Option.valOf o
            (fn w => 
               List.foldl
                  (fn (_,acc as (SOME n)) => acc
                    | ((n,w'),_) =>
                        if w' = w
                           then SOME n
                           else NONE) NONE vs)
      val flagWord = lookup_ flagvals
      val sync_flagWord = lookup_ sync_flagvals
      val protWord = lookup_ protvals
      fun toWord conv =
         let fun iter w [] = w
               | iter w (f::fs) =
                   iter (Word.orb(conv f,w)) fs
         in iter 0w0
         end
      val flagsWord = toWord flagWord
      val sync_flagsWord = toWord sync_flagWord
      val protWord = toWord protWord

      open Lightning32

      val () = init_jit();

      val jit_ = jit_new_state();

      fun jit_fprolog jit_ =
         jit_note (jit_,NULL,0w0)
         before jit_prolog (jit_);

      val wsz = Word.fromInt (WORDSIZE div 8);

      val mmap_call = jit_fprolog (jit_);
      val v = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, v);

      val _  = jit_ldxi (jit_, V1, V0, wsz * 0w0); (* V1 = Field(v,0) *)
      val _  = jit_ldxi (jit_, V2, V0, wsz * 0w1); (* V2 = Field(v,1) *)
      val _  = jit_rshi (jit_, V2, V2, 0w1); (* Long_val(V2) *)
      val _  = jit_ldxi (jit_, R0, V0, wsz * 0w2); (* R0 = Field(v,2) *)
      val _  = jit_rshi (jit_, R0, R0, 0w1); (* Long_val(R0) *)
      val _  = jit_ldxi (jit_, R1, V0, wsz * 0w3); (* R1 = Field(v,3) *)
      val _  = jit_rshi (jit_, R1, R1, 0w1); (* Long_val(R1) *)
      val _  = jit_ldxi (jit_, R2, V0, wsz * 0w4); (* R2 = Field(v,4) *)
      val _  = jit_rshi (jit_, R2, R2, 0w1); (* Long_val(R2) *)
      val _  = jit_ldxi (jit_, V0, V0, wsz * 0w5); (* V0 = Field(v,5) *)
      val _  = jit_rshi (jit_, V0, V0, 0w1); (* Long_val(V0) *)
      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_pushargr (jit_, V2);
      val _ = jit_pushargr (jit_, R0);
      val _ = jit_pushargr (jit_, R1);
      val _ = jit_pushargr (jit_, R2);
      val _ = jit_pushargr (jit_, V0);
      val _ = jit_finishi (jit_, (ssymp "mmap"));
      val _ = jit_retval (jit_, R0);

      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val munmap_call = jit_fprolog (jit_);
      val b = jit_arg (jit_);
      val l = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, b);
      val () = jit_getarg (jit_, V1, l);
      val _  = jit_rshi (jit_, V1, V1, 0w1); (* Long_val(V1) *)

      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V0);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_finishi (jit_, (ssymp "munmap"));
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val mprotect_call = jit_fprolog (jit_);
      val b = jit_arg (jit_);
      val l = jit_arg (jit_);
      val flags = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, b);
      val () = jit_getarg (jit_, V1, l);
      val _  = jit_rshi (jit_, V1, V1, 0w1); (* Long_val(V1) *)
      val () = jit_getarg (jit_, V2, flags);
      val _  = jit_rshi (jit_, V2, V2, 0w1); (* Long_val(V2) *)

      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V0);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_pushargr (jit_, V2);
      val _ = jit_finishi (jit_, (ssymp "mprotect"));
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val msync_call = jit_fprolog (jit_);
      val b = jit_arg (jit_);
      val l = jit_arg (jit_);
      val flags = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, b);
      val () = jit_getarg (jit_, V1, l);
      val _  = jit_rshi (jit_, V1, V1, 0w1); (* Long_val(V1) *)
      val () = jit_getarg (jit_, V2, flags);
      val _  = jit_rshi (jit_, V2, V2, 0w1); (* Long_val(V2) *)

      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V0);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_pushargr (jit_, V2);
      val _ = jit_finishi (jit_, (ssymp "msync"));
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val _ = jit_emit (jit_);

      val mmap_add = jit_address (jit_,mmap_call);
      val mprotect_add = jit_address (jit_,mprotect_call);
      val msync_add = jit_address (jit_,msync_call);
      val munmap_add = jit_address (jit_,munmap_call);

      val () = jit_clear_state (jit_);

      val mmap_ : (cptr * word * word * word * int * word) -> cptr =
          fn a =>
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in app1 mmap_add a
          end
      val munmap_ : cptr -> word -> int =
          fn a => fn b =>
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in app2 munmap_add a b
          end
      val msync_ : cptr -> word -> word -> int =
          fn a => fn b => fn c =>
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in app3 msync_add a b c
          end
      val mprotect_ : cptr -> word -> word -> int =
          fn a => fn b => fn c =>
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in app3 mprotect_add a b c
          end
   in fun mmap (addr,size,prot,flags,fd,offs) =
          let val flags = flagsWord flags
              val prot = protWord prot
              val rv = mmap_ (addr,size,prot,flags,FDSet.toInt fd,offs)
          in if (ValRepr.wordFromCptr rv) = (ValRepr.wordFromCptr MAP_FAILED)
                then SysErr.raiseSysErr "mmap" addr
                else rv
          end
      fun munmap (addr,size) =
          let val rv = munmap_ addr size
          in if rv = 0
                then ()
                else SysErr.raiseSysErr "munmap"
          end
      fun msync (addr,size,flags) =
          let val rv = msync_ addr size (sync_flagsWord flags)
          in if rv = 0
                then ()
                else SysErr.raiseSysErr "msync"
          end
      fun mprotect (addr,size,flags) =
          let val rv = mprotect_ addr size (protWord flags)
          in if rv = 0
                then ()
                else SysErr.raiseSysErr "mprotect"
          end
      fun mmapMappedArray (addr,size,prot,flags,fd,offs) =
          let val flags = flagsWord flags
              val prot = protWord prot
              val rv = mmap_ (addr,size,prot,flags,FDSet.toInt fd,offs)
          in if (ValRepr.wordFromCptr rv) = (ValRepr.wordFromCptr MAP_FAILED)
                then SysErr.raiseSysErr "mmapMappedArray"
                else MappedWord8Array.alloc_final ((ssymp "munmap"),2,rv,(Word.toInt size))
          end
      val PAGE_SIZE = PAGE_SIZE
   end
end

