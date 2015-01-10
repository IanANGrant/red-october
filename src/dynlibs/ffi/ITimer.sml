signature TimeSpec =
sig
   type timespec
   val timespec : word * word -> timespec
   val writeTimespec : (MappedStruct.fifo * timespec) -> unit
   val readTimespec : MappedStruct.fifo -> timespec
   val op + : timespec * timespec -> timespec
   val op - : timespec * timespec -> timespec
   val scale_period : timespec * int -> timespec
   val scale_freq : timespec * int -> timespec
   val fromHertz : real -> timespec
   val fromSeconds : int -> timespec
   val fromMilliseconds : int -> timespec
   val fromMicroseconds : int -> timespec
   val size : int
end

structure TimeSpec : TimeSpec =
struct
   type timespec = {tv_sec : word, tv_nsec : word}
   fun timespec (sec,nsec) = {tv_sec = sec, tv_nsec = nsec} : timespec
   fun fromHertz f =
      let val secs = Real.floor (1.0/f)
          val nsecs = Real.round (((1.0/f) - (Real.fromInt secs)) * 1000000000.0)
      in  {tv_sec = Word.fromInt secs, tv_nsec = Word.fromInt nsecs} : timespec
      end
   fun fromSeconds m = {tv_sec = (Word.fromInt m), tv_nsec = 0w0} : timespec
   fun fromMilliseconds m = {tv_sec = (Word.fromInt (m div 1000)),
                             tv_nsec = Word.fromInt(1000000 * (m mod 1000))} : timespec
   fun fromMicroseconds m = {tv_sec = (Word.fromInt (m div 1000000)),
                             tv_nsec = Word.fromInt(1000 * (m mod 1000000))} : timespec
   val scale_period = fn (a : timespec,m : int)
        => let val m' = (Word.fromInt m)
               val usec = (((#tv_sec a) * 0w1000000) + (#tv_nsec a) div 0w1000) * m' 
           in {tv_sec = (usec div 0w1000000),
               tv_nsec = (usec mod 0w1000000) * 0w1000}
           end
   val scale_freq = fn (a : timespec,m : int)
        => let val m' = (Word.fromInt m)
               val usec = (((#tv_sec a) * 0w1000000) + (#tv_nsec a) div 0w1000) div m' 
           in {tv_sec = (usec div 0w1000000),
               tv_nsec = (usec mod 0w1000000) * 0w1000}
           end
   val op + = fn (a : timespec,b : timespec)
        => let val nsec' = (#tv_nsec a)+(#tv_nsec b) 
               val sec' = nsec' div 0w1000000000
               val nsec = nsec' mod 0w1000000000
               val sec = (#tv_sec a)+(#tv_sec b)+sec'
           in {tv_sec = sec, tv_nsec = nsec}
           end
   val op - = fn (a : timespec,b : timespec)
        => let val nsec' = Int.-(Word.toInt(#tv_nsec a),  (Word.toInt(#tv_nsec b)))
               val (sec',nsec'') = if nsec' < 0 then (~1,Int.+(1000000000,nsec')) else (0,nsec')
               val sec'' = Int.+(Int.-(Word.toInt(#tv_sec a), (Word.toInt(#tv_sec b))), sec')
           in if sec'' < 0 then raise Fail "ITimer.-: negative time"
                           else {tv_sec = Word.fromInt sec'', tv_nsec = Word.fromInt nsec''}
           end
   fun writeTimespec (b,{tv_sec = sec, tv_nsec = nsec}) =
      (MappedStruct.writeWord32 (b,sec);
       MappedStruct.writeWord32 (b,nsec))
   fun readTimespec b =
      {tv_sec = MappedStruct.readWord32 b,
       tv_nsec = MappedStruct.readWord32 b}
   val size = 8
end

signature ITimerSpec =
sig
   type itimerspec
   val itimerspec : TimeSpec.timespec * TimeSpec.timespec -> itimerspec
   val writeItimerspec : (MappedStruct.fifo * itimerspec) -> unit
   val readItimerspec : MappedStruct.fifo -> itimerspec
   val size : int
end

structure ITimerSpec : ITimerSpec =
struct
   type itimerspec = {it_interval : TimeSpec.timespec, it_value : TimeSpec.timespec}
   fun itimerspec (interval,value) = {it_interval = interval, it_value = value} : itimerspec
   fun writeItimerspec (b,{it_interval = interval, it_value = value}) =
      (TimeSpec.writeTimespec (b,interval);
       TimeSpec.writeTimespec (b,value))
   fun readItimerspec b =
      {it_interval = TimeSpec.readTimespec b,
       it_value = TimeSpec.readTimespec b}
   val size = 2 * TimeSpec.size
end

structure SigEvent =
struct
   datatype notify =
      SIGEV_NONE
   |  SIGEV_SIGNAL
   |  SIGEV_THREAD
   |  SIGEV_THREAD_ID
   datatype value =
      SIval_int of int
    | SIval_ptr of Dynlib.cptr
   local
      val dlih = Dynlib.dlopen {lib = "libcinfo.so",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }
      fun syminf s = Dynlib.dlsym dlih s;
      val getconsts = 
           Dynlib.app1
              (syminf "sigevent_constants")
                 : unit -> (int * int * int * int * int * int *
                            word * word * word * word)
      val (size,
           offs_notify,
           offs_signo,
           offs_value,
           offs_notify_function,
           offs_notify_attributes,
           SIGEV_NONE_val,
           SIGEV_SIGNAL_val,
           SIGEV_THREAD_val,
           SIGEV_THREAD_ID_val) = getconsts ()
      fun notifyWord SIGEV_NONE = SIGEV_NONE_val
        | notifyWord SIGEV_SIGNAL = SIGEV_SIGNAL_val
        | notifyWord SIGEV_THREAD = SIGEV_THREAD_val
        | notifyWord SIGEV_THREAD_ID = SIGEV_THREAD_ID_val
      val notifyvs =
             [(SIGEV_NONE_val,SIGEV_NONE),
              (SIGEV_SIGNAL_val,SIGEV_SIGNAL),
              (SIGEV_THREAD_val,SIGEV_THREAD),
              (SIGEV_THREAD_ID_val,SIGEV_THREAD_ID)]
      val wordNotify_ =
            fn w => 
               List.foldl
                  (fn (_,acc as (SOME n)) => acc
                    | ((w',n),_) =>
                        if w' = w
                           then SOME n
                           else NONE) NONE notifyvs
      val wordNotify =
             fn w =>
                 Option.valOf (wordNotify_ w)
                  handle Option =>
                       raise Fail ("SigEvent.wordNotify: \
                                   \not a defined constant: 0wx"^
                                  (Word.toString w))
   in
      type sigevent = {notify : notify, signo : Signal.signal, value : value}
      fun sigevent (notify : notify,signo : Signal.signal ,value : value) =
                   {notify = notify, signo = signo, value = value}
      fun writeSigevent (b,{notify = notify, signo = signo, value = value}) =
         let val arr = MappedWord8Array.array(size,0w0)
             val () = MappedWord8Array.copyVec
                       (case value
                         of SIval_int i =>
                               {dst = arr,
                                di = offs_value,
                                src = WordRepr.word32Vec (Word.fromInt i)}
                          | SIval_ptr cptr =>
                               {dst = arr,
                                di = offs_value,
                                src = WordRepr.word32Vec (ValRepr.wordFromCptr cptr)})
             val () = MappedWord8Array.copyVec
                       {dst = arr, di = offs_signo, src = WordRepr.word32Vec (Signal.toWord signo)}
             val () = MappedWord8Array.copyVec
                       {dst = arr, di = offs_notify, src = WordRepr.word32Vec (notifyWord notify)}
         in MappedStruct.write (b,MappedWord8ArraySlice.full arr)
         end
      fun readSigevent b = 
        let val slc = MappedStruct.readSlice (b,size)
            val signoslc = MappedWord8ArraySlice.subslice (slc,offs_signo,SOME 4)
            val valueslc = MappedWord8ArraySlice.subslice (slc,offs_value,SOME 4)
            val notifyslc = MappedWord8ArraySlice.subslice (slc,offs_notify,SOME 4)
        in {notify = wordNotify (WordRepr.vecWord32 (MappedWord8ArraySlice.vector notifyslc)),
            signo = Signal.fromWord (WordRepr.vecWord32 (MappedWord8ArraySlice.vector signoslc)),
            value = SIval_ptr (ValRepr.cptrFromWord
                                  (WordRepr.vecWord32
                                      (MappedWord8ArraySlice.vector valueslc)))}
        end
      val size = size
   end
end

structure RTSignals =
struct
   datatype clock =
          CLOCK_REALTIME
        | CLOCK_MONOTONIC
        | CLOCK_MONOTONIC_RAW
        | CLOCK_PROCESS_CPUTIME_ID
        | CLOCK_THREAD_CPUTIME_ID
   datatype flag = TIMER_ABSTIME
   local
      val dlih = Dynlib.dlopen {lib = "libcinfo.so",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }
      fun syminf s = Dynlib.dlsym dlih s;
      val getconstants = 
           Dynlib.app1
              (syminf "rtsignals_constants")
                 : unit -> (word * word * word * word * word * word)
      val getsignals = 
           Dynlib.app1
              (syminf "rtsignals_signals")
                 : unit -> (Signal.signal * Signal.signal)
      val (CLOCK_REALTIME_val,
           CLOCK_MONOTONIC_val,
           CLOCK_MONOTONIC_RAW_val,
           CLOCK_PROCESS_CPUTIME_ID_val,
           CLOCK_THREAD_CPUTIME_ID_val,
           TIMER_ABSTIME_val) = getconstants ()
      fun clockWord CLOCK_REALTIME = CLOCK_REALTIME_val
        | clockWord CLOCK_MONOTONIC = CLOCK_MONOTONIC_val
        | clockWord CLOCK_MONOTONIC_RAW = CLOCK_MONOTONIC_RAW_val
        | clockWord CLOCK_PROCESS_CPUTIME_ID = CLOCK_PROCESS_CPUTIME_ID_val
        | clockWord CLOCK_THREAD_CPUTIME_ID = CLOCK_THREAD_CPUTIME_ID_val
      val clockvs = [(CLOCK_REALTIME_val,CLOCK_REALTIME),
                     (CLOCK_MONOTONIC_val,CLOCK_MONOTONIC),
                     (CLOCK_MONOTONIC_RAW_val,CLOCK_MONOTONIC_RAW),
                     (CLOCK_PROCESS_CPUTIME_ID_val,CLOCK_PROCESS_CPUTIME_ID),
                     (CLOCK_THREAD_CPUTIME_ID_val,CLOCK_THREAD_CPUTIME_ID)]
      fun flagWord TIMER_ABSTIME = TIMER_ABSTIME_val
      val wordFlag =
           fn w =>
              if w = TIMER_ABSTIME_val
                 then TIMER_ABSTIME
                 else raise Fail ("RTSignals.wordFlag: invalid flag: 0wx"^(Word.toString w))
      val flagsWord =
         let fun iter w [] = w
               | iter w (f::fs) = iter (Word.orb(flagWord f,w)) fs
         in iter 0w0
         end
      val wordClock_ =
            fn w => 
               List.foldl
                  (fn (_,acc as (SOME n)) => acc
                    | ((w',n),_) =>
                        if w' = w
                           then SOME n
                           else NONE) NONE clockvs
      val wordClock =
             fn w =>
                 Option.valOf (wordClock_ w)
                  handle Option =>
                       raise Fail ("RTSignals.wordClock: \
                                   \not a defined constant: 0wx"^
                                  (Word.toString w))
   in
      val (rtmin,rtmax) = getsignals ()
      fun writeClock (b,c) = MappedStruct.writeWord32(b, clockWord c)
      val readClock = wordClock o MappedStruct.readWord32
      val size = 4
      val clockWord = clockWord
      val wordClock = wordClock
      val flagsWord = flagsWord
   end
end

structure ITimer =
struct
   local open MappedStruct
         open Dynlib

      val dlh = Dynlib.dlopen
                {lib = "librt.so",
                 flag = Dynlib.RTLD_LAZY,
                 global = false}

      val dlxh = Dynlib.dlopen {lib = "",
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

      val timer_create_call = jit_fprolog (jit_);
      val clkid = jit_arg (jit_);
      val sevp = jit_arg (jit_);
      val timid = jit_arg (jit_);
      val () = jit_getarg_i (jit_, V0, clkid);
      val _  = jit_rshi (jit_, V0, V0, 0w1); (* Long_val(v) *)

      val () = jit_getarg (jit_, V1, sevp);
      val () = jit_getarg (jit_, V2, timid);

      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V0);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_pushargr (jit_, V2);
      val _ = jit_finishi (jit_,symp "timer_create");
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val timer_settime_call = jit_fprolog (jit_);
      val timid = jit_arg (jit_);
      val flags = jit_arg (jit_);
      val newitsp = jit_arg (jit_);
      val olditsp = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, timid);
      val () = jit_getarg_i (jit_, V1, flags);
      val _  = jit_rshi (jit_, V1, V1, 0w1); (* V1=Long_val(flags) *)
      val () = jit_getarg (jit_, V2, newitsp);
      val () = jit_getarg (jit_, R0, olditsp);

      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V0);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_pushargr (jit_, V2);
      val _ = jit_pushargr (jit_, R0);
      val _ = jit_finishi (jit_,symp "timer_settime");
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val timer_gettime_call = jit_fprolog (jit_);
      val timid = jit_arg (jit_);
      val itsp = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, timid);
      val () = jit_getarg (jit_, V1, itsp);

      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V0);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_finishi (jit_,symp "timer_gettime");
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val timer2arg_call = jit_fprolog (jit_);
      val timerfnp = jit_arg (jit_);
      val timid = jit_arg (jit_);
      val itsp = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, timerfnp);
      val () = jit_getarg (jit_, V1, timid);
      val () = jit_getarg (jit_, V2, itsp);

      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_pushargr (jit_, V2);
      val _ = jit_finishr (jit_, V0);
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val timer1arg_call = jit_fprolog (jit_);
      val timerfnp = jit_arg (jit_);
      val timidp = jit_arg (jit_);
      val () = jit_getarg (jit_, V0, timerfnp);
      val () = jit_getarg (jit_, V1, timidp);

      val _ = jit_prepare (jit_);
      val _ = jit_pushargr (jit_, V1);
      val _ = jit_finishr (jit_, V0);
      val _ = jit_retval (jit_, R0);

      val _ = jit_lshi (jit_, R0, R0, 0w1);
      val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
      val _ = jit_retr (jit_, R0);
      val _ = jit_epilog (jit_);

      val _ = jit_emit (jit_);

      val tccalladd = jit_address (jit_,timer_create_call);
      val tstcalladd = jit_address (jit_,timer_settime_call);
      val tgtcalladd = jit_address (jit_,timer_gettime_call);
      val timer2argcalladd = jit_address (jit_,timer2arg_call);
      val timer1argcalladd = jit_address (jit_,timer1arg_call);

      val () = jit_clear_state (jit_);

      val timer_create_ : word -> cptr -> cptr -> int = app3 tccalladd;
      val timer_settime_ : cptr -> word -> cptr -> cptr -> int = app4 tstcalladd;
      val timer_gettime_ : cptr -> cptr -> int = app2 tgtcalladd;
      val timer2arg_call : cptr -> word -> cptr -> int =
          fn a => fn b => fn c =>
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in app3 timer2argcalladd a b c
          end
      val timer1arg_call : cptr -> cptr -> int =
          fn a => fn b => 
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in app2 timer1argcalladd a b
          end;
   in
      fun timer_create clkid sev =
         let val jitref = jit_ (* to keep the state from being GCed *)
             val size = SigEvent.size + 4
             fun nbits n 0w0 = n
               | nbits n w = nbits (n+1) (Word.>>(w,0w1))
             val bits = nbits 0 (Word.fromInt (size - 1))
             val buff = fifo bits
             val timidp = writePtr buff
             val () = writeWord32 (buff,0w0)
             val sevp = writePtr buff
             val () = SigEvent.writeSigevent (buff,sev)
             val rv = timer_create_ (RTSignals.clockWord clkid) sevp timidp
         in if rv = 0
               then readAddr buff
               else SysErr.raiseSysErr "timer_create"
         end
      fun timer_delete timid =
         let val rv = timer1arg_call (symp "timer_delete") timid
         in if rv = 0
               then ()
               else SysErr.raiseSysErr "timer_delete"
         end
      fun timer_getoverrun timid =
         let val rv = timer1arg_call (symp "timer_getoverrun") timid
         in if rv < 0
               then SysErr.raiseSysErr "timer_getoverrun"
               else rv
         end
      fun timer_settime timid flags its =
         let val jitref = jit_ (* to keep the state from being GCed *)
             val buff = fifo 6
             val oldits = ITimerSpec.itimerspec 
                            (TimeSpec.timespec (0w0,0w0),
                             TimeSpec.timespec (0w0,0w0))
             val newitsp = writePtr buff
             val () = ITimerSpec.writeItimerspec (buff,its)
             val olditsp = writePtr buff
             val () = ITimerSpec.writeItimerspec (buff,oldits)
             val _ = ITimerSpec.readItimerspec buff
             val rv = timer_settime_ timid (RTSignals.flagsWord flags) newitsp olditsp
         in if rv = 0
               then ITimerSpec.readItimerspec buff
               else SysErr.raiseSysErr "timer_settime"
         end
      fun timer_gettime timid =
         let val jitref = jit_ (* to keep the state from being GCed *)
             val buff = fifo 5
             val its = ITimerSpec.itimerspec 
                            (TimeSpec.timespec (0w0,0w0),
                             TimeSpec.timespec (0w0,0w0))
             val itsp = writePtr buff
             val () = ITimerSpec.writeItimerspec (buff,its)
             val rv = timer_gettime_ timid itsp
         in if rv = 0
               then ITimerSpec.readItimerspec buff
               else SysErr.raiseSysErr "timer_gettime"
         end
      fun clock_gettime clockid =
         let val jitref = jit_ (* to keep the state from being GCed *)
             val buff = fifo 5
             val ts = TimeSpec.timespec (0w0,0w0)
             val tsp = writePtr buff
             val () = TimeSpec.writeTimespec (buff,ts)
             val rv = timer2arg_call (symp "clock_gettime") (RTSignals.clockWord clockid) tsp
         in if rv = 0
               then TimeSpec.readTimespec buff
               else SysErr.raiseSysErr "clock_gettime"
         end
      fun clock_getres clockid =
         let val jitref = jit_ (* to keep the state from being GCed *)
             val buff = fifo 5
             val ts = TimeSpec.timespec (0w0,0w0)
             val tsp = writePtr buff
             val () = TimeSpec.writeTimespec (buff,ts)
             val rv = timer2arg_call (symp "clock_getres") (RTSignals.clockWord clockid) tsp
         in if rv = 0
               then TimeSpec.readTimespec buff
               else SysErr.raiseSysErr "clock_getres"
         end
      fun clock_settime clockid ts =
         let val jitref = jit_ (* to keep the state from being GCed *)
             val buff = fifo 5
             val tsp = writePtr buff
             val () = TimeSpec.writeTimespec (buff,ts)
             val rv = timer2arg_call (symp "clock_settime") (RTSignals.clockWord clockid) tsp
         in if rv = 0
               then ()
               else SysErr.raiseSysErr "clock_settime"
         end
   end
end
