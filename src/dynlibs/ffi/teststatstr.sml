val _ = load "Process";
val _ = load "Socket";
val _ = load "Time";
val _ = load "MappedStruct";
val _ = load "ITimer";
val _ = load "SigAction";
val _ = load "SigHandler";
val _ = load "PSelect";
val _ = load "MemDebug";

val _ = MemDebug.debug_set false;

open MappedStruct;

val _ = MemDebug.report_alloc("testing");

val bufStr = String.implode o
             (List.map (Char.chr o Word8.toInt)) o
             (Word8Vector.foldr op :: [])

val strBuf = Word8Vector.fromList o
             (List.map (Word8.fromInt o Char.ord)) o
             String.explode

fun writeStr fd s =
   (Socket.writeVec
     (fd, {buf = strBuf s,
           ofs = 0,
           size = NONE}));

local
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
end

val clkid = RTSignals.CLOCK_REALTIME;
val notify = SigEvent.SIGEV_SIGNAL;
val nrtclocks = 20;
fun add_signal s n =
   Signal.fromWord
     (Signal.toWord s + (Word.fromInt n));

val rtsignal0 = RTSignals.rtmin;
val value = SigEvent.SIval_int 0;

val res = ITimer.clock_getres (clkid);

fun iter acc 0 = acc
  | iter (acc1,acc2) n =
      let val n' = n - 1
          val s' = add_signal rtsignal0 n'
          val sev = SigEvent.sigevent
                      (SigEvent.SIGEV_SIGNAL,
                       s',
                       SigEvent.SIval_int n')
          val itimer = ITimer.timer_create clkid sev;
      in iter (itimer::acc1,s'::acc2) n'
      end

val (itimerlist,rt_sigs) = iter ([],[]) nrtclocks;
val itimers = Vector.fromList itimerlist;

val rt_hndlrs = SigHandler.sigaction nrtclocks;
val rtmin_idx = 0;

val misc_hndlrs = SigHandler.sigaction 2;
val int_idx = 0;
val usr1_idx = 1;
val inth = SigHandler.get_cptr (misc_hndlrs,int_idx);
val usr1h = SigHandler.get_cptr (misc_hndlrs,usr1_idx);
val misc_sigs = [Signal.int, Signal.usr1];

val printSigs = fn hndlrs =>
      print ("set_sigs: "^(Word.toString
                (SigHandler.set_sigs hndlrs))^"\n");

fun printSigSetList hndlrs =
   let val w = SigHandler.set_sigs hndlrs
       fun iter s _ 0w0 = s
         | iter s n w =
                let val s' = 
                   if Word.andb(w,0w1) = 0w1
                      then (String.str(Char.chr(Char.ord #"A" + n)))
                      else "-"
                in iter (s^s') (n+1) (Word.>>(w,0w1))
                end
   in iter "" 0 w
   end

open SigAction;

val ignoreAllRTSignals =
  fn () =>
      List.app
       (fn s =>
          ignore 
           (sigaction
            (s,
            {sa_flags = [],
             sa_mask = SigSet.sigset [],
             sa_handler = SIG_IGN}))) rt_sigs;

local
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
end

val now = ITimer.clock_gettime (RTSignals.CLOCK_REALTIME);

val hertz = TimeSpec.timespec (0w1,0w0);

fun milliSecondsFromNow n =
   TimeSpec.+(ITimer.clock_gettime (RTSignals.CLOCK_REALTIME),
              TimeSpec.fromMilliseconds n);

val timescale = 
   Vector.tabulate
     (nrtclocks,
      fn i =>
         (TimeSpec.scale_freq (hertz,i+1)))

fun enabletimer (n,when) =
   ignore
    (ITimer.timer_settime
     (Vector.sub(itimers,n)) [RTSignals.TIMER_ABSTIME]
       (ITimerSpec.itimerspec(Vector.sub(timescale,n),when)))

fun disabletimer n =
   let val itsoff = ITimerSpec.itimerspec
               (TimeSpec.timespec (0w00,0w0),
                TimeSpec.timespec (0w00,0w0))
   in ignore
        (ITimer.timer_settime
          (Vector.sub(itimers,n)) [] itsoff)
   end

fun disableAllTimers () =
   let fun iter 0 = ()
         | iter i =
             let val n = nrtclocks - i
             in disabletimer n;
                iter (i-1)
             end
   in iter nrtclocks
   end

exception UsrSignal of int * word

fun print_misc_sig (hndlrs,signo) =
   (print ("\nsig_signal:"^
              (Word.toString
                (Option.valOf
                  (SigHandler.sig_signal(hndlrs,signo)))));
    print ("\nsi_code:"^
             (Word.toString
               (Option.valOf
                 (SigHandler.si_code(hndlrs,signo)))));
    print ("\nsi_pid:"^
             (Word.toString
               (Option.valOf
                 (SigHandler.si_pid(hndlrs,signo)))));
    print ("\nsi_ptr:"^
             (Word.toString
                (Option.valOf
                   (SigHandler.si_ptr(hndlrs,signo))))^"\n"));

fun print_rt_sig (hndlrs,signo) =
   (print ("\nsig_signal:"^
              (Word.toString
                (Option.valOf
                  (SigHandler.sig_signal(hndlrs,signo)))));
    print ("\nsi_code:"^
             (Word.toString
               (Option.valOf
                 (SigHandler.si_code(hndlrs,signo)))));
    print ("\nsi_utime:"^
             (Word.toString
               (Option.valOf
                 (SigHandler.si_utime(hndlrs,signo)))));
    print ("\nsi_stime:"^
             (Word.toString
                (Option.valOf
                   (SigHandler.si_stime(hndlrs,signo)))));
    print ("\nsi_overrun:"^
             (Word.toString
               (Option.valOf
                 (SigHandler.si_overrun(hndlrs,signo)))));
    print ("\nsi_timerid:"^
             (Word.toString
                (Option.valOf
                   (SigHandler.si_timerid(hndlrs,signo))))^"\n"));

fun printCounts hndlrs n =
   let val _ = printSigs hndlrs
       fun iter 0 = ()
         | iter i =
            let val n' = n - i
            in print ("count["^(Int.toString n')^"]: "^
                 (Word.toString
                   ((SigHandler.si_count(hndlrs,n'))))^"\n");
               iter (i - 1)
            end
   in iter n
   end

val printrtcounts =
  fn () =>
    printCounts rt_hndlrs nrtclocks;

val timers =
   let val (l,_) =
         (List.foldr
           (fn (s,(acc,i)) =>
              let val n = i-1
              in ((s,n)::acc,n)
              end)
           ([],nrtclocks)
          rt_sigs)
   in ref l
   end

fun starttimer (s,n) when =
   (enabletimer (n, when);
    push_sigact (s, 
        sigaction
          (s,
           {sa_flags = [SA_SIGINFO],
            sa_mask = SigSet.sigset [],
            sa_handler = SA_sigaction
                             (SigHandler.get_cptr
                                 (rt_hndlrs,n))}));
    ignore (SigSet.sigprocmask
              (SigSet.SIG_UNBLOCK,
               SigSet.sigset [s])))

val _ = ignoreAllRTSignals ();

val oldss = SigSet.sigprocmask
              (SigSet.SIG_UNBLOCK,
               SigSet.sigset (misc_sigs));

val blockAllRTSigs =
   fn () => SigSet.sigprocmask
              (SigSet.SIG_BLOCK,
               SigSet.sigset (rt_sigs));

val _ = blockAllRTSigs ();

val _ = printrtcounts();

val timerq = ref (!timers)

fun startnexttimer when =
   case (!timerq)
     of [] => ()
      | (t::ts) => (starttimer t when;
                    timerq := ts)

val _ = 
  let val when = milliSecondsFromNow 1000
  in while (!timerq) <> []
     do startnexttimer when
  end;

fun foldsigs f acc = fn (n,hndlrs) =>
   let val flags = SigHandler.set_sigs hndlrs
       fun iter acc 0 = acc
         | iter acc i = 
             let val n' = n - i
                 val acc = if Word.andb(Word.<<(0w1,Word.fromInt n'),flags) = 0w0
                              then f (n',acc) else acc
             in iter acc (i-1)
             end
   in iter acc n
   end

fun resetcounts (n,hndlrs) =
   fn acc =>
      foldsigs (fn (i,acc) => 
                   let val c = Word.toInt (SigHandler.si_count (hndlrs,i))
                       val acc = if c > 1
                                    then (i,c)::acc
                                    else acc
                   in SigHandler.reset_count (hndlrs,i);
                      acc
                   end)
            acc
            (n,hndlrs)

fun listsigs (n,hndlrs) =
   let val flags = SigHandler.set_sigs hndlrs
       fun iter acc 0 = acc
         | iter acc i = 
             let val n' = n - i
                 val f = if Word.andb(Word.<<(0w1,Word.fromInt n'),flags) = 0w0
                            then "-" else "*"
             in iter (acc^f) (i-1)
             end
   in iter "" n
   end

val overruns : (int * int) list ref = ref [];

fun loop 0 = print ("Done.\n")
  | loop n = 
       (while not (SigHandler.isset misc_hndlrs)
              andalso not (SigHandler.isset rt_hndlrs)
           do Process.sleep (Time.fromMilliseconds 1000);
         if SigHandler.isset rt_hndlrs
            then (print ((listsigs (nrtclocks,rt_hndlrs))^"\n");
                  overruns := (resetcounts (nrtclocks,rt_hndlrs) (!overruns));
                  SigHandler.reset(rt_hndlrs))
            else ();
         if SigHandler.isset_sig (misc_hndlrs,int_idx)
            then (blockAllRTSigs ();
                  ignoreAllRTSignals();
                  disableAllTimers();
                  print_misc_sig(misc_hndlrs,int_idx);
                  raise Interrupt)
            else (if SigHandler.isset_sig (misc_hndlrs,usr1_idx)
                    then let val arg = Option.valOf (SigHandler.si_ptr(misc_hndlrs,usr1_idx))
                         in SigHandler.reset_sig(misc_hndlrs,usr1_idx);
                            raise UsrSignal (1,arg)
                         end
                    else ();
                 if n = 990 then SigAction.sigqueue
                                         (SigAction.getpid (),
                                          Signal.usr1,
                                          ValRepr.longFromWord (Word.fromInt (n-1)))
                           else ();
                 loop (n-1))) handle UsrSignal (n,w) =>
                                 (print ("usr"^(Int.toString n)^
                                        " signal with arg "^(Int.toString(Word.toInt w))^
                                        "\nRe-entering loop ...\n");
                                        loop (Word.toInt w));

val _ =
   push_sigact
      (Signal.int,
         sigaction
        (Signal.int,
        {sa_flags = [SA_SIGINFO],
         sa_mask = SigSet.sigset [],
         sa_handler = SA_sigaction (inth)}));

val _ =
   push_sigact
       (Signal.usr1,
             sigaction
             (Signal.usr1,
             {sa_flags = [SA_SIGINFO],
              sa_mask = SigSet.sigset [],
              sa_handler = SA_sigaction (usr1h)}));

val _ = 
   let 
   in loop 1000
       handle Interrupt =>
                (restore_sigacts ();
                 print ("\nconsole interrupt\n"))
   end

val _ = SigSet.sigprocmask(SigSet.SIG_SETMASK,oldss);

val _ = Vector.appi
          (fn (idx,itimer) =>
                  (print ("Timer "^(Int.toString idx)^" overrun: "^
                         (Int.toString (ITimer.timer_getoverrun itimer))^" count: "^
                          (Word.toString (SigHandler.si_count (rt_hndlrs,idx)))^"\n");
                   disabletimer idx;
                   ITimer.timer_delete itimer))
           itimers;

val pid = SigAction.getpid ();
val ppid = SigAction.getppid ();

val _ = print ("Waiting (for 5 seconds) for a console interrupt. Press Ctl-C to continue\n")
val siginfo_int =
      SigHandler.sigtimedwait (SigSet.sigset [Signal.int]) (TimeSpec.fromSeconds 5)

val _ = print ("Waiting (forever) for another console interrupt. Press Ctl-C to continue\n")

val (signo2,siginfo2) = SigHandler.sigwaitinfo (SigSet.sigset [Signal.int]);

val _ =
   push_sigact
      (Signal.int,
         sigaction
        (Signal.int,
        {sa_flags = [SA_SIGINFO],
         sa_mask = SigSet.sigset [],
         sa_handler = SA_sigaction (inth)}));

val _ = print ("Waiting (forever) for yet another console interrupt. Press Ctl-C to continue ...\n")

val () = SigSet.sigsuspend (SigSet.sigset []);

val _ = MemDebug.report_alloc("testing");
