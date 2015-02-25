val _ = load "SigAction";
val _ = load "SigHandler";
val _ = load "PSelect";
val _ = load "MappedNativeWordRegister";

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
   fun waitOnSigs (reg,value,hndlrs,sigidx,ss,tout) =
      let fun iter () = 
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

      in iter ()
      end
end

fun mkHandlers (sig1,sig2,pid) =
   let val hndlrs = SigHandler.sigaction 2
       val sig1_idx = 0
       val sig2_idx = 1
       val sigs = [sig1, sig2]
       val _ = ignoreSignals sigs
       val oldss = unBlockSignals sigs
       val mask = SigSet.sigprocmask
                     (SigSet.SIG_BLOCK,
                      SOME (SigSet.sigset sigs))
       val _ = setSigaction hndlrs (sig1,sig1_idx)
       val _ = setSigaction hndlrs (sig2,sig2_idx)
       val reg1 = SigHandler.si_int_reg (hndlrs,sig1_idx)
       val reg2 = SigHandler.si_int_reg (hndlrs,sig2_idx)
       fun block_on_read n =
          let val value = Word.fromInt n
          in waitOnSigs (reg1,value,hndlrs,sig1_idx,mask,TimeSpec.fromMilliseconds 1000)
          end
       fun block_on_write n =
          let val value = Word.fromInt n
          in waitOnSigs (reg2,value,hndlrs,sig2_idx,mask,TimeSpec.fromMilliseconds 1000)
          end
       fun signal_read n =
          let val value = Word.fromInt n
          in SigAction.sigqueue (pid, sig2, ValRepr.longFromWord (value))
          end
       fun signal_write n =
          let val value = Word.fromInt n
          in SigAction.sigqueue (pid, sig1, ValRepr.longFromWord (value))
          end
   in (reg1,reg2,block_on_read,block_on_write,signal_read,signal_write)
   end
