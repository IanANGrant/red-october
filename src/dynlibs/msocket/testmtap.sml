val _ = load "Time";
val _ = load "Process";
val _ = load "Unix";
val _ = load "Socket";
val _ = load "MemDebug";
val _ = load "Values";
val _ = load "PSelect";

local
   val nofds = FDSet.fromList[]
   val nosigs = SigSet.sigset[]
in fun readBuf tout buf n = fn fd => 
      let val (nfds,rfds,_,_) =
               PSelect.pselect 
                   {maxfd = 1 + FDSet.toInt fd,
                    rfds = FDSet.fromList[fd],
                    wfds = nofds,
                    efds = nofds,
                    tout = TimeSpec.fromMilliseconds tout,
                    ss = nosigs}
          val nr = case nfds
                     of 1 => Socket.readVec (fd, {buf = buf, ofs = 0, size = SOME n})
                      | _ => 0
      in nr
      end
end

fun readpackets bufsz tout max =
  let val buf = Word8Vector.tabulate (bufsz,fn _ => 0w0)
      val proc = Unix.execute
                      ("./mtap",
                           ["-vvvv", "-h",
                            "-w", "5", (* "-S", "/mtaptestshm", *)
                            "-U", "/etc/qemu2-ifup",
                            "-e", "/tmp/mtap-output"])
      val fds = Unix.unixFDsOf proc
      val infd = #1 fds
      val done = ref false
      val total = ref 0
  in while not (!done) andalso !total < max
        do let val nr = readBuf tout buf bufsz infd
           in total := !total + nr;
              if nr > 0
                 then (print ("read "^(Int.toString nr)^" bytes\n");
                       Values.dumpb buf;
                       print "\n")
                 else ()
           end handle Interrupt => done := true;
     print "Shutting down mtap process ... ";
     Unix.kill(proc, Signal.int);
     Unix.reap proc;
     print "\nDone.\n"
  end;

val st = readpackets 256 (10*60*1000) (8192*1024);
