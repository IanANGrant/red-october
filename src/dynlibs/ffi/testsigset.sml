val _ = load "Signal";
val _ = load "SigSet";
val _ = load "StaticBuffer";

val dump = StaticBuffer.dumpb_all o StaticStruct.buffer;

val dumpw = StaticBuffer.dumpw_all o StaticStruct.buffer;

open StaticStruct;

val ss = SigSet.sigset [Signal.int,Signal.alrm];
val false = SigSet.sigismember (ss,Signal.quit);
val true = SigSet.sigismember (ss,Signal.alrm);

val buff = fifo 8;

val () = SigSet.writeSigset (buff,ss);
val fr = free buff;
val _ = dump buff;
val ss2 = SigSet.readSigset buff;
val false = SigSet.sigismember (ss2,Signal.quit);
val true = SigSet.sigismember (ss2,Signal.alrm);

val _ = dump buff;


