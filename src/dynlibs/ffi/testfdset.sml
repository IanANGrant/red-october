val _ = load "FDSet";

val stdin = FDSet.fromInt 0
val stdout = FDSet.fromInt 1
val stderr = FDSet.fromInt 2

val rfdl = [stdin];
val wfdl = [stdout,stderr];

val rfds = FDSet.fromList rfdl;
val wfds = FDSet.fromList wfdl;

val nfds = FDSet.lub (List.concat[rfdl,wfdl]);

val toIntList = List.map FDSet.toInt o FDSet.toList

val rfdsl' = toIntList rfds
val wfdsl' = toIntList wfds;

val wfds = FDSet.fromList wfdl;
val rfds = FDSet.fromList rfdl;

val fds = FDSet.fdset();
val _ = FDSet.dump fds;
val () = FDSet.fd_set(stdin,fds);
val isset1 = FDSet.fd_isset(stdin,fds);
val () = FDSet.fd_set(stderr,fds);
val isset2 = FDSet.fd_isset(stderr,fds);
val _ = FDSet.dump fds;
val buff = StaticStruct.fifo 7
val () = FDSet.writeFdset (buff,fds);
val fds' = FDSet.readFdset buff;
val _ = FDSet.dump fds';
val isset2' = FDSet.fd_isset(stderr,fds');
