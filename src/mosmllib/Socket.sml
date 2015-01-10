(* Ken Friis Larsen (ken@friislarsen.net) 1998-10-26 *)

(* The initial implementation was financed by the PROSPER project. *)

(* Beautification and documentation by sestoft@dina.kvl.dk 
   1999-02-01, 2000-05-16, 2000-10-24 *)

structure Socket :> Socket =
struct

    prim_type sock_  (* an abstract value containing a socket descriptor   *)
    prim_type addr    (* union saddr = sockaddr + sockaddr_un + sockaddr_in *)
    
    type file_perm = int;

    type unixfd = sock_ ref;

    datatype ('addressfam, 'socktype) sock = SOCK of sock_
    datatype 'addressfam sock_addr = ADDR of addr

    (* witness types for the socket parameter *)
    type dgram     = unit
    type 'a stream = unit
    type passive   = unit
    type active    = unit

    (* witness types for the addressfam parameter *)
    type pf_file = unit
    type pf_inet = unit

datatype open_flag =
    O_APPEND                       (* `open' for appending *)
  | O_BINARY                       (* `open' in binary mode *)    
  | O_CREAT                        (* create the file if nonexistent *)
  | O_EXCL                         (* fails if the file exists *)
  | O_RDONLY                       (* `open' read-only *)
  | O_RDWR                         (* `open' for reading and writing *)
  | O_TEXT                         (* `open' in text mode *)
  | O_TRUNC                        (* truncate the file to 0 if it exists *)
  | O_WRONLY                       (* `open' write-only *)

    local 
	open Dynlib
	val hdl  = dlopen {lib = "libmsocket.so",
			   flag = RTLD_LAZY, global = false}
	val symb = Dynlib.dlsym hdl
	fun app1 name = Dynlib.app1 (symb ("msocket_"^name))
	fun app2 name = Dynlib.app2 (symb ("msocket_"^name))
	fun app3 name = Dynlib.app3 (symb ("msocket_"^name))
	fun app4 name = Dynlib.app4 (symb ("msocket_"^name))
	fun app5 name = Dynlib.app5 (symb ("msocket_"^name))

	val constants_ : unit  -> int * int * int * int 
	    * int * int * int * int 
	    * int * int             
	           = app1 "constants" 

	val newfileaddr_ : string -> addr       = app1 "newfileaddr"
	val newinetaddr_ : string ->int -> addr = app2 "newinetaddr"

	            (* domain, type *)
	val socket_   : int -> int -> sock_      = app2 "socket"

	val accept_   : sock_ -> sock_ * addr    = app1 "accept"
	val bind_     : sock_ -> addr -> unit    = app2 "bind"
	val connect_  : sock_ -> addr -> unit    = app2 "connect"
	val listen_   : sock_ -> int -> unit     = app2 "listen"
	val close_    : sock_ -> unit            = app1 "close"
	val shutdown_ : sock_ -> int -> unit     = app2 "shutdown"

	val getinetaddr_ : addr -> string        = app1 "getinetaddr"

	type buff = Word8Vector.vector	(* Even for arrays; see a2v *)

	(* The integer arguments are: offset, size, flags *)

	val send_     : sock_ -> buff -> int -> int -> int -> int 
	                = app5 "send"
	val sendto_   : sock_ -> buff -> int * int -> int -> addr -> int
	                = app5 "sendto"
	val recv_     : sock_ -> buff -> int -> int -> int -> int 
                        = app5 "recv"
	val recvfrom_ : sock_ -> buff -> int -> int -> int -> int * addr
                        = app5 "recvfrom"

	val fddesc_ : int -> unixfd
                        = app1 "fddesc"

        val descfd_ : unixfd -> int
                        = app1 "descfd"

	val select_   : sock_ vector -> sock_ vector -> sock_ vector -> 
                        int -> int -> sock_ list * sock_ list * sock_ list
                        = app5 "select"

	val pselect_   : sock_ vector -> sock_ vector -> sock_ vector -> 
                        int * int -> Signal.signal vector -> sock_ list * sock_ list * sock_ list
                        = app5 "pselect"
			
	val desccmp_  : sock_ -> sock_ -> int = app2 "desccmp";

	val (SOCK_STREAM,
	     SOCK_DGRAM,
	     PF_UNIX,
	     PF_INET,
	     NO_RECVS_,
	     NO_SENDS_, 
	     NO_RECVS_OR_SENDS_,
	     MSG_OOB,
	     MSG_PEEK,
	     MSG_DONTROUTE) = constants_ ()
	
	val (MSG_OOB,
	     MSG_PEEK,
	     MSG_DONTROUTE) = (Word.fromInt MSG_OOB,
			       Word.fromInt MSG_PEEK,
			       Word.fromInt MSG_DONTROUTE)

	prim_val vector_ : int -> Word8Vector.vector = 1 "create_string"

	fun extract vec len = 
	    Word8VectorSlice.vector(Word8VectorSlice.slice(vec, 0, SOME len))

prim_val sys_open :
  string -> open_flag list -> file_perm -> int = 3 "sys_open"
        (* Open a file. The second argument is the opening mode.
           The third argument is the permissions to use if the file
           must be created. The result is a file descriptor opened on the
           file. *)
prim_val sys_close :
  int -> unit = 1 "sys_close"
        (* Close a file descriptor. *)

	val read_    : sock_ -> buff -> int -> int -> int
	                = app4 "read"

	val write_   : sock_ -> buff -> int * int -> int
	                = app3 "write"

	val fdclose_ : sock_ -> int
	                = app1 "fdclose"

	val fsync_   : sock_ -> int
	                = app1 "fsync"

	val ftruncate_   : sock_ -> int -> int
	                = app2 "ftruncate"

prim_val s_irusr : file_perm = 0 "s_irusr";
prim_val s_iwusr : file_perm = 0 "s_iwusr";
prim_val s_ixusr : file_perm = 0 "s_ixusr";

prim_val s_irgrp : file_perm = 0 "s_irgrp";
prim_val s_iwgrp : file_perm = 0 "s_iwgrp";
prim_val s_ixgrp : file_perm = 0 "s_ixgrp";

prim_val s_iroth : file_perm = 0 "s_iroth";
prim_val s_iwoth : file_perm = 0 "s_iwoth";
prim_val s_ixoth : file_perm = 0 "s_ixoth";

prim_val s_irall : file_perm = 0 "s_irall";
prim_val s_iwall : file_perm = 0 "s_iwall";
prim_val s_ixall : file_perm = 0 "s_ixall";

prim_val s_isuid : file_perm = 0 "s_isusr";
prim_val s_isgid : file_perm = 0 "s_isgid";

    in
       val S_IRWXA = s_irall + s_iwall + s_ixall;
       val S_IRALL = s_irall;
       val S_IWALL = s_iwall;
       val S_IXALL = s_ixall;
       val S_IRWXU = s_irusr + s_iwusr + s_ixusr;
       val S_IRUSR = s_irusr;
       val S_IWUSR = s_iwusr;
       val S_IXUSR = s_ixusr;
       val S_IRWXG = s_irgrp + s_iwgrp + s_ixgrp;
       val S_IRGRP = s_irgrp;
       val S_IWGRP = s_iwgrp;
       val S_IXGRP = s_ixgrp;
       val S_IRWXO = s_iroth + s_iwoth + s_ixoth;
       val S_IROTH = s_iroth;
       val S_IWOTH = s_iwoth;
       val S_IXOTH = s_ixoth;

        val fdopen : string * open_flag list * file_perm -> unixfd = 
                       fn (fnm,flags, perms) => 
                          let val fd = sys_open fnm flags perms
                              (* val _ = TextIO.print ("fdopen: fd = "^(Int.toString fd)^"\n") *)
                          in fddesc_ fd end

        val fdclose : unixfd -> int = fn (ref fd) => fdclose_ fd

        val fsync : unixfd -> int = fn (ref fd) => fsync_ fd

        val ftruncate : unixfd -> int -> int = fn (ref fd) => ftruncate_ fd

	fun getinetaddr (ADDR a : pf_inet sock_addr) = getinetaddr_ a

	fun fileAddr s = ADDR(newfileaddr_ s)
	fun inetAddr s port = ADDR(newinetaddr_ s port)

	fun fileStream () = SOCK(socket_ PF_UNIX SOCK_STREAM)
	fun fileDgram ()  = SOCK(socket_ PF_UNIX SOCK_DGRAM)
	fun inetStream () = SOCK(socket_ PF_INET SOCK_STREAM)
	fun inetDgram ()  = SOCK(socket_ PF_INET SOCK_DGRAM)

	fun accept (SOCK sock) = 
	    let val (s,a) = accept_ sock
	    in  (SOCK s, ADDR a)
	    end

	fun bind (SOCK sock, ADDR addr) = bind_ sock addr

	fun connect (SOCK sock, ADDR addr) = connect_ sock addr

	fun listen (SOCK sock, queuelen) = listen_ sock queuelen

	fun close (SOCK sock) = close_ sock

	datatype shutdown_mode = NO_RECVS | NO_SENDS | NO_RECVS_OR_SENDS
	
	fun shutdown (SOCK sock, NO_RECVS) = 
	    shutdown_ sock NO_RECVS_
	  | shutdown (SOCK sock, NO_SENDS) = 
	    shutdown_ sock NO_SENDS_
	  | shutdown (SOCK sock, NO_RECVS_OR_SENDS) = 
	    shutdown_ sock NO_RECVS_OR_SENDS_

	(* Buffers = subvectors and subarrays for output and input *)
   
	type 'a buf = {buf : 'a, ofs : int, size : int option}

	fun chkvbuf {buf : Word8Vector.vector, ofs : int, size : int option} = 
	    let val len = Word8Vector.length buf
		val sz  = case size of NONE => len - ofs | SOME n => n
	    in
		if ofs<0 orelse sz<0 orelse ofs+sz>len then raise Subscript
		else (ofs, sz) 
	    end		

	fun chkabuf {buf : Word8Array.array, ofs : int, size : int option} = 
	    let val len = Word8Array.length buf
		val sz  = case size of NONE => len - ofs | SOME n => n
	    in
		if ofs<0 orelse sz<0 orelse ofs+sz>len then raise Subscript
		else (ofs, sz) 
	    end		

	(* Output flags *)
	
	type out_flags = {don't_route : bool, oob : bool}

	fun getoflags {don't_route, oob} =
	    Word.toInt (Word.orb(if don't_route then MSG_DONTROUTE else 0w0,
				 if oob         then MSG_OOB       else 0w0))

	fun sendVec (SOCK sock, vbuf) =
	    let val (ofs, sz) = chkvbuf vbuf 
	    in send_ sock (#buf vbuf) ofs sz 0 end

	fun sendVec' (SOCK sock, vbuf, oflags) =
	    let val (ofs, sz) = chkvbuf vbuf 
	    in send_ sock (#buf vbuf) ofs sz (getoflags oflags) end

	fun sendVecTo (SOCK sock, ADDR addr, vbuf) =
	    let val (ofs, sz) = chkvbuf vbuf 
	    in sendto_ sock (#buf vbuf) (ofs, sz) 0 addr end

	fun sendVecTo' (SOCK sock, ADDR addr, vbuf, oflags) =
	    let val (ofs, sz) = chkvbuf vbuf 
	    in sendto_ sock (#buf vbuf) (ofs, sz) (getoflags oflags) addr end

	(* Warning: this crucially depends on the representation of a
  	   Word8Array.array as a reference to a Word8Vector.vector;
	   see mosml/src/mosmllib/Word8Array.mlp: *)

	fun a2v (a : Word8Array.array) = 
	    let prim_val deref : Word8Array.array -> Word8Vector.vector
		= 1 "field0";
	    in deref a end

	fun sendArr (SOCK sock, abuf) =
	    let val (ofs, sz) = chkabuf abuf 
	    in send_ sock (a2v (#buf abuf)) ofs sz 0 end

	fun sendArr' (SOCK sock, abuf, oflags) =
	    let val (ofs, sz) = chkabuf abuf 
	    in send_ sock (a2v (#buf abuf)) ofs sz (getoflags oflags) end

	fun sendArrTo (SOCK sock, ADDR addr, abuf) =
	    let val (ofs, sz) = chkabuf abuf 
	    in sendto_ sock (a2v (#buf abuf)) (ofs, sz) 0 addr end

	fun sendArrTo' (SOCK sock, ADDR addr, abuf, oflags) =
	    let val (ofs, sz) = chkabuf abuf 
	    in 
		sendto_ sock (a2v (#buf abuf)) (ofs, sz) 
		        (getoflags oflags) addr 
	    end

	(* Input flags *)

	type in_flags = {peek : bool, oob : bool}

	fun getiflags {peek, oob} =
	    Word.toInt(Word.orb(if peek then MSG_PEEK else 0w0,
				if oob  then MSG_OOB  else 0w0))

	fun recvVec (SOCK sock, len) =
	    let val vec  = vector_ len 
		val size = recv_ sock vec 0 len 0; 
	    in extract vec size end

	fun recvArr (SOCK sock, abuf) =
	    let val (ofs, sz) = chkabuf abuf
	    in recv_ sock (a2v (#buf abuf)) ofs sz 0 end

	fun recvVec' (SOCK sock, len, iflags) =
	    let val vec  = vector_ len 
		val size = recv_ sock vec 0 len (getiflags iflags) 
	    in extract vec size end

	fun recvArr' (SOCK sock, abuf, iflags) =
	    let val (ofs, sz) = chkabuf abuf
	    in recv_ sock (a2v (#buf abuf)) ofs sz (getiflags iflags) end

	fun recvVecFrom (SOCK sock, len) =
	    let val vec  = vector_ len 
		val (size, addr) = recvfrom_ sock vec 0 len 0; 
	    in (extract vec size, ADDR addr) end

	fun recvArrFrom (SOCK sock, abuf) =
	    let val (ofs, sz) = chkabuf abuf
		val (size, addr) = recvfrom_ sock (a2v (#buf abuf)) ofs sz 0
	    in (size, ADDR addr) end

	fun recvVecFrom' (SOCK sock, len, iflags) =
	    let val vec  = vector_ len
		val (size, addr) = recvfrom_ sock vec 0 len (getiflags iflags)
	    in (extract vec size, ADDR addr) end

	fun recvArrFrom' (SOCK sock, abuf, iflags) =
	    let val (ofs, sz) = chkabuf abuf
		val (size, addr) = 
		    recvfrom_ sock (a2v (#buf abuf)) ofs sz (getiflags iflags)
	    in (size, ADDR addr) end

        (* FD read/write *)

       	fun readVec (ref fd, vbuf) =
	    let val (ofs, sz) = chkvbuf vbuf 
	    in read_ fd (#buf vbuf) ofs sz end

	fun readArr (ref fd, abuf) =
	    let val (ofs, sz) = chkabuf abuf
	    in read_ fd (a2v (#buf abuf)) ofs sz end

       	fun writeVec (ref fd, vbuf) =
	    let val (ofs, sz) = chkvbuf vbuf 
	    in write_ fd (#buf vbuf) (ofs, sz) end

	fun writeArr (ref fd, abuf) =
	    let val (ofs, sz) = chkabuf abuf
	    in write_ fd (a2v (#buf abuf)) (ofs, sz) end

	(* We can let sock_desc be a synonym for sock_, and compare
	   sock_descs by looking at the value (in Unix, int) inside
	   the sock_ *)

	type sock_desc = sock_ 

	fun sockDesc (SOCK sock_) = sock_

        fun fdDesc (ref fd) = fd 

	fun sameDesc (sock1_, sock2_) = (desccmp_ sock1_ sock2_ = 0)

	fun compare (sock1_, sock2_) =
	    let val cmp = desccmp_ sock1_ sock2_ 
	    in if cmp < 0 then LESS
	       else if cmp > 0 then GREATER
	       else EQUAL
	    end

	(* Note: This must agree with the particular representation of
	   Time.time found in mosml/src/mosmllib/Time.sml: *)

        prim_val fromtime : Time.time -> real = 1 "identity"

	fun select { rds, wrs, exs, timeout } =
	    let val (tsec, tusec) = 
		    case timeout of
			NONE   => (~1,0)
		      | SOME t => 
			    let val r    = fromtime t 
				val sec  = trunc(r/1000000.0)
				val usec = trunc(r - 1000000.0 * real sec)
			    in (sec, usec) end
		val rvec = Vector.fromList rds
		val wvec = Vector.fromList wrs
		val evec = Vector.fromList exs
		val (rds', wrs', exs') = select_ rvec wvec evec tsec tusec
	    in { rds = rds', wrs = wrs', exs = exs' } end

	fun pselect { rds, wrs, exs, timeout, signals : Signal.signal list } =
	    let val (tsec, tnsec) = 
		    case timeout of
			NONE   => (~1,0)
		      | SOME t => 
			    let val r    = fromtime t 
				val sec  = trunc(r/1000000000.0)
				val nsec = trunc(r - 1000000000.0 * real sec)
			    in (sec, nsec) end
		val rvec = Vector.fromList rds
		val wvec = Vector.fromList wrs
		val evec = Vector.fromList exs
		val sigvec = Vector.fromList signals
		val (rds', wrs', exs') = pselect_ rvec wvec evec (tsec,tnsec) sigvec
	    in { rds = rds', wrs = wrs', exs = exs' } end
    end
end
