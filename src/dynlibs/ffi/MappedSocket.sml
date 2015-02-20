(* Ken Friis Larsen (ken@friislarsen.net) 1998-10-26 *)

(* The initial implementation was financed by the PROSPER project. *)

(* Beautification and documentation by sestoft@dina.kvl.dk 
   1999-02-01, 2000-05-16, 2000-10-24 *)

(* Uglified by Ian Grant to work with MappedWord8Array.array buffers

   This thing should really be a functor parameterised on the
   representing type. Then we could share common code and specialise
   it to any underlying type, not just Word8Vectors and Word8Arrays. *)

structure MappedSocket :> MappedSocket =
struct

    prim_type sock_  (* an abstract value containing a socket descriptor   *)
    prim_type addr    (* union saddr = sockaddr + sockaddr_un + sockaddr_in *)
    
    type file_perm = int;

    type unixfd = sock_ ref;

    datatype ('addressfam, 'socktype) sock = SOCK of sock_
    datatype 'addressfam sock_addr = ADDR of addr

    (* witness types for the socket parameter *)
    type dgram     = unit
    type 'a stream = 'a Socket.stream
    type passive   = unit
    type active    = unit

    (* witness types for the addressfam parameter *)
    type pf_file = unit
    type pf_inet = unit

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

	type buff = Dynlib.cptr

	(* The integer arguments are: offset, size, flags *)

	val send_     : sock_ -> buff -> int -> int -> int -> int 
	                = app5 "send"
	val sendto_   : sock_ -> buff -> int * int -> int -> addr -> int
	                = app5 "sendto"
	val recv_     : sock_ -> buff -> int -> int -> int -> int 
                        = app5 "recv"
	val recvfrom_ : sock_ -> buff -> int -> int -> int -> int * addr
                        = app5 "recvfrom"

	val read_    : sock_ -> buff -> int -> int -> int
	                = app4 "read"

	val write_   : sock_ -> buff -> int * int -> int
	                = app3 "write"

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

	(* Buffers = subvectors and subarrays for output and input *)
   
	type 'a buf = {buf : 'a, ofs : int, size : int option}

	fun chkabuf {buf : MappedWord8Array.array, ofs : int, size : int option} = 
	    let val len = MappedWord8Array.length buf
		val sz  = case size of NONE => len - ofs | SOME n => n
	    in
		if ofs<0 orelse sz<0 orelse ofs+sz>len then raise Subscript
		else (ofs, sz) 
	    end		

	(* Output flags *)

	fun a2v (a : MappedWord8Array.array) = 
	    MappedWord8Array.get_cptr a

	(* Output flags *)
	
	type out_flags = {don't_route : bool, oob : bool}

	fun getoflags {don't_route, oob} =
	    Word.toInt (Word.orb(if don't_route then MSG_DONTROUTE else 0w0,
				 if oob         then MSG_OOB       else 0w0))

	fun sendArr_ (SOCK sock, abuf) =
	    let val (ofs, sz) = chkabuf abuf 
	    in send_ sock (a2v (#buf abuf)) ofs sz 0 end

	fun sendArr'_ (SOCK sock, abuf, oflags) =
	    let val (ofs, sz) = chkabuf abuf 
	    in send_ sock (a2v (#buf abuf)) ofs sz (getoflags oflags) end

	fun sendArrTo_ (SOCK sock, ADDR addr, abuf) =
	    let val (ofs, sz) = chkabuf abuf 
	    in sendto_ sock (a2v (#buf abuf)) (ofs, sz) 0 addr end

	fun sendArrTo'_ (SOCK sock, ADDR addr, abuf, oflags) =
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

	fun recvArr_ (SOCK sock, abuf) =
	    let val (ofs, sz) = chkabuf abuf
	    in recv_ sock (a2v (#buf abuf)) ofs sz 0 end

	fun recvArr'_ (SOCK sock, abuf, iflags) =
	    let val (ofs, sz) = chkabuf abuf
	    in recv_ sock (a2v (#buf abuf)) ofs sz (getiflags iflags) end

	fun recvArrFrom_ (SOCK sock, abuf) =
	    let val (ofs, sz) = chkabuf abuf
		val (size, addr) = recvfrom_ sock (a2v (#buf abuf)) ofs sz 0
	    in (size, ADDR addr) end

	fun recvArrFrom'_ (SOCK sock, abuf, iflags) =
	    let val (ofs, sz) = chkabuf abuf
		val (size, addr) = 
		    recvfrom_ sock (a2v (#buf abuf)) ofs sz (getiflags iflags)
	    in (size, ADDR addr) end

        (* FD read/write *)

	fun readArr_ (ref fd, abuf) =
	    let val (ofs, sz) = chkabuf abuf
	    in read_ fd (a2v (#buf abuf)) ofs sz end

	fun writeArr_ (ref fd, abuf) =
	    let val (ofs, sz) = chkabuf abuf
	    in write_ fd (a2v (#buf abuf)) (ofs, sz) end
    in
        val readArr : Socket.unixfd * MappedWord8Array.array Socket.buf -> int
               = Obj.magic readArr_
        val writeArr : Socket.unixfd * MappedWord8Array.array Socket.buf -> int
               = Obj.magic writeArr_
        val sendArr : ('a, Socket.active Socket.stream) Socket.sock *
                      MappedWord8Array.array Socket.buf -> int
               = Obj.magic sendArr_
        val sendArr' : ('a, Socket.active Socket.stream) Socket.sock *
                       MappedWord8Array.array Socket.buf * Socket.out_flags -> int
               = Obj.magic sendArr'_
        val sendArrTo : ('a, Socket.dgram) Socket.sock * 'a Socket.sock_addr *
                         MappedWord8Array.array Socket.buf -> int
               = Obj.magic sendArrTo_
        val sendArrTo' : ('a, Socket.dgram) Socket.sock * 'a Socket.sock_addr *
                         MappedWord8Array.array Socket.buf * Socket.out_flags -> int
               = Obj.magic sendArrTo'_
        val recvArr : ('a, Socket.active Socket.stream) Socket.sock * 
                      MappedWord8Array.array Socket.buf -> int
               = Obj.magic recvArr_
        val recvArr' : ('a, Socket.active Socket.stream) Socket.sock *
                       MappedWord8Array.array Socket.buf * Socket.in_flags -> int
               = Obj.magic recvArr'_
        val recvArrFrom  : ('a, Socket.dgram) Socket.sock *
                           MappedWord8Array.array Socket.buf -> int * 'a Socket.sock_addr
               = Obj.magic recvArrFrom_
        val recvArrFrom' : ('a, Socket.dgram) Socket.sock *
                           MappedWord8Array.array Socket.buf * Socket.in_flags
                                   -> int * 'a Socket.sock_addr
               = Obj.magic recvArrFrom'_
    end
end
