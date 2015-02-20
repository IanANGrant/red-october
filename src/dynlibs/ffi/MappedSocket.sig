(* Socket -- SML Basis Library -- requires Dynlib *)

val readArr : Socket.unixfd * MappedWord8Array.array Socket.buf -> int
val writeArr : Socket.unixfd * MappedWord8Array.array Socket.buf -> int

(* Socket output operations *)
val sendArr    : ('a, Socket.active Socket.stream) Socket.sock * MappedWord8Array.array Socket.buf -> int
val sendArr'   : ('a, Socket.active Socket.stream) Socket.sock * MappedWord8Array.array Socket.buf 
                 * Socket.out_flags -> int
val sendArrTo  : ('a, Socket.dgram) Socket.sock * 'a Socket.sock_addr * MappedWord8Array.array Socket.buf -> int
val sendArrTo' : ('a, Socket.dgram) Socket.sock * 'a Socket.sock_addr * MappedWord8Array.array Socket.buf
                 * Socket.out_flags -> int

(* Socket input operations *)
val recvArr      : ('a, Socket.active Socket.stream) Socket.sock * MappedWord8Array.array Socket.buf -> int
val recvArr'     : ('a, Socket.active Socket.stream) Socket.sock * MappedWord8Array.array Socket.buf
                   * Socket.in_flags -> int
val recvArrFrom  : ('a, Socket.dgram) Socket.sock * MappedWord8Array.array Socket.buf 
                   -> int * 'a Socket.sock_addr
val recvArrFrom' : ('a, Socket.dgram) Socket.sock * MappedWord8Array.array Socket.buf * Socket.in_flags
                   -> int * 'a Socket.sock_addr

(* 
   Structure Socket defines functions for creating and using sockets,
   a means for communication between SML processes on the same machine
   or via a network.

   [('addressfam, 'socktype) sock] is the type of sockets with address
   family 'addressfam and having type 'socktype.

   ['addressfam sock_addr] is the type of sockets addresses.

   The possible address (protocol) families are 

        type pf_file    The Unix address family (file)
        type pf_inet    The Internet address family

   The possible socket types are 
        type dgram      datagram sockets
        type 'a stream  stream sockets
        type passive    passive stream sockets
        type active     active, or connected, stream sockets

   [fileAddr fname] returns a socket address for the Unix protocol
   family, created from the given file name fname.

   [inetAddr inetaddr portno] returns a socket address for the
   Internet protocol family, created from the given Internet number
   (e.g. "130.225.40.253") and port number (e.g. 8080).

   [fileStream ()] returns a new stream socket for the Unix protocol
   family.

   [fileDgram ()] returns a new datagram socket for the Unix protocol
   family.

   [inetStream ()] returns a new stream socket for the Internet
   protocol family.

   [inetDgram ()] returns a new datagram socket for the Internet
   protocol family.

   [accept sock] extracts the first connection on the queue of pending
   connections to sock.  Returns (sock', addr) where sock' is a copy
   of the socket sock, bound to that connection, and addr is the
   address of the communications counterpart (the other end of the
   connection).  Blocks if no connections are pending.  The stream
   socket sock must have been assigned a name (with bind) and must be
   listening for connections (following a call to listen).

   [bind sock addr] binds the socket sock to the address addr, that
   is, assigns the name addr to the socket.  Binding a name in the
   Unix protocol family creates a socket in the file system that must
   be deleted when it is no longer needed

   [connect (sock, addr)] attempts to connect socket sock to the
   communications peer at address addr.  If sock is a datagram socket,
   then addr is the address to which datagrams is to be sent, and the
   only address from which datagrams will be accepted.  If sock is a
   stream socket, then addr specifies another socket to which to
   connect.

   [listen (sock, queuelen)] enables the passive stream socket sock to
   accept incoming connections.  The parameter queuelen specifies the
   maximal number of pending connections.  Further connections from
   clients may be refused when this limit is reached.

   [close sock] closes the socket.

   [shutdown sock shutdown_mode] shuts down socket sock for further
   communication, as specified by the shutdown_mode parameter:

   [NO_RECVS]           no further receives are allowed;

   [NO_SENDS]           no further sends are allowed;

   [NO_RECVS_OR_SENDS]  no further receives or sends are allowed.

   [getinetaddr addr] returns the Internet number
   (e.g. "130.225.40.253") of the Internet socket address addr.

   ['a buf] is the type of records { buf, ofs, size } which represent
   subvectors or subarrays:
   if size = SOME s it represents buf[ofs..ofs+s-1];
   if size = NONE   it represents buf[ofs..len-1] where len is buf's length.
   When the subbuffer is used in a call, exception Subscript will be raised 
   if ofs < 0 or size < 0 or ofs+size > len.   

   [sendVec (sock, vecbuf)] transmits the bytes from buffer vecbuf on
   the active stream socket sock.  Returns the number of bytes sent.
   Blocks until sufficient space is available at the socket.

   [sendArr (sock, arrbuf)] is analogous til sendVec.

   [sendVec' (sock, vecbuf, out_flags)] transmits the bytes from
   buffer vecbuf on the active stream socket sock, observing the
   out_flags.  Returns the number of bytes sent.  Blocks until
   sufficient space is available at the socket.

   [out_flags] is the type of records { don't_route, oob } in which
   the field don't_route specifies whether routing should be bypassed,
   and the field oob specifies whether data should be sent out-of-band.

   [sendArr' (sock, arrbuf, out_flags)] is analogous til sendVec'.

   [sendVecTo (sock, addr, vecbuf)] transmits the bytes from buffer
   vecbuf on the datagram socket sock to the target address addr.
   Returns the number of bytes sent.  Blocks until sufficient space is
   available at the socket.

   [sendArrTo (sock, addr, arrbuf)] is analogous til sendVecTo.

   [sendVecTo' (sock, addr, vecbuf, out_flags)] transmits the bytes
   from buffer vecbuf on the datagram socket sock to the target
   address addr, observing the out_flags.  Returns the number of bytes
   sent.  Blocks until sufficient space is available at the socket.
   See above for a description of vecbuf and out_flags.

   [sendArrTo' (sock, addr, arrbuf, out_flags)] is analogous til sendVecTo'.

   [recvVec (sock, n)] receives up to n bytes from the active stream
   socket sock.  Returns a byte vector containing the bytes actually
   received.  Blocks until some data become available at the socket,
   then returns any available data, up to n bytes.  Excess data are
   not lost; they are available for subsequent receive calls.

   [recvArr (sock, arrbuf)] receives bytes from the active stream
   socket sock into the subarray arrbuf, up to the available space.
   If #size(arrbuf) = SOME(s) the available space is s bytes; if
   #size(arrbuf) = NONE the available space is len - #ofs(arrbuf)
   bytes.  Returns the number of bytes actually received.  Blocks
   until some data become available at the socket.  Excess data are
   not lost; they are available for subsequent receive calls.

   [recvVec' (sock, n, in_flags)] receives up to n bytes from the
   active stream socket sock, observing the in_flags.  Returns a byte
   vector containing the bytes actually received.  Blocks until some
   data become available at the socket, then returns any available
   data, up to n bytes.  Data in excess of n bytes are not lost; they
   are available for subsequent receive calls.

   [in_flags] is the type of records { peek, oob } in which the field
   peek specifies that the data read should not be removed from the
   receive queue, and the field oob specifies that data may be
   received out-of-band.

   [recvArr' (sock, arrbuf, in_flags)] receives bytes from the active
   stream socket sock into the subarray arrbuf, observing the
   in_flags, up to the available space..  Returns the number of bytes
   actually received.  Blocks until some data become available at the
   socket.  Excess data are not lost; they are available for
   subsequent receive calls.

   [recvVecFrom (sock, n)] receives up to n bytes from the datagram
   socket sock.  Returns a byte vector containing the bytes actually
   received.  Blocks until some data become available at the socket,
   then returns any available data, up to n bytes.

   [recvArrFrom (sock, arrbuf)] receives bytes from the datagram
   socket sock into the subarray arrbuf.  Returns the number of bytes
   actually received.  Blocks until some data become available at the
   socket.

   [recvVecFrom' (sock, n, in_flags)] receives up to n bytes from the
   datagram socket sock, observing the in_flags (see above).  Returns
   (vec, addr) where vec is a byte vector containing the bytes
   actually received, and addr is the source address of the message.
   Blocks until some data become available at the socket, then returns
   any available data, up to n bytes.

   [recvArrFrom' (sock, arrbuf, in_flags)] receives bytes from the
   datagram socket sock into the array buffer arrbuf, observing the
   in_flags (see above).  Returns (n, addr) where n is the number of
   bytes actually received, and addr is the source address of the
   message.  Blocks until some data become available at the socket.

   [sockDesc sock] returns a descriptor for the socket sock, to be
   used in a call to select.

   [compare (sd1, sd2)] compares sd1 and sd2 according to an
   unspecified total ordering, and returns LESS if sd1 precedes sd2,
   returns GREATER is sd1 precedes sd2, and returns EQUAL otherwise.
   
   [sameDesc (sd1, sd2)] returns true if sd1 and sd2 describe the same
   socket.  Equivalent to compare(sd1, sd2) = EQUAL.  

   [select { rds, wrs, exs, timeout }] blocks the calling process
   until some input/output operations become possible on some sockets.
   The call will check the sockets described in rds for reading, those
   in wrs for writing, and those in exs for exceptional conditions.
   Returns { rds, wrs, exs } where rds now is a list of descriptors of
   sockets ready for reading, wrs are ready for writing, and exs have
   exceptional conditions.  The order of the socket descriptors in the
   results is the same as their order in the corresponding arguments.
   If timeout is NONE then the call blocks until some input/output
   operations become possible; if timeout is SOME(t) then the call
   blocks for at most time t.

   A server socket is considered ready for reading if there is a
   pending connection which can be accepted with `accept'.  A client
   socket is ready for writing when its connection is fully
   established.
*)
