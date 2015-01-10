signature FDSet =
sig
   type fdset
   val size : int
   val fdsetsize : int
   val maxfd : int
   val fdset : unit -> fdset
   val writeFdset : MappedStruct.fifo * fdset -> unit
   val readFdset : MappedStruct.fifo -> fdset
   val fd_clr : Socket.unixfd * fdset -> unit
   val fd_set : Socket.unixfd * fdset -> unit
   val fd_isset : Socket.unixfd * fdset -> bool
   val fd_zero : fdset -> unit
   val toInt : Socket.unixfd -> int
   val fromInt : int -> Socket.unixfd
   val fromList : Socket.unixfd list -> fdset
   val toList : fdset -> Socket.unixfd list
  val lub : Socket.unixfd list -> int
end

structure FDSet :> FDSet 
  where type fdset = Word8Vector.vector =
struct
   type fdset = Word8Vector.vector
   type unixfd_ = int ref;

   local
      val dlih = Dynlib.dlopen {lib = "libcinfo.so",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }
      fun syminf s = Dynlib.dlsym dlih s;

      val fdset_size = 
           Dynlib.app1
              (syminf "fdset_size")
                 : unit -> int

      val fdset_fdsetsize = 
           Dynlib.app1
              (syminf "fdset_fdsetsize")
                 : unit -> int

      val fdset_openmax = 
           Dynlib.app1
              (syminf "fdset_openmax")
                 : unit -> int

	val fdset_fddesc_ : int -> unixfd_
           = Dynlib.app1
              (syminf "fdset_fddesc")

        val fdset_descfd_ : unixfd_ -> int
           = Dynlib.app1
              (syminf "fdset_descfd")

      val fdset_zero
           = Dynlib.app1
              (syminf "fdset_zero")
                 : Dynlib.cptr -> unit

      val fdset_set 
           = Dynlib.app2
              (syminf "fdset_set")
                 : int -> Dynlib.cptr -> unit

      val fdset_clr 
           = Dynlib.app2
              (syminf "fdset_clr")
                 : int -> Dynlib.cptr -> unit

      val fdset_isset 
           = Dynlib.app2
              (syminf "fdset_isset")
                 : int -> Dynlib.cptr -> bool
      
    prim_val vector_ : int -> Word8Vector.vector     = 1 "create_string"
    prim_val magic   : 'a -> 'b                      = 1 "identity"

    val to_cptr : Word8Vector.vector -> Dynlib.cptr = magic
    val fromunixfd : Socket.unixfd -> unixfd_ = magic
    val tounixfd : unixfd_ -> Socket.unixfd = magic
   in
      val size = fdset_size ()
      val fdsetsize = fdset_fdsetsize ()
      val maxfd = fdset_openmax ()
      val fdset =
         fn () =>
            let val v = vector_ size
                val _ = fdset_zero (to_cptr v)
            in v
            end
      val fd_clr_ =
        fn ((ref fd) : unixfd_,fdset : fdset) =>
            fdset_clr fd (to_cptr fdset)
      val fd_set_ =
        fn ((ref fd) : unixfd_,fdset : fdset) =>
            fdset_set fd (to_cptr fdset)
      val fd_isset_ =
        fn ((ref fd) : unixfd_,fdset : fdset) =>
            fdset_isset fd (to_cptr fdset)
      fun fd_set (fd,fdset) = fd_set_(fromunixfd fd,fdset)
      fun fd_clr (fd,fdset) = fd_clr_(fromunixfd fd,fdset)
      fun fd_isset (fd,fdset) = fd_isset_(fromunixfd fd,fdset)
      val fd_zero = fn (fdset : fdset) => fdset_zero (to_cptr fdset)
      val writeFdset = MappedStruct.writeVec 
      val readFdset = fn b => MappedStruct.readVec (b,size)
      val toInt =  fdset_descfd_ o fromunixfd
      val fromInt = tounixfd o fdset_fddesc_

fun toList fds =
   let fun iter acc 0 = acc
         | iter acc n =
           let val n' = n-1
               val fd = fromInt n'
           in iter (if fd_isset(fd,fds) then fd::acc else acc) n'
           end
   in iter [] maxfd 
   end

fun fromList l =
   let val fds = fdset()
       fun iter [] = ()
         | iter (fd::rest) =
              (fd_set (fd,fds);
               iter rest)
   in iter l;
      fds
   end

fun lub l =
   1 + 
    (List.foldr 
     (fn (fd,m) =>
         let val i = toInt fd
         in if i > m then i else m
         end) ~1 l)
   end
end
