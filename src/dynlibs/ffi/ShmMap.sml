structure ShmMap =
struct
    local
       structure MMap = MMap
       val NULL = ValRepr.cptrFromWord 0w0
       val SysErrString = fn (s,e) => (s^": "^(SysErr.toString e))
    in fun shm_open (shmname,shmflags,shmmode,mflags,mprot,size,address) =
           let val cptr = ValRepr.cptrFromWord address
           in DoubleMappedWord8Array.fromShm shmname cptr size shmflags shmmode mprot mflags
           end
       fun shm_create (shmname,shmflags,shmmode,mflags,mprot,size,address) =
           let val fd = Shm.shm_open (shmname,Fcntl.O_CREAT::Fcntl.O_TRUNC::shmflags,shmmode) 
                          handle SysErr.SysErr p => raise Fail ("shm_create: shm_open: "^(SysErrString p))
               val rv = Socket.ftruncate fd ((Word.toInt MMap.PAGE_SIZE) * size)
               val _ = if rv = ~1 then raise Fail "shm_create: ftruncate failed" else ()
               val arrsiz = MMap.PAGE_SIZE * (Word.fromInt size)
               val array = MMap.mmapMappedArray (NULL,arrsiz,mprot,mflags,fd,0w0)
                          handle SysErr.SysErr p => 
                                    raise Fail ("shm_create: mmapMAppedArray"^(SysErrString p))
               val _ = Socket.fdclose fd
           in let val saveme = array
              in shm_open (shmname,shmflags,shmmode,mflags,mprot,size,address) 
              end
           end
    end
end
