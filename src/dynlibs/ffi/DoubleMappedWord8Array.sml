local
   val dlxh = Dynlib.dlopen {lib = "",
                    flag = Dynlib.RTLD_LAZY,
                    global = false }
   fun ssym s = Dynlib.dlsym dlxh s
   fun ssymp s = Dynlib.cptr (ssym s)
   val NULL = ValRepr.cptrFromWord 0w0
in
   fun array_ fnname name address npages oflags fmode prot flags =
      let val wnpages = Word.fromInt npages
          val fd1 = Shm.shm_open (name,oflags,fmode)
          val mb1 = MMap.mmap (address, MMap.PAGE_SIZE * wnpages, prot, flags, fd1, 0w0)
          val mb1addr = ValRepr.wordFromCptr mb1
          val _ = Socket.fdclose fd1
          val fd2 = Shm.shm_open (name,oflags,fmode)
          val addr2 = ValRepr.cptrFromWord (mb1addr + MMap.PAGE_SIZE * wnpages)
          val mb2 = MMap.mmap (addr2, MMap.PAGE_SIZE * wnpages, prot, MMap.MAP_FIXED::flags, fd2, 0w0)
          val mb2addr = ValRepr.wordFromCptr mb2
          val _ = Socket.fdclose fd2
          val array = if mb2addr = mb1addr + MMap.PAGE_SIZE * wnpages
                         then MappedWord8Array.alloc_final
                               ((ssymp "munmap"),2,mb1,Word.toInt MMap.PAGE_SIZE * 2 * npages)
                         else raise Fail ("DoubleMappedWord8Array."^fnname^": mapped regions not contiguous")
      in array
      end
   fun array (npages,v) =
      let val (name,fd) = Shm.mk_tmpshm ("dmarray",[Fcntl.O_RDWR],[Fcntl.S_IRWXU])
          val _ = Socket.ftruncate fd (npages * Word.toInt MMap.PAGE_SIZE)
          val prot = [MMap.PROT_READ,MMap.PROT_WRITE,MMap.PROT_EXEC]
          val flags = [MMap.MAP_SHARED]
          val buff = MMap.mmap (NULL, MMap.PAGE_SIZE * (Word.fromInt npages), prot, flags, fd, 0w0)
          val result = array_ "array" name NULL npages [Fcntl.O_RDWR] [Fcntl.S_IRWXU] prot flags
          val _ = Socket.fdclose fd
          val _ = Shm.shm_unlink name
      in MappedWord8Array.modify (fn _ => v) result;
         result
      end
   fun fromShm name =
       array_ "fromShm" name
end
