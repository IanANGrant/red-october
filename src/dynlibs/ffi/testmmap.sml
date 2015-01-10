val _ = load "FifoBuffer";
val _ = load "ValRepr";

val _ = load "Shm";
val _ = load "MMap";
val _ = load "ShmMap";

structure WordStruct
   :> GenericWord
        where type largeword = Word.word
          and type word = Word.word =
struct
   type largeword = Word.word
   open Word
end

structure Fifo =
 ConcreteFifoBuffer
    (structure WordStruct = WordStruct : GenericWord
     structure ArrayStruct = MappedWord8Array : GenericArray
     structure ArraySliceStruct = MappedWord8ArraySlice : GenericArraySlice
     val length = Word8Vector.length
     val zero = 0w0);

val fd = Shm.shm_open ("/shmname",[Fcntl.O_CREAT,Fcntl.O_RDWR],[Fcntl.S_IRUSR,Fcntl.S_IWUSR]);
val _ = Socket.ftruncate fd (Word.toInt MMap.PAGE_SIZE);
val NULL = ValRepr.cptrFromWord 0w0
val buff = MMap.mmap
            (NULL,
             MMap.PAGE_SIZE,
            [MMap.PROT_READ,MMap.PROT_WRITE],
            [MMap.MAP_SHARED],
             fd,
             0w0);
val _ = MMap.munmap (buff,MMap.PAGE_SIZE);
val arr = MMap.mmapMappedArray
            (NULL,
             MMap.PAGE_SIZE,
            [MMap.PROT_READ,MMap.PROT_WRITE],
            [MMap.MAP_SHARED,MMap.MAP_ANONYMOUS],
             fd,
             0w0);
val buff2 = Fifo.fromSlice (MappedWord8ArraySlice.full arr);

val _ = Shm.shm_unlink "/shmname";

fun mapShm (shmname,npages) =
   ShmMap.shm_create (shmname,
       [Fcntl.O_RDWR,Fcntl.O_CREAT,Fcntl.O_TRUNC],
       [Fcntl.S_IRWXU],
       [MMap.MAP_SHARED],
       [MMap.PROT_WRITE,MMap.PROT_READ,MMap.PROT_EXEC], npages)

val buff = mapShm ("/shmname",2)

