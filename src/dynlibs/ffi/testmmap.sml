val _ = load "Dynlib";
val _ = load "Socket";

val _ = load "MappedWord8Array";
val _ = load "MappedSocket";
val _ = load "FifoBuffer";
val _ = load "ValRepr";

val _ = load "Shm";
val _ = load "MMap";
val _ = load "ShmMap";
val _ = load "DoubleMappedWord8Array";
val _ = load "Word8Array";

val NULL = ValRepr.cptrFromWord 0w0

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

val _ = Socket.fdclose fd;

val buff2 = Fifo.fromSlice (MappedWord8ArraySlice.full arr);

val _ = Shm.shm_unlink "/shmname";

fun mapShm (shmname,npages) =
   ShmMap.shm_create (shmname,
       [Fcntl.O_RDWR,Fcntl.O_CREAT,Fcntl.O_TRUNC],
       [Fcntl.S_IRWXU],
       [MMap.MAP_SHARED],
       [MMap.PROT_WRITE,MMap.PROT_READ,MMap.PROT_EXEC], npages)
   
val arr1 = DoubleMappedWord8Array.array (1,0w0);

val arr1s = MappedWord8Array.length arr1;

val _ = MappedWord8Array.update(arr1,arr1s div 2,0w0);
val _ = MappedWord8Array.update(arr1,0,0wx2a);
val r = MappedWord8Array.sub(arr1,arr1s div 2);

val _ = MappedWord8Array.update(arr1,arr1s div 2 - 1,0w0);
val _ = MappedWord8Array.update(arr1,arr1s - 1,0wx2a);
val r = MappedWord8Array.sub(arr1,arr1s div 2 - 1);

val fdrand = Socket.fdopen ("/dev/urandom",[Socket.O_RDONLY],Socket.S_IRUSR);

val nr = MappedSocket.readArr (fdrand,{buf=arr1,ofs=0,size=SOME 32});

val rnds = Vector.tabulate(nr,fn i => MappedWord8Array.sub (arr1,i));

val rnds' = Vector.tabulate(nr,fn i => MappedWord8Array.sub (arr1,i+(arr1s div 2)));

val true = rnds=rnds';

val _ = Socket.fdclose fdrand;
