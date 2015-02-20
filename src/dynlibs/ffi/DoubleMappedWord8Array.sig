val array : int * Word8.word -> MappedWord8Array.array
val fromShm : string -> Dynlib.cptr -> int -> Fcntl.flags list -> Fcntl.mode list ->
                     MMap.prot list -> MMap.flags list ->  MappedWord8Array.array
