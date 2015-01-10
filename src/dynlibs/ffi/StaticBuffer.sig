type elem = Word8.word
type array = StaticWord8Array.array
type vector = Word8Vector.vector
type vector_slice = Word8VectorSlice.slice
type slice = Word8ArraySlice.slice

val gc_minor : unit-> unit
val gc_major : unit-> unit
val gc_full_major : unit-> unit

val get_debug : unit -> bool

val set_debug : bool -> unit

val slice : array * int * int option -> slice

val buffer : array -> Dynlib.cptr

val svec_wrap_cptr2     : Dynlib.cptr -> Dynlib.cptr -> int -> array

val length : array -> int

val dumpbytes : Dynlib.cptr -> int -> unit

val dumpwords : Dynlib.cptr -> int -> unit

val dumpvecbytes: vector -> int -> unit

val dumpvecwords: vector -> int -> unit

val svec_getcptrword : Dynlib.cptr -> Word.word
val svec_setcptrword : Word.word -> Dynlib.cptr

val svec_getcptrvalue : Dynlib.cptr -> vector
val svec_setcptrvalue : vector -> Dynlib.cptr

val svec_cptr_byte_offset : Dynlib.cptr -> int -> Dynlib.cptr
val svec_byte_offset : array -> int -> Dynlib.cptr

val dumpb : array -> int -> int -> unit
val dumpw : array -> int -> int -> unit

val dumpb_all : array -> unit
val dumpw_all : array -> unit

val vec_dumpb : vector -> int -> int -> unit
val vec_dumpw : vector -> int -> int -> unit

val vec_dumpb_all : vector -> unit
val vec_dumpw_all : vector -> unit
