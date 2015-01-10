type elem = Word8.word
type array = StaticWord8Array.array
type vector = Word8Vector.vector
type vector_slice = Word8VectorSlice.slice
type slice = Word8ArraySlice.slice

local 
    prim_val magic   : 'a -> 'b                        = 1 "identity";

    prim_type array_;

    fun from_array (a : array)  = !(magic a) : array_;
    fun to_array (a : array_)  = (magic (ref a)) : array;

    open Dynlib

    val dlh = Dynlib.dlopen
                {lib = "libmffi.so",
                 flag = Dynlib.RTLD_LAZY,
                 global = false}

    val dlxh = Dynlib.dlopen {lib = "",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }

    val sym = Dynlib.dlsym dlh

    val selfsym = Dynlib.dlsym dlxh

    val selfapp1 : string -> 'a -> 'b
         = fn s => Dynlib.app1 (selfsym s)

    val app1 : string -> 'a -> 'b
         = fn s => Dynlib.app1 (sym s)
    val app2 : string -> 'a -> 'b -> 'c
         = fn s => Dynlib.app2 (sym s)
    val app3 : string -> 'a -> 'b -> 'c -> 'd
         = fn s => Dynlib.app3 (sym s)
    val app4 : string -> 'a -> 'b -> 'c -> 'd -> 'e
         = fn s => Dynlib.app4 (sym s)
    val app5 : string -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
         = fn s => Dynlib.app5 (sym s)

    val buffer : array_ -> Dynlib.cptr
          = app1 "svec_getbuffer"

    val getlength     : array_ -> int         
          = app1 "svec_getlength"

    val read      : array_ -> int -> int -> vector
          = app3 "svec_getvalue"

    val write      : array_ -> int -> vector -> int
          = app3 "svec_setvalue"

    prim_val vector_  : int -> vector                 = 1 "create_string";
    prim_val subv_    : vector -> int -> elem         = 2 "get_nth_char";
    prim_val updatev_ : vector -> int -> elem -> unit = 3 "set_nth_char";

    prim_val sub__     : Dynlib.cptr -> int -> elem         = 2 "get_nth_char";

    prim_val update__  : Dynlib.cptr -> int -> elem -> unit = 3 "set_nth_char";

    prim_val length__  : Dynlib.cptr -> int                 = 1 "string_length";

    val sub_     : array_ -> int -> elem
           = fn a => sub__ (buffer a)

    val update_  : array_ -> int -> elem -> unit
           = fn a => update__ (buffer a)

    val length_  : array_ -> int
           = fn a => length__ (buffer a)

    val array_ : int -> array_
          = app1 "svec_make"

    val svec_wrap_cptr     : Dynlib.cptr -> Dynlib.cptr -> int -> array_
          = app3 "svec_wrap_cptr"

    val svec_wrap_cptr2    : Dynlib.cptr -> Dynlib.cptr -> int -> array_
          = app3 "svec_wrap_cptr2"

    val svec_getcptrword : Dynlib.cptr -> Word.word
        = app1 "svec_getcptrword"

    val svec_setcptrword : Word.word -> Dynlib.cptr
        = app1 "svec_setcptrword"

    fun vec_byte_offset vec offs =
        Word8VectorSlice.vector (Word8VectorSlice.slice (vec,offs,NONE));
in 
    val gc_minor : unit-> unit = selfapp1 "gc_minor"
    val gc_major : unit-> unit = selfapp1 "gc_major"
    val gc_full_major : unit-> unit = selfapp1 "gc_full_major"

    val get_debug : unit -> bool
        = app1 "jit_get_debug"

    val set_debug : bool -> unit
        = app1 "jit_set_debug"

    val svec_wrap_cptr2     : Dynlib.cptr -> Dynlib.cptr -> int -> array
        = fn arr => fn ffn => fn l => to_array (svec_wrap_cptr2 arr ffn l)

    val buffer : array -> Dynlib.cptr
         = buffer o from_array

    val length : array -> int
         = getlength o from_array

    val slice : array * int * int option -> slice
       = fn (a,offs,lenopt) =>
          let val alen = length a
              val len = 
                case lenopt
                  of SOME n => if n + offs > alen
                                  then raise Subscript
                                  else n
                   | NONE => alen - offs
              val arr = ref (buffer a)
          in if offs > alen 
                then raise Subscript
                else magic (arr : Dynlib.cptr ref, offs : int, len : int)
          end

    val dumpbytes : Dynlib.cptr -> int -> unit
        = app2 "ffi_dumpbytes"

    val dumpwords : Dynlib.cptr -> int -> unit
       = app2 "ffi_dumpwords"

    val dumpvecbytes: vector -> int -> unit
       = app2 "ffi_dumpbytes"

    val dumpvecwords: vector -> int -> unit
       = app2 "ffi_dumpwords"

    val svec_getcptrword : Dynlib.cptr -> Word.word
        = svec_getcptrword

    val svec_setcptrword : Word.word -> Dynlib.cptr
        = svec_setcptrword

    val svec_getcptrvalue : Dynlib.cptr -> vector
        = app1 "svec_getcptrvalue"

    val svec_setcptrvalue : vector -> Dynlib.cptr
        = app1 "svec_setcptrvalue"

    fun svec_cptr_byte_offset cptr offs =
        svec_setcptrword (Word.+(svec_getcptrword cptr,Word.fromInt offs))

    fun svec_byte_offset a = svec_cptr_byte_offset (buffer a)

fun dumpb a offs length = 
   let val avail = (getlength (from_array a)) - offs
       val len = if length > avail then avail else length
   in if len <= 0 
      then () 
      else dumpbytes (svec_byte_offset a offs) len
   end

fun dumpw a offs length = 
   let val avail = ((getlength (from_array a)) - offs) div 4;
       val len = if length > avail then avail else length
   in if len <= 0 
      then () 
      else dumpwords (svec_byte_offset a offs) len
   end

fun dumpb_all a = dumpb a 0 (getlength (from_array a))
fun dumpw_all a = dumpw a 0 (getlength (from_array a) div 4)

fun vec_dumpb v offs length = 
   let val avail = (Word8Vector.length v) - offs
       val len = if length > avail then avail else length
   in if len <= 0 
      then () 
      else dumpvecbytes (vec_byte_offset v offs) len
   end

fun vec_dumpw v offs length = 
   let val avail = ((Word8Vector.length v) - offs) div 4;
       val len = if length > avail then avail else length
   in if len <= 0 
      then () 
      else dumpvecwords (vec_byte_offset v offs) len
   end

fun vec_dumpb_all v = vec_dumpb v 0 (Word8Vector.length v)
fun vec_dumpw_all v = vec_dumpw v 0 (Word8Vector.length v div 4)

end
