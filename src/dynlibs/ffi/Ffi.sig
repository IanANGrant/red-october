(* Ffi functions, lightning functions, static vectors, etc. *)
(* This module requires Dynlib and GNU lightning, and the GNU FFI
   library to be installed *)

type svector

datatype ffi_type_enum = 
    FFI_TYPE_VOID
  | FFI_TYPE_INT
  | FFI_TYPE_FLOAT
  | FFI_TYPE_DOUBLE
  | FFI_TYPE_LONGDOUBLE
  | FFI_TYPE_UINT8
  | FFI_TYPE_SINT8
  | FFI_TYPE_UINT16
  | FFI_TYPE_SINT16
  | FFI_TYPE_UINT32
  | FFI_TYPE_SINT32
  | FFI_TYPE_UINT64
  | FFI_TYPE_SINT64
  | FFI_TYPE_STRUCT
  | FFI_TYPE_POINTER

datatype ffi_type_struct_pointers =
   ffi_type_void
 | ffi_type_uchar
 | ffi_type_schar
 | ffi_type_uint
 | ffi_type_sint
 | ffi_type_ushort
 | ffi_type_sshort
 | ffi_type_ulong
 | ffi_type_slong
 | ffi_type_float
 | ffi_type_double
 | ffi_type_longdouble
 | ffi_type_pointer
 | ffi_type_uint8
 | ffi_type_sint8
 | ffi_type_uint16
 | ffi_type_sint16
 | ffi_type_uint32
 | ffi_type_sint32
 | ffi_type_uint64
 | ffi_type_sint64

datatype ffi_status_enum =
    FFI_OK
 |  FFI_BAD_TYPEDEF
 |  FFI_BAD_ABI

datatype ffi_int_size = 
    Int
 |  Short
 |  Long

datatype ffi_signed =
   Signed
 | Unsigned

datatype ffi_float_size =
   Single
 | Double
 | LongDouble

datatype ffi_array_dimension =
   Incomplete
 | Dim of int

datatype ffi_type =
   Pointer of ffi_type
 | Array of ffi_array_dimension * ffi_type
 | Const of ffi_type
 | Function of ffi_type * ffi_type_enum option
 | Integer of ffi_signed * ffi_int_size
 | Structure of ffi_type list
 | Float of ffi_float_size
 | Character of ffi_signed
 | Void
 | VariadicFunction of ffi_type * ffi_type list * ffi_type_enum option

val heap_start : unit -> Dynlib.cptr
val heap_end : unit -> Dynlib.cptr
val gc_phase : unit -> Int.int

val gc_minor : unit-> unit
val gc_major : unit-> unit
val gc_full_major : unit-> unit

val first_atoms_ : Dynlib.cptr
val raiseprimitive0 : Dynlib.cptr

val ffi_alloc : Dynlib.cptr
val ffi_realloc : Dynlib.cptr
val ffi_resize : Dynlib.cptr
val ffi_free : Dynlib.cptr

val ffi_stat_alloc : Dynlib.cptr
val ffi_stat_resize : Dynlib.cptr
val ffi_stat_free : Dynlib.cptr

val sys_malloc : Dynlib.cptr
val sys_realloc : Dynlib.cptr
val sys_free : Dynlib.cptr

val my_alloc : Dynlib.cptr
val my_realloc : Dynlib.cptr
val my_free : Dynlib.cptr

val jit_set_memfuns : unit -> unit

val jit_constants : (string * word) list

val ffi_report_alloc: string -> unit

val svec_make : Int.int -> svector

val svec_wrap_cptr : Dynlib.cptr -> Dynlib.cptr -> Int.int -> svector

val svec_clear : svector -> unit

val svec_getlength : svector -> Int.int

val svec_from_vector : Word8Vector.vector -> svector

val svec_from_word_vector : word vector -> svector

val wordVectorFromVector : Word8Vector.vector -> word vector

val vectorFromWordVector : word vector -> Word8Vector.vector

val svec_getbuffercptr   :  svector -> Dynlib.cptr

val svec_getbufferarray : svector -> Word8Array.array

val svec_getpointervalue : svector -> Word8Vector.vector

val svec_getcptrvalue : Dynlib.cptr -> Word8Vector.vector

val svec_getcptr : Word8Vector.vector -> Dynlib.cptr

val svec_getcptrword : Dynlib.cptr -> Word.word

val svec_setcptrvalue : Word8Vector.vector -> Dynlib.cptr

val svec_setcptrword : Word.word -> Dynlib.cptr

val svec_getvecword : Word8Vector.vector -> Word.word

val svec_setvecword : Word.word -> Word8Vector.vector

val svec_getvecint : Word8Vector.vector -> Int.int

val svec_setvecint : Int.int -> Word8Vector.vector

val svec_getvalue   : svector -> Int.int -> Int.int -> Word8Vector.vector

val svec_setvalue   : svector -> Int.int -> Word8Vector.vector -> Int.int

val svec_getvecstring : Word8Vector.vector -> string

val jit_get_arch : unit -> string

val jit_get_byteorder : unit -> Int.int

val jit_get_little_endian : unit -> Int.int

val jit_get_big_endian : unit -> Int.int

val jit_get_wordsize : unit -> Int.int

val jit_get_codetablelength : unit -> Int.int

val jit_get_sizeslength : unit -> Int.int

val jit_get_constantslength : unit -> Int.int

val jit_get_codetableentry : Int.int -> Int.int * string

val jit_get_sizesentry : Int.int -> Int.int * string

val jit_get_constantsentry : Int.int -> Word.word * string

val jit_initialise_constants : bool -> unit

val BYTE_ORDER : Int.int

val LITTLE_ENDIAN : Int.int

val BIG_ENDIAN : Int.int

val jit_get_constant : string -> Word.word

val jit_get_debug : unit -> bool

val jit_set_debug : bool -> unit

val ffi_default_abi : unit -> Int.int

val ffi_cif_length : unit -> Int.int

val ffi_type_length : unit -> Int.int

val ffi_closure_length : unit -> Int.int

val ffi_status_values_length : unit -> Int.int

val ffi_type_values_length : unit -> Int.int

val ffi_type_sizes_length : unit -> Int.int

val ffi_type_struct_length : unit -> Int.int

val ffi_type_struct_pointers_length : unit -> Int.int

val ffi_status_values_entry : Int.int -> Int.int * string

val ffi_type_values_entry : Int.int -> Int.int * string

val ffi_type_sizes_entry : Int.int -> Int.int * Int.int * string

val ffi_type_struct_entry : Int.int -> Int.int * Int.int * string

val ffi_type_struct_pointers_entry : Int.int -> Word.word * String.string

val typeEnumToInt : ffi_type_enum -> Int.int

val typeEnumFromInt : Int.int -> ffi_type_enum option

val typeToStructPointer : ffi_type_struct_pointers -> Word.word

val statusEnumToInt : ffi_status_enum -> Int.int
val statusEnumFromInt : Int.int -> ffi_status_enum
val statusIntToString : Int.int -> string
val typeEnumToSize : ffi_type_enum -> Int.int
val typeEnumToAlign : ffi_type_enum -> Int.int

val typepFromTypeEnum : ffi_type_enum -> word

val jit_code : string -> Int.int

val NULL : Dynlib.cptr
val NULLvec : Word8Vector.vector

val ffi_getdryrun : unit -> bool

val ffi_setdryrun : bool -> unit

val ffi_dumpbytes : Dynlib.cptr -> Int.int -> unit

val ffi_dumpwords : Dynlib.cptr -> Int.int -> unit

val ffi_dumpvecbytes: Word8Vector.vector -> Int.int -> unit

val ffi_dumpvecwords: Word8Vector.vector -> Int.int -> unit

val svec_cptr_byte_offset : Dynlib.cptr -> int -> Dynlib.cptr
val svec_byte_offset : svector -> Int.int -> Dynlib.cptr
val svec_dumpb : svector -> Int.int -> Int.int -> unit
val svec_dumpw : svector -> Int.int -> Int.int -> unit
val svec_dumpb_all : svector -> unit
val svec_dumpw_all : svector -> unit

val svec_debugb : string -> svector -> unit
val svec_debugw : string -> svector -> unit
val vec_debugb : string -> Word8Vector.vector -> unit
val vec_debugw : string -> Word8Vector.vector -> unit

val ffi_prep_cif_ : Dynlib.cptr -> Int.int -> Int.int -> Dynlib.cptr -> Dynlib.cptr -> Int.int

val ffi_call_ : Dynlib.cptr -> Dynlib.cptr -> Dynlib.cptr -> Dynlib.cptr -> unit

val ffi_call_n_ : Dynlib.cptr -> Dynlib.cptr -> Dynlib.cptr -> Dynlib.cptr -> unit

val realToDoubleVector : real -> Word8Vector.vector
val doubleVectorToReal : Word8Vector.vector -> real

val realToFloatVector : real -> Word8Vector.vector
val floatVectorToReal : Word8Vector.vector -> real

val vectorToList : Word8Vector.vector -> Word8.word list
val listToVector : Word8.word list -> Word8Vector.vector

val vectorFromWordList : Word.word list -> Word8Vector.vector
val wordListFromVector : Word8Vector.vector -> Word.word list

val vectorFromString : string -> Word8Vector.vector
val stringFromVector : Word8Vector.vector -> string

val svec_from_string : string -> svector
val svec_to_string : svector -> string

val ffi_default_abi_number : Int.int
val ffi_cif_struct_size : Int.int
val ffi_type_struct_size : Int.int
val ffi_closure_size : Int.int

val ffi_trampoline : string -> Dynlib.cptr -> ffi_type ->
                     ('a -> Word8Vector.vector) -> (Word8Vector.vector -> 'b) -> 'a -> 'b

val trampolines : (string * ffi_type) list ref

val mkargssvec : Word8Vector.vector list -> Word8Vector.vector * svector

val var  : Dynlib.cptr -> 'b                            
val app1 : Dynlib.cptr -> 'a1 -> 'b                     
val app2 : Dynlib.cptr -> 'a1 -> 'a2 -> 'b              
val app3 : Dynlib.cptr -> 'a1 -> 'a2 -> 'a3 -> 'b       
val app4 : Dynlib.cptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'b
val app5 : Dynlib.cptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b
