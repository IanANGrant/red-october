(* Ffi -- interface to the GNU FFI library *)
(*
load "Word8ArraySlice";
load "Dynlib";
load "Mosml";
load "Int";
*)

prim_type svector;
type svec = svector;

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

datatype ffi_int_size = 
    Int
 |  Short
 |  Long;

datatype ffi_signed =
   Signed
 | Unsigned;

datatype ffi_float_size =
   Single
 | Double
 | LongDouble;

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
  (* I would rather not have variadic functions in the interface at
     all, because they are really just C syntactic sugar. There must
     always be an underlying function taking a variable length list of
     values, mustn't there? *)
 | VariadicFunction of ffi_type * ffi_type list * ffi_type_enum option

local 
    open Word8Vector
    open Dynlib
    val libpath = Path.concat(FileSys.getDir (), "libmffi.so")
    val dlh = Dynlib.dlopen { lib = libpath, flag = Dynlib.RTLD_LAZY, global = false }

    val dlxh = Dynlib.dlopen {lib = "",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }
in

val first_atoms_ = Dynlib.cptr (Dynlib.dlsym dlxh "first_atoms")
val raiseprimitive0 = Dynlib.cptr (Dynlib.dlsym dlxh "raiseprimitive0")

val jit_set_memfuns : unit -> unit = app1 (dlsym dlh "jit_set_memfuns")

val ffi_report_alloc: string -> unit
   = app1 (dlsym dlh "ffi_report_alloc")

val ffi_alloc = Dynlib.cptr (Dynlib.dlsym dlh "mosml_ffi_alloc")
val ffi_realloc = Dynlib.cptr (Dynlib.dlsym dlh "mosml_ffi_realloc")
val ffi_resize = Dynlib.cptr (Dynlib.dlsym dlh "mosml_ffi_resize")
val ffi_free = Dynlib.cptr (Dynlib.dlsym dlh "mosml_ffi_free")

val ffi_stat_alloc = Dynlib.cptr (Dynlib.dlsym dlxh "stat_alloc")
val ffi_stat_resize = Dynlib.cptr (Dynlib.dlsym dlxh "stat_resize")
val ffi_stat_free = Dynlib.cptr (Dynlib.dlsym dlxh "stat_free")

val sys_malloc = Dynlib.cptr (Dynlib.dlsym dlxh "malloc")
val sys_realloc = Dynlib.cptr (Dynlib.dlsym dlxh "realloc")
val sys_free = Dynlib.cptr (Dynlib.dlsym dlxh "free")

val my_alloc = Dynlib.cptr (Dynlib.dlsym dlh "my_alloc")
val my_realloc = Dynlib.cptr (Dynlib.dlsym dlh "my_realloc")
val my_free = Dynlib.cptr (Dynlib.dlsym dlh "my_free")

val svec_make    : Int.int -> svec        
    = app1 (dlsym dlh "svec_make")

val svec_wrap_cptr : Dynlib.cptr -> Dynlib.cptr -> Int.int -> svec
    = app3 (dlsym dlh "svec_wrap_cptr")

val svec_clear   : svec -> unit
    = app1 (dlsym dlh "svec_clear")

val svec_getlength    : svec -> Int.int         
    = app1 (dlsym dlh "svec_getlength")

val svec_getbuffercptr : svec -> Dynlib.cptr
    = app1 (dlsym dlh "svec_getbuffer")

local
   val buffervec : svec -> vector
       = app1 (dlsym dlh "svec_getbuffer")
   prim_val array_ : Word8Vector.vector ref -> Word8Array.array = 1 "identity"
in
   val svec_getbufferarray : svec -> Word8Array.array
       = fn sv => array_ (ref (buffervec sv))
end

val svec_getpointervalue : svec -> vector
    = app1 (dlsym dlh "svec_getpointervalue")

val svec_getcptrvalue : Dynlib.cptr -> vector
    = app1 (dlsym dlh "svec_getcptrvalue")

val svec_getcptrword : Dynlib.cptr -> Word.word
    = app1 (dlsym dlh "svec_getcptrword")

val svec_setcptrvalue : vector -> Dynlib.cptr
    = app1 (dlsym dlh "svec_setcptrvalue")

val svec_getcptr : vector -> Dynlib.cptr
    = app1 (dlsym dlh "svec_getcptr")

val svec_setcptrword : Word.word -> Dynlib.cptr
    = app1 (dlsym dlh "svec_setcptrword")

val svec_setvecword : Word.word -> vector
    = app1 (dlsym dlh "svec_setvecword")

val svec_setvecint : Int.int -> vector
    = app1 (dlsym dlh "svec_setvecword")

val svec_getvecint : vector -> Int.int
    = app1 (dlsym dlh "svec_getvecword")

val svec_getvecword : vector -> Word.word
    = app1 (dlsym dlh "svec_getvecword")

val svec_getvalue   : svec -> Int.int -> Int.int -> vector
    = app3 (dlsym dlh "svec_getvalue")

val svec_setvalue   : svec -> Int.int -> vector -> Int.int
    = app3 (dlsym dlh "svec_setvalue")

val ffi_default_abi : unit -> Int.int
    = app1 (dlsym dlh "ffi_default_abi")

val ffi_cif_length : unit -> Int.int
    = app1 (dlsym dlh "ffi_cif_length")

val ffi_type_length : unit -> Int.int
    = app1 (dlsym dlh "ffi_type_length")

val ffi_closure_length : unit -> Int.int
    = app1 (dlsym dlh "ffi_closure_length")

val ffi_status_values_length : unit -> Int.int
    = app1 (dlsym dlh "ffi_get_statusvalueslength")

val ffi_status_values_entry : Int.int -> (Int.int * String.string)
    = app1 (dlsym dlh "ffi_get_statusvaluesentry")

val ffi_type_values_entry : Int.int -> (Int.int * String.string)
    = app1 (dlsym dlh "ffi_get_typevaluesentry")

val ffi_type_sizes_entry : Int.int -> (Int.int * Int.int * String.string)
    = app1 (dlsym dlh "ffi_get_typesizesentry")

val ffi_type_struct_entry : Int.int -> (Int.int * Int.int * String.string)
    = app1 (dlsym dlh "ffi_get_typestructentry")

val ffi_type_struct_pointers_entry : Int.int -> Word.word * string
    = app1 (dlsym dlh "ffi_get_typestructpointersentry")

val ffi_type_struct_pointers_length : unit -> Int.int
    = app1 (dlsym dlh "ffi_get_typestructpointerslength")

val ffi_type_struct_length : unit -> Int.int
    = app1 (dlsym dlh "ffi_get_typestructlength")

val ffi_type_sizes_length : unit -> Int.int
    = app1 (dlsym dlh "ffi_get_typesizeslength")

val ffi_type_values_length : unit -> Int.int
    = app1 (dlsym dlh "ffi_get_typevalueslength")

val jit_get_arch : unit -> string
    = app1 (dlsym dlh "jit_get_arch")

val jit_get_byteorder : unit -> Int.int
    = app1 (dlsym dlh "jit_get_byteorder")

val jit_get_little_endian : unit -> Int.int
    = app1 (dlsym dlh "jit_get_little_endian")

val jit_get_big_endian : unit -> Int.int
    = app1 (dlsym dlh "jit_get_big_endian")

val jit_get_wordsize : unit -> Int.int
    = app1 (dlsym dlh "jit_get_wordsize")

val jit_get_codetablelength : unit -> Int.int
    = app1 (dlsym dlh "jit_get_codetablelength")

val jit_get_sizeslength : unit -> Int.int
    = app1 (dlsym dlh "jit_get_sizeslength")

val jit_get_constantslength : unit -> Int.int
    = app1 (dlsym dlh "jit_get_constantslength")

val jit_get_codetableentry : Int.int -> Int.int * string
    = app1 (dlsym dlh "jit_get_codetableentry")

val jit_get_sizesentry : Int.int -> Int.int * string
    = app1 (dlsym dlh "jit_get_sizesentry")

val jit_get_constantsentry : Int.int -> Word.word * string
    = app1 (dlsym dlh "jit_get_constantsentry")

val jit_initialise_constants : bool -> unit
    = app1 (dlsym dlh "jit_initialise_constants")

val jit_get_debug : unit -> bool
    = app1 (dlsym dlh "jit_get_debug")

val jit_set_debug : bool -> unit
    = app1 (dlsym dlh "jit_set_debug")

val ffi_default_abi : unit -> Int.int
    = app1 (dlsym dlh "ffi_default_abi")

val ffi_cif_length : unit -> Int.int
    = app1 (dlsym dlh "ffi_cif_length")

val ffi_type_length : unit -> Int.int
    = app1 (dlsym dlh "ffi_type_length")

val ffi_setdryrun : bool -> unit
   = app1 (dlsym dlh "ffi_setdryrun")

val ffi_getdryrun : unit -> bool
   = app1 (dlsym dlh "ffi_getdryrun")

val ffi_dumpbytes : Dynlib.cptr -> Int.int -> unit
   = app2 (dlsym dlh "ffi_dumpbytes")

val ffi_dumpwords : Dynlib.cptr -> Int.int -> unit
   = app2 (dlsym dlh "ffi_dumpwords")

val ffi_dumpvecbytes: vector -> Int.int -> unit
   = app2 (dlsym dlh "ffi_dumpbytes")

val ffi_dumpvecwords: vector -> Int.int -> unit
   = app2 (dlsym dlh "ffi_dumpwords")

val ffi_prep_cif_ : Dynlib.cptr -> Int.int -> Int.int -> Dynlib.cptr -> Dynlib.cptr -> Int.int
    = app5 (dlsym dlh "ffi_prep_cif_")

val ffi_call_ : Dynlib.cptr -> Dynlib.cptr -> Dynlib.cptr -> Dynlib.cptr -> unit
    = app4 (dlsym dlh "ffi_call_")

val ffi_call_n_ : Dynlib.cptr -> Dynlib.cptr -> Dynlib.cptr -> Dynlib.cptr -> unit
    = app4 (dlsym dlh "ffi_call_n_")

fun assq n l =
  let fun lookup k =
     let fun iter [] = raise Fail ("Ffi.assq: internal error: no value in "^
                                               n^" table for key "^k)
           | iter ((k',v)::ps) = if k' = k then v else iter ps
     in iter l
     end
  in lookup
  end;

fun assqi n l =
  let fun lookup k =
     let fun iter [] = raise Fail ("Ffi.assq: internal error: no value in "^
                                      n^" table for key "^(Int.toString k))
           | iter ((k',v)::ps) = if k' = k then v else iter ps
     in iter l
     end
  in lookup
  end;

local
   fun gettbl f l =
      let fun iter 0 acc = (f 0)::acc
            | iter n acc = iter (n-1) ((f n)::acc)
      in iter (l - 1) []
      end
   val tlen = ffi_type_struct_length();
   val trows = gettbl ffi_type_struct_entry tlen
in
   val tstbl = trows
end

local
   val _ = jit_initialise_constants(false);
   fun gettbl f l =
      let fun iter 0 acc = (f 0)::acc
            | iter n acc = iter (n-1) ((f n)::acc)
      in iter (l - 1) []
      end
   val cnstsl = jit_get_constantslength()
   val cnsttbl = gettbl jit_get_constantsentry (cnstsl)
   val ctbl = List.map (fn (v,k) => (k,v)) (cnsttbl)
   val ent = assq "constants_table" ctbl
in
   val WORDSIZE = jit_get_wordsize();
   val BYTE_ORDER = jit_get_byteorder();
   val LITTLE_ENDIAN = jit_get_little_endian();
   val BIG_ENDIAN = jit_get_big_endian();
   val jit_get_constant = ent
   val ctbl = ctbl
end

local 
   fun gettbl f l =
      let fun iter 0 acc = (f 0)::acc
            | iter n acc = iter (n-1) ((f n)::acc)
      in iter (l - 1) []
      end
   val tvl = ffi_type_values_length();
   val vttbl = gettbl ffi_type_values_entry tvl
   val tvtbl = List.map (fn (v,k) => (k,v)) (gettbl ffi_type_values_entry tvl);
   val tvetbl = List.map (fn (v,k) => v) (gettbl ffi_type_values_entry tvl);
   val ent = assq "type_values" tvtbl;
   val tne = assqi "type_values" vttbl;
(* val _ = List.app (fn (k,v) => print ("  | lu FFI_TYPE_"^k^" = \""^k^"\"\n")) tvtbl;
   val _ = List.app (fn (k,v) => print ("  | lui FFI_TYPE_"^k^" = "^(Int.toString v)^"\n")) tvtbl;
   val _ = List.app (fn (k,v) => print ("  | ul \""^k^"\" = FFI_TYPE_"^k^"\n")) tvtbl;
*) val tvev = Vector.tabulate (tvl, fn n => let val (s,n') = List.nth (tvtbl,n) in n' end);
in 
fun lu FFI_TYPE_VOID = "VOID"
  | lu FFI_TYPE_INT = "INT"
  | lu FFI_TYPE_FLOAT = "FLOAT"
  | lu FFI_TYPE_DOUBLE = "DOUBLE"
  | lu FFI_TYPE_LONGDOUBLE = "LONGDOUBLE"
  | lu FFI_TYPE_UINT8 = "UINT8"
  | lu FFI_TYPE_SINT8 = "SINT8"
  | lu FFI_TYPE_UINT16 = "UINT16"
  | lu FFI_TYPE_SINT16 = "SINT16"
  | lu FFI_TYPE_UINT32 = "UINT32"
  | lu FFI_TYPE_SINT32 = "SINT32"
  | lu FFI_TYPE_UINT64 = "UINT64"
  | lu FFI_TYPE_SINT64 = "SINT64"
  | lu FFI_TYPE_STRUCT = "STRUCT"
  | lu FFI_TYPE_POINTER = "POINTER";
fun lui FFI_TYPE_VOID = 0
  | lui FFI_TYPE_INT = 1
  | lui FFI_TYPE_FLOAT = 2
  | lui FFI_TYPE_DOUBLE = 3
  | lui FFI_TYPE_LONGDOUBLE = 4
  | lui FFI_TYPE_UINT8 = 5
  | lui FFI_TYPE_SINT8 = 6
  | lui FFI_TYPE_UINT16 = 7
  | lui FFI_TYPE_SINT16 = 8
  | lui FFI_TYPE_UINT32 = 9
  | lui FFI_TYPE_SINT32 = 10
  | lui FFI_TYPE_UINT64 = 11
  | lui FFI_TYPE_SINT64 = 12
  | lui FFI_TYPE_STRUCT = 13
  | lui FFI_TYPE_POINTER = 14
fun ul "VOID" = FFI_TYPE_VOID
  | ul "INT" = FFI_TYPE_INT
  | ul "FLOAT" = FFI_TYPE_FLOAT
  | ul "DOUBLE" = FFI_TYPE_DOUBLE
  | ul "LONGDOUBLE" = FFI_TYPE_LONGDOUBLE
  | ul "UINT8" = FFI_TYPE_UINT8
  | ul "SINT8" = FFI_TYPE_SINT8
  | ul "UINT16" = FFI_TYPE_UINT16
  | ul "SINT16" = FFI_TYPE_SINT16
  | ul "UINT32" = FFI_TYPE_UINT32
  | ul "SINT32" = FFI_TYPE_SINT32
  | ul "UINT64" = FFI_TYPE_UINT64
  | ul "SINT64" = FFI_TYPE_SINT64
  | ul "STRUCT" = FFI_TYPE_STRUCT
  | ul "POINTER" = FFI_TYPE_POINTER
  | ul s = raise Fail ("Ffi.typeEnumFromInt: internal error:\
                      \ no type_values_table entry for type index "^s);
fun listmax l = Option.getOpt ((List.foldl (fn (n,NONE) => SOME n
                                             | (n,SOME m) => SOME (if n > m then n else m))
                                            Int.minInt l), 0);
fun list_index l i = 
    let val (r,_) = List.foldl 
                        (fn (i',(a,n)) =>
                               if (i' = i) 
                                  then (SOME n,n+1)
                                  else (a,n+1))
                        (NONE,0) l
    in r
    end;
val etvv = Vector.tabulate ((listmax tvetbl) + 1, (fn i => SOME (ul (tne i)) handle Fail _ => NONE));
val tvev = Vector.fromList tvetbl
fun typeEnumToInt te = Vector.sub (tvev, lui te) 
          handle Subscript => raise Fail "Ffi.typeEnumToInt: internal error."
fun typeEnumFromInt i = Vector.sub (etvv, i) handle Subscript => NONE
end

local 
   fun gettbl f l =
      let fun iter 0 acc = (f 0)::acc
            | iter n acc = iter (n-1) ((f n)::acc)
      in iter (l - 1) []
      end
   val tsl = ffi_type_sizes_length();
   val tstbl = List.map (fn (_,v,k) => (k,v)) (gettbl ffi_type_sizes_entry tsl);
   val tsetbl = List.map (fn (_,v,k) => v) (gettbl ffi_type_sizes_entry tsl);
   val ent = assq "type_sizes" tstbl;
   val tatbl = List.map (fn (v,_,k) => (k,v)) (gettbl ffi_type_sizes_entry tsl);
   val taetbl = List.map (fn (v,_,k) => v) (gettbl ffi_type_sizes_entry tsl);
   val aent = assq "type_alignment" tatbl;
in
  val typeEnumToSize = ent o lu
  val typeEnumToAlign = aent o lu
end

local
   fun gettbl f l =
      let fun iter 0 acc = (f 0)::acc
            | iter n acc = iter (n-1) ((f n)::acc)
      in iter (l - 1) []
      end
   val tspl = ffi_type_struct_pointers_length();
   val tsptbl = List.map (fn (v,k) => (k,v)) (gettbl ffi_type_struct_pointers_entry (tspl));
(* val _ = List.app (fn (k,v) => print (" | ffi_type_"^k^"\n")) tsptbl;
   val _ = List.app (fn (k,v) => print (" | lu ffi_type_"^k^" = ent \""^k^"\" \n")) tsptbl;
*) val ent = assq "type_struct_pointers" tsptbl;

   fun lu ffi_type_void = ent "void" 
     | lu ffi_type_uchar = ent "uchar" 
     | lu ffi_type_schar = ent "schar" 
     | lu ffi_type_uint = ent "uint" 
     | lu ffi_type_sint = ent "sint" 
     | lu ffi_type_ushort = ent "ushort" 
     | lu ffi_type_sshort = ent "sshort" 
     | lu ffi_type_ulong = ent "ulong" 
     | lu ffi_type_slong = ent "slong" 
     | lu ffi_type_float = ent "float" 
     | lu ffi_type_double = ent "double" 
     | lu ffi_type_longdouble = ent "longdouble" 
     | lu ffi_type_pointer = ent "pointer" 
     | lu ffi_type_uint8 = ent "uint8" 
     | lu ffi_type_sint8 = ent "sint8" 
     | lu ffi_type_uint16 = ent "uint16" 
     | lu ffi_type_sint16 = ent "sint16" 
     | lu ffi_type_uint32 = ent "uint32" 
     | lu ffi_type_sint32 = ent "sint32" 
     | lu ffi_type_uint64 = ent "uint64" 
     | lu ffi_type_sint64 = ent "sint64";
in
  val typeToStructPointer = lu
end

local
fun lui FFI_TYPE_VOID = ffi_type_void
  | lui FFI_TYPE_INT = ffi_type_sint
  | lui FFI_TYPE_FLOAT = ffi_type_float
  | lui FFI_TYPE_DOUBLE = ffi_type_double
  | lui FFI_TYPE_LONGDOUBLE = ffi_type_longdouble
  | lui FFI_TYPE_UINT8 = ffi_type_uint8
  | lui FFI_TYPE_SINT8 = ffi_type_sint8
  | lui FFI_TYPE_UINT16 = ffi_type_uint16
  | lui FFI_TYPE_SINT16 = ffi_type_sint16
  | lui FFI_TYPE_UINT32 = ffi_type_uint32
  | lui FFI_TYPE_SINT32 = ffi_type_sint32
  | lui FFI_TYPE_UINT64 = ffi_type_uint64
  | lui FFI_TYPE_SINT64 = ffi_type_sint64
  | lui FFI_TYPE_STRUCT = raise Fail "typepFromEnum: internal error: no ffi_type struct for FFI_TYPE_STRUCT"
  | lui FFI_TYPE_POINTER = ffi_type_pointer
in
   fun typepFromTypeEnum e =
      let val tse = lui e
          val tspw = typeToStructPointer tse
      in tspw 
      end
end

datatype ffi_status_enum =
   FFI_OK
 | FFI_BAD_TYPEDEF
 | FFI_BAD_ABI

local
   fun gettbl f l =
      let fun iter 0 acc = (f 0)::acc
            | iter n acc = iter (n-1) ((f n)::acc)
      in iter (l - 1) []
      end
   val svl = ffi_status_values_length();
   val svtbl = List.map (fn (v,k) => (k,v)) (gettbl ffi_status_values_entry (svl));
(* val _ = List.app (fn (k,v) => print (" | FFI_"^k^"\n")) svtbl;
   val _ = List.app (fn (k,v) => print (" | lu FFI_"^k^" = ent \""^k^"\" \n")) svtbl;
   val _ = List.app (fn (k,v) => print (" | ul "^(Int.toString (ent k))^" = FFI_"^k^" \n")) svtbl;
*) val ent = assq "status_values" svtbl;
   val tne = (fn s => "FFI_"^s) o (assqi "status_values" (gettbl ffi_status_values_entry (svl)));
   fun lu FFI_OK = ent "OK" 
     | lu FFI_BAD_TYPEDEF = ent "BAD_TYPEDEF" 
     | lu FFI_BAD_ABI = ent "BAD_ABI"
   fun ul 0 = FFI_OK 
     | ul 1 = FFI_BAD_TYPEDEF 
     | ul 2 = FFI_BAD_ABI 
     | ul n = raise Fail ("Ffi.statusEnumFromInt: unknown libffi status "^(Int.toString n))
in
   val statusEnumToInt = lu
   val statusEnumFromInt = ul
   val statusIntToString = tne
end

local
   fun gettbl f l =
      let fun iter 0 acc = (f 0)::acc
            | iter n acc = iter (n-1) ((f n)::acc)
      in iter (l - 1) []
      end
   val ctl = jit_get_codetablelength();
   val ctbl = List.map (fn (v,k) => (k,v)) (gettbl jit_get_codetableentry (ctl));
   val ent = assq "code_table" ctbl;
in
   val jit_code = ent
end

local
   val nulv = Word8Vector.fromList [0w0,0w0,0w0,0w0]
in
   val NULLvec = nulv
   val NULL = svec_setcptrvalue(nulv);
end

prim_val svec_getvecstring : Word8Vector.vector -> string
          = 1 "identity"

val realToDoubleVector = Mosml.doubleVec
val doubleVectorToReal = Mosml.vecDouble
val realToFloatVector = Mosml.floatVec
val floatVectorToReal = Mosml.vecFloat

val vectorToList = Word8Vector.foldr (fn (x,a)=> x::a) []
val listToVector = Word8Vector.fromList

(* This is a rather inefficient way of computing the identity function!
   Better to use prim 1 "identity" *)
val vectorFromString = Word8Vector.fromList o (List.map (Word8.fromInt o ord)) o explode
val stringFromVector = Word8Vector.foldl (fn (b,s) => s^(String.str (Char.chr (Word8.toInt b)))) ""

val vectorFromWordVector =
  Word8Vector.concat o (Vector.foldr (fn (w,a) => (svec_setvecword w)::a) [])

val wordVectorFromVector =
  Vector.fromList o
      (fn (_,a) => a) o 
            (Word8Vector.foldr
                 (fn (b,(p,a)) => 
                      if List.length(p)=3 
                         then ([],(svec_getvecword (Word8Vector.fromList ((b::p))))::a)
                         else (b::p,a))
                 ([],[]))

val vectorFromWordList = vectorFromWordVector o Vector.fromList
val wordListFromVector = (Vector.foldr (fn (e,a) => e::a) []) o wordVectorFromVector

fun svec_from_vector v =
    let val sv = svec_make(Word8Vector.length v)
        val vl = svec_setvalue sv 0 v
    in sv
    end

val svec_from_string = svec_from_vector o vectorFromString

fun svec_to_string sv = 
      String.implode
        (Word8Array.foldr 
           (fn (w,a) => (Char.chr (Word8.toInt w))::a) [] (svec_getbufferarray sv));

val svec_from_word_vector = svec_from_vector o vectorFromWordVector

fun svec_cptr_byte_offset cptr offs =
    svec_setcptrword (Word.+(svec_getcptrword cptr,Word.fromInt offs));

fun svec_byte_offset svec = svec_cptr_byte_offset (svec_getbuffercptr svec);

fun svec_dumpb sv offs length = 
   let val avail = (svec_getlength sv) - offs
       val len = if length > avail then avail else length
   in if len <= 0 
      then () 
      else ffi_dumpbytes (svec_byte_offset sv offs) len
   end;

fun svec_dumpw sv offs length = 
   let val avail = ((svec_getlength sv) - offs) div 4;
       val len = if length > avail then avail else length
   in if len <= 0 
      then () 
      else ffi_dumpwords (svec_byte_offset sv offs) len
   end;

fun svec_dumpb_all sv = svec_dumpb sv 0 (svec_getlength sv)
fun svec_dumpw_all sv = svec_dumpw sv 0 (svec_getlength sv div 4)

fun vec_byte_offset vec offs =
    Word8VectorSlice.vector (Word8VectorSlice.slice (vec,offs,NONE));

fun vec_dumpb v offs length = 
   let val avail = (Word8Vector.length v) - offs
       val len = if length > avail then avail else length
   in if len <= 0 
      then () 
      else ffi_dumpvecbytes (vec_byte_offset v offs) len
   end;

fun vec_dumpw v offs length = 
   let val avail = ((Word8Vector.length v) - offs) div 4;
       val len = if length > avail then avail else length
   in if len <= 0 
      then () 
      else ffi_dumpvecwords (vec_byte_offset v offs) len
   end;

fun vec_dumpb_all v = vec_dumpb v 0 (Word8Vector.length v)
fun vec_dumpw_all v = vec_dumpw v 0 (Word8Vector.length v div 4)

fun svec_debug dumper =
  let fun dump msg sv =
      let val debug = jit_get_debug()
      in if debug 
            then (jit_set_debug(false);
                  print (msg^"\n");
                  dumper sv;
                  jit_set_debug(debug))
            else ()
      end
  in dump end;

val svec_debugb = svec_debug svec_dumpb_all

val svec_debugw = svec_debug svec_dumpw_all

val vec_debugb = svec_debug vec_dumpb_all

val vec_debugw = svec_debug vec_dumpw_all

val ffi_default_abi_number = ffi_default_abi()

val ffi_cif_struct_size = ffi_cif_length()

val ffi_type_struct_size = ffi_type_length()

val ffi_closure_size = ffi_closure_length()

prim_val var  : Dynlib.cptr -> 'b                                    = 1 "c_var"
prim_val app1 : Dynlib.cptr -> 'a1 -> 'b                             = 2 "cfun_app1"
prim_val app2 : Dynlib.cptr -> 'a1 -> 'a2 -> 'b                      = 3 "cfun_app2"
prim_val app3 : Dynlib.cptr -> 'a1 -> 'a2 -> 'a3 -> 'b               = 4 "cfun_app3"
prim_val app4 : Dynlib.cptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'b        = 5 "cfun_app4"
prim_val app5 : Dynlib.cptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b = 6 "cfun_app5"


val heap_start : unit -> Dynlib.cptr = fn () => (Dynlib.cptr (Dynlib.dlsym dlxh "heap_start"))
val heap_end : unit -> Dynlib.cptr = fn () => (Dynlib.cptr (Dynlib.dlsym dlxh "heap_end"))
val gc_phase : unit -> int = fn () => var (Dynlib.cptr (Dynlib.dlsym dlxh "gc_phase"))

val gc_minor : unit-> unit = app1 (Dynlib.cptr (Dynlib.dlsym dlxh "gc_minor"))
val gc_major : unit-> unit = app1 (Dynlib.cptr (Dynlib.dlsym dlxh "gc_major"))
val gc_full_major : unit-> unit = app1 (Dynlib.cptr (Dynlib.dlsym dlxh "gc_full_major"))


local
   val debug = jit_get_debug()
   fun ffi_arg_types l =
       let fun iter acc (Pointer _) = ffi_type_pointer::acc
             | iter acc (Structure l) = (List.rev (ffi_arg_types l))@acc 
             | iter acc (Function _) = raise Fail "Ffi.ffi_type: argument is a function."
             | iter acc (VariadicFunction _) = raise Fail "Ffi.ffi_type:\
                                                      \ argument is a variadic function."
             | iter acc (Array(Incomplete,t)) = iter acc (Pointer t)
             | iter acc (Array _) = raise Fail "Ffi.ffi_type: can't deal with arrays yet, sorry."
             | iter acc (Integer(Signed,Int)) = ffi_type_sint::acc
             | iter acc (Integer(Unsigned,Int)) = ffi_type_uint::acc
             | iter acc (Integer(Signed,Short)) = ffi_type_sshort::acc
             | iter acc (Integer(Unsigned,Short)) = ffi_type_ushort::acc
             | iter acc (Integer(Signed,Long)) = ffi_type_slong::acc
             | iter acc (Integer(Unsigned,Long)) = ffi_type_ulong::acc
             | iter acc (Character(Signed)) = ffi_type_schar::acc
             | iter acc (Character(Unsigned)) = ffi_type_uchar::acc
             | iter acc (Float(Double)) = ffi_type_double::acc
             | iter acc (Float(Single)) = ffi_type_float::acc
             | iter acc (Float(LongDouble)) = ffi_type_longdouble::acc
             | iter acc (Const(t)) = iter acc t
             | iter [] (Void) = []
             | iter _ (Void) = raise Fail "Ffi.ffi_type: argument is a void value."
      in List.foldl (fn (t,a) => iter a t) [] l 
      end
   fun ffi_type_unary t =
       let val l = ffi_arg_types [t]
       in case l
            of [] => ffi_type_void
             | [s] => s
             | _ => raise Fail "Ffi.ffi_type: internal error."
       end
   fun ffi_make_trampoline_types (Function(Structure(args),ret)) = 
         let val argtypes = List.rev (ffi_arg_types args) 
             val nargs = List.length args
         in {nargs = nargs, argtypes = argtypes, retval = ret}
         end
     | ffi_make_trampoline_types (Function(arg,ret)) = 
         let val argtypes = case arg of Void => [] | _ => [ffi_type_unary arg]
             val nargs = List.length argtypes
         in {nargs = nargs, argtypes = argtypes, retval = ret}
         end
     | ffi_make_trampoline_types _ = raise Fail "ffi_make_trampoline: not a function type."
   fun alignsize te =
      let val sz = typeEnumToSize te
          val asz = typeEnumToSize FFI_TYPE_POINTER
      in if sz mod asz <> 0 then asz * (sz div asz + 1) else sz
      end
   fun ffi_make_trampoline_svec {nargs = nargs, argtypes = argtypes, retval = retval} =
       let val rvsize = 
                case retval 
                  of NONE => 0
                   | SOME FFI_TYPE_VOID => 0
                   | SOME te => alignsize te 
           val argslen = (List.length argtypes) * (typeEnumToSize FFI_TYPE_POINTER)
           val ciflen = ffi_cif_struct_size
           val argstypesoff = ciflen
           val argsoff = argstypesoff + argslen
           val rvoff = argsoff + argslen
           val svecsize = ciflen + 2 * argslen + rvsize 
           val svec = svec_make(svecsize)
           val svecpw = svec_getcptrword (svec_getbuffercptr(svec))
           val argstypespw = svecpw + (Word.fromInt argstypesoff)
           val argspw = svecpw + (Word.fromInt argsoff)
           val rvpw = if rvsize = 0 then 0wx0 else argspw + (Word.fromInt argslen)
       in {  svec = svec,  argstypesoff = argstypesoff, argsoff = argsoff, rvoff = rvoff, 
           svecpw = svecpw, argstypespw = argstypespw,   argspw = argspw,   rvpw = rvpw}
       end
   fun ffi_make_trampoline doit name fp t afn rfn = 
         let fun debug dfn msg = dfn (name^": "^msg^": ")
             val debugw = debug svec_debugw 
             val debugvw = debug vec_debugw
             val vecZeros = fn n => Word8Vector.tabulate (n, fn _ => 0wx0)
             val (ttr as {nargs = nargs, argtypes = argtypes, retval = retval})
              = ffi_make_trampoline_types t
             val (svdata as {  svec = svec,  argstypesoff = argstypesoff, argsoff = argsoff, rvoff = rvoff,
                             svecpw = svecpw, argstypespw = argstypespw,   argspw = argspw,   rvpw = rvpw})
              = ffi_make_trampoline_svec ttr
             val _ = debugw "got svec" svec
             val argtypepws = Vector.fromList (List.map typeToStructPointer argtypes)
             val argtypeps = vectorFromWordVector argtypepws
             val _ = debugvw "argtypeps" argtypeps
             val cifp = svec_setcptrword svecpw
             val abi = ffi_default_abi()
             val argstypesp = svec_setcptrword argstypespw
             val argstypesoff = ffi_cif_struct_size
             val atpl = svec_setvalue svec argstypesoff argtypeps
             val _ = debugw "set argtypes, svec buffer is now" svec
             val cspl = svec_setvalue svec 0 (vecZeros ffi_cif_struct_size)
             val _ = debugw "zero'ed cif, svec buffer is now" svec
             val argspl = svec_setvalue svec argsoff (vecZeros (rvoff - argsoff))
             val _ = debugw "zero'ed args, svec buffer is now" svec
             val rvp = svec_setcptrword rvpw
             val rvtypep = case retval
                             of NONE => NULL
                              | SOME rvt => svec_setcptrword (typepFromTypeEnum rvt)
             val rvpl = svec_setvalue svec rvoff (vecZeros ((svec_getlength svec) - rvoff))
             val _ = debugw "set rv, svec buffer is now" svec
             val pcrv = if doit then ffi_prep_cif_  cifp abi nargs rvtypep argstypesp else 0
             val _ = debugw "prep_cif returned, svec buffer is now" svec
             val pcr = statusEnumFromInt pcrv
         in if pcr <> FFI_OK
               then raise Fail ("Ffi.ffi_make_trampoline_fn: internal error:\
                                 \ ffi_prep_cif returned "^(statusIntToString pcrv))
               else
                  fn args =>
                      let val vecNull = Word8Vector.fromList []
                          val argsps = if nargs = 0
                                          then case Word8Vector.length (afn (* tag *) args)
                                                 of 0 => vecNull
                                                  | _ => raise Fail ("Ffi.ffi_make_trampoline: "
                                                                       ^name^": arity")
                                          else afn (* tag *) args
                          val _ = debugvw "argsps" argsps
                          val argsp = svec_setcptrword argspw
                          val apl = svec_setvalue svec argsoff argsps
                          val _ = debugw "set argsps, svec buffer is now" svec
                          val ffi_call = if nargs > 1 then ffi_call_n_ else ffi_call_ 
                          val () = if doit then ffi_call cifp fp rvp argsp else ()
                          val _ = debugw "ffi_call returned, svec buffer is now" svec
                          val rvec = case retval 
                                       of NONE => vecNull
                                        | SOME FFI_TYPE_VOID => vecNull
                                        | SOME _ => svec_getvalue svec rvoff 
                                                     ((svec_getlength svec) - rvoff)
                      in rfn rvec
                      end
         end
in
   val ffi_trampoline = ffi_make_trampoline true
end;

fun mkargssvec argsl =
   let fun mkargsps p =
             let fun iter (n, (p,pvs)) =
                       let val p' = Word.+(p,Word.fromInt n)
                       in (p', Word8Vector.concat [pvs,svec_setvecword p])
                       end
                 val nilv = Word8Vector.fromList []
                 fun extract (_,v) = v
             in extract o (List.foldl iter (p,nilv))
             end
       val args = Word8Vector.concat argsl
       val svec = svec_make(Word8Vector.length args)
       val svl = svec_setvalue svec 0 args
       val () = svec_debugw ("mkargssvec: args svec["^(Int.toString svl)^"] is") svec
       val svpw = svec_getvecword (svec_getpointervalue svec)
       val argsv = mkargsps svpw (List.map Word8Vector.length argsl)
       val () = vec_debugw ("mkargssvec: argsv["^(Int.toString (Word8Vector.length argsv))^"] is") argsv
   in (argsv,svec)
   end;

end
