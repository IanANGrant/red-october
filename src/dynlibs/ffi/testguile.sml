val () = List.app Meta.load ["Jit", "IntInf"];

val _ = Meta.quotation := true;

val _ = Jit.jit_set_memory_functions Ffi.my_alloc Ffi.my_realloc Ffi.my_free;

val () = Jit.init_jit Jit.argv0;

local open Dynlib
in
   val dlxh = dlopen {lib = "", flag = Set[RTLD_LAZY,RTLD_DEEPBIND], global = false}
end

fun jitptr hdl s =
  Jit.Pointer
    (Ffi.svec_getcptrvalue
       (Dynlib.cptr (Dynlib.dlsym hdl s)))

val bcopyp = jitptr dlxh "bcopy"
val alloc_stringp = jitptr dlxh "alloc_string"
val string_lengthp = jitptr dlxh "string_length"

val scm_c_eval_stringp = jitptr dlxh "scm_c_eval_string"
val scm_boolean_pp = jitptr dlxh "scm_boolean_p"
val scm_to_boolp = jitptr dlxh "scm_to_bool"
val scm_char_pp = jitptr dlxh "scm_char_p"
val scm_symbol_pp = jitptr dlxh "scm_symbol_p"
val scm_string_pp = jitptr dlxh "scm_string_p"
val scm_keyword_pp = jitptr dlxh "scm_keyword_p"
val scm_eq_pp = jitptr dlxh "scm_eq_p"
val scm_equal_pp = jitptr dlxh "scm_equal_p"

val scm_c_make_bytevectorp = jitptr dlxh "scm_c_make_bytevector"
val scm_bytevector_pp = jitptr dlxh "scm_bytevector_p"
val scm_bytevector_eq_pp = jitptr dlxh "scm_bytevector_eq_p"
val scm_c_make_bytevectorp = jitptr dlxh "scm_c_make_bytevector"
val scm_c_bytevector_lengthp = jitptr dlxh "scm_c_bytevector_length"
val scm_utf8_to_stringp = jitptr dlxh "scm_utf8_to_string"
val scm_string_to_utf8p = jitptr dlxh "scm_string_to_utf8"

val scm_pair_pp = jitptr dlxh "scm_pair_p"
val scm_list_pp = jitptr dlxh "scm_list_p"
val scm_null_pp = jitptr dlxh "scm_null_p"
val scm_integer_pp = jitptr dlxh "scm_integer_p"
val scm_exact_integer_pp = jitptr dlxh "scm_exact_integer_p"
val scm_real_pp = jitptr dlxh "scm_real_p"
val scm_rational_pp = jitptr dlxh "scm_rational_p"
val scm_finite_pp = jitptr dlxh "scm_finite_p"
val scm_inf_pp = jitptr dlxh "scm_inf_p"
val scm_nan_pp = jitptr dlxh "scm_nan_p"

val scm_is_signed_integerp = jitptr dlxh "scm_is_signed_integer"
val scm_is_unsigned_integerp = jitptr dlxh "scm_is_unsigned_integer"
val scm_to_longp = jitptr dlxh (if Jit.WORDSIZE = 32 then "scm_to_int32" else "scm_to_int64")
val scm_to_ulongp = jitptr dlxh (if Jit.WORDSIZE = 32 then "scm_to_uint32" else "scm_to_uint64")
val scm_from_ulongp = jitptr dlxh (if Jit.WORDSIZE = 32 then "scm_from_uint32" else "scm_from_uint64")
val scm_from_longp = jitptr dlxh (if Jit.WORDSIZE = 32 then "scm_from_int32" else "scm_from_int64")
val scm_to_mpzp = jitptr dlxh "scm_to_mpz"
val scm_from_mpzp = jitptr dlxh "scm_from_mpz"
val scm_from_pointerp = jitptr dlxh "scm_from_pointer"
val scm_to_pointerp = jitptr dlxh "scm_to_pointer"
val scm_pointer_pp = jitptr dlxh "scm_pointer_p"
val scm_pointer_addressp = jitptr dlxh "scm_pointer_address"
val scm_pointer_to_bytevectorp = jitptr dlxh "scm_pointer_to_bytevector"
val scm_bytevector_to_pointerp = jitptr dlxh "scm_bytevector_to_pointer"

val scm_array_pp = jitptr dlxh "scm_array_p"
val scm_typed_array_pp = jitptr dlxh "scm_typed_array_p"

val scm_primitive_evalp = jitptr dlxh "scm_primitive_eval"
val scm_symbol_to_keywordp = jitptr dlxh "scm_symbol_to_keyword"
val scm_keyword_to_symbolp = jitptr dlxh "scm_keyword_to_symbol"
val scm_symbol_to_stringp = jitptr dlxh "scm_symbol_to_string"
val scm_string_to_symbolp = jitptr dlxh "scm_string_to_symbol"
val scm_from_utf8_symbolp = jitptr dlxh "scm_from_utf8_symbol"
val scm_consp = jitptr dlxh "scm_cons"
val scm_carp = jitptr dlxh "scm_car"
val scm_cdrp = jitptr dlxh "scm_cdr"

val scm_defined_pp = jitptr dlxh "scm_defined_p"
val scm_variable_pp = jitptr dlxh "scm_variable_p"
val scm_variable_bound_pp = jitptr dlxh "scm_variable_bound_p"
val scm_variable_refp = jitptr dlxh "scm_variable_ref"
val scm_public_variablep = jitptr dlxh "scm_public_variable"
val scm_call_np = jitptr dlxh "scm_call_n"

val wsz = Jit.WORDSIZE div 8
val first_atoms = Jit.Pointer (Ffi.svec_getcptrvalue Ffi.first_atoms_)

fun jit_atom0 (jit_, v1) =
   let open Jit
       val _ = jit_movi_p (jit_, v1, first_atoms)
       val _ = jit_addi (jit_, v1, v1, wsz * 1)
   in ()
   end

fun jit_atom (jit_, v1, v2) =
   let open Jit
       val _ = jit_muli (jit_, v2, v2, wsz)
       val _ = jit_movi_p (jit_, v1, first_atoms)
       val _ = jit_addi (jit_, v1, v1, wsz * 1)
       val _ = jit_addr (jit_, v1, v1, v2)
   in ()
   end

local open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0) (* V1 = Field(v,0)   *)
      val _ = jit_ldxi (jit_, V2, V0, wsz * 1) (* V2 = Field(v,1)  *)
      val _ = jit_subi (jit_, R0, V2, wsz * 1) (* R0 = Op_val(V2) *)
      val _ = jit_ldr (jit_, R1, R0)           (* R1 = Hd_op(R0) *)
      val _ = jit_rshi (jit_, V0, R1, 10)      (* V0 = Wosize_hd(R1) *)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_pushargr (jit_, V2)
      val _ = jit_pushargr (jit_, V0)
      val _ = jit_finishi (jit_, scm_call_np)
      val _ = jit_retval (jit_, R1)
      val _ = jit_retr (jit_, R1)
      val fptr = jit_emit (jit_)
   in
       val scm_call_n = Ffi.app1 fptr : cptr * (cptr Vector.vector) -> cptr
   end

local open Jit
   val jit_ = Jit.jit_new_state ()
   val () = jit_prolog (jit_)
   val v = jit_arg (jit_)
   val () = jit_getarg (jit_, V0, v)
   val _ = jit_prepare (jit_)
   val _ = jit_pushargr (jit_, V0)
   val _ = jit_finishi (jit_, scm_c_eval_stringp) 
   val _ = jit_retval (jit_, R0)
   val _ = jit_retr (jit_, R0)
   val scm_eval_stringptr = jit_emit (jit_)
in
   val scm_eval_string : string -> cptr = Ffi.app1 scm_eval_stringptr
end

fun qlToString l =
   let fun iter r [] = r
         | iter r ((QUOTE s)::fs) = iter (r^s) fs
         | iter r ((ANTIQUOTE s)::fs) = iter (r^s) fs
   in iter "" l
   end;

val scm_evalq = scm_eval_string o qlToString

fun scm_pred1 jit_pred =
   let open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val () = jit_pred (jit_, R0, V0)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, R0)
      val _ = jit_finishi (jit_, scm_to_boolp)
      val _ = jit_retval (jit_, R0)
      val _ = jit_atom (jit_,R1, R0)
      val _ = jit_retr (jit_, R1)
      val fptr = jit_emit (jit_)
   in
       Ffi.app1 fptr : cptr -> bool
   end

fun scm_c_pred1 jit_pred =
   let open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val () = jit_pred (jit_, R0, V0)
      val _ = jit_atom (jit_,R1, R0)
      val _ = jit_retr (jit_, R1)
      val fptr = jit_emit (jit_)
   in
       Ffi.app1 fptr : cptr -> bool
   end

fun scm_pred2 jit_pred =
   let open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0) (* V1 = Field(v,0)   *)
      val _ = jit_ldxi (jit_, V2, V0, wsz * 1) (* V2 = Field(v,1)  *)
      val () = jit_pred (jit_, R0, V1, V2)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, R0)
      val _ = jit_finishi (jit_, scm_to_boolp)
      val _ = jit_retval (jit_, R0)
      val _ = jit_atom (jit_,R1, R0)
      val _ = jit_retr (jit_, R1)
      val fptr = jit_emit (jit_)
   in
       Ffi.app1 fptr : cptr * cptr -> bool
   end

local open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0) (* V1 = Field(v,0)   *)
      val _ = jit_ldxi (jit_, V2, V0, wsz * 1) (* V2 = Field(v,1)  *)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_pushargr (jit_, V2)
      val _ = jit_finishi (jit_, scm_to_mpzp)
      val _ = jit_atom0 (jit_, R1)
      val _ = jit_retr (jit_, R1)
      val fptr = jit_emit (jit_)
   in
       val scm_to_mpz = Ffi.app1 fptr : cptr * Dynlib.cptr -> unit
   end

local open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0) (* V1 = Field(v,0)   *)
      val _ = jit_ldxi (jit_, V2, V0, wsz * 1) (* V2 = Field(v,1)  *)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_pushargr (jit_, V2)
      val _ = jit_finishi (jit_, scm_from_pointerp)
      val _ = jit_retval (jit_, R1)
      val _ = jit_retr (jit_, R1)
      val fptr = jit_emit (jit_)
   in
       val scm_from_pointer = Ffi.app1 fptr : Dynlib.cptr * Dynlib.cptr -> cptr
   end

(* SCM_API SCM scm_pointer_to_bytevector (SCM pointer, SCM type,
                                       SCM offset, SCM len);
   SCM_API SCM scm_bytevector_to_pointer (SCM bv, SCM offset); *)

local open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_prepare (jit_)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0) (* V1 = Field(v,0)   *)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 1) (* V1 = Field(v,1)  *)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 2) (* V1 = Field(v,2)  *)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 3) (* V1 = Field(v,3)  *)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_finishi (jit_, scm_pointer_to_bytevectorp)
      val _ = jit_retval (jit_, R1)
      val _ = jit_retr (jit_, R1)
      val fptr = jit_emit (jit_)
   in
       val scm_pointer_to_bytevector = Ffi.app1 fptr :
             cptr * cptr * cptr * cptr-> cptr
   end

(* This should be a generic set of functions implementing the macros
   SCM_IMP, SCM_CELL_TYPE, SCM_CELL_WORD_n etc. defined in
   libguile/tags.h and libguile/gc.h *)

local open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 2) (* V2 = Field(v,2)  *)
      val _ = jit_retr (jit_, V1)
      val fptr = jit_emit (jit_)
   in
       val scm_c_bytevector_contents = Ffi.app1 fptr : cptr -> Dynlib.cptr
   end

local open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, V0)
      val _ = jit_finishi (jit_, string_lengthp)
      val _ = jit_retval (jit_, V1)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_finishi (jit_, scm_c_make_bytevectorp)
      val _ = jit_retval (jit_, V2)
      val _ = jit_ldxi (jit_, R0, V2, wsz * 2) (* R0 = Field(V2,2) = bytevector_contents  *)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, V0)
      val _ = jit_pushargr (jit_, R0)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_finishi (jit_, bcopyp)
      val _ = jit_retr (jit_, V2)
      val fptr = jit_emit (jit_)
   in
       val sml_string_to_bytevector = Ffi.app1 fptr : String.string -> cptr
       val sml_vector_to_bytevector = Ffi.app1 fptr : Word8Vector.vector -> cptr
   end

local open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 1) (* V1 = Field(v,1)  *)
      val _ = jit_ldxi (jit_, V2, V0, wsz * 2) (* V2 = Field(v,2)  *)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_finishi (jit_, alloc_stringp)
      val _ = jit_retval (jit_, V0)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, V2)
      val _ = jit_pushargr (jit_, V0)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_finishi (jit_, bcopyp)
      val _ = jit_retr (jit_, V0)
      val fptr = jit_emit (jit_)
   in
       val sml_string_from_bytevector = Ffi.app1 fptr : cptr -> String.string
       val sml_vector_from_bytevector = Ffi.app1 fptr : cptr -> Word8Vector.vector
   end

local open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0) (* V1 = Field(v,0)  *)
      val _ = jit_ldxi (jit_, V2, V0, wsz * 1) (* V2 = Field(v,1)  *)
      val _ = jit_rshi (jit_, V2, V2, 1)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, V2)
      val _ = jit_finishi (jit_, alloc_stringp)
      val _ = jit_retval (jit_, V0)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_pushargr (jit_, V0)
      val _ = jit_pushargr (jit_, V2)
      val _ = jit_finishi (jit_, bcopyp)
      val _ = jit_retr (jit_, V0)
      val fptr = jit_emit (jit_)
   in
       val sml_string_from_cptr = Ffi.app1 fptr : Dynlib.cptr * Int.int -> String.string
       val sml_vector_from_cptr = Ffi.app1 fptr : Dynlib.cptr * Int.int -> Word8Vector.vector
   end

fun scm_unary jit_unop =
   let open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val () = jit_unop (jit_, R0, V0)
      val _ = jit_retr (jit_, R0)
      val fptr = jit_emit (jit_)
   in
       Ffi.app1 fptr : cptr -> cptr
   end

fun scm_binary jit_binop =
   let open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0) (* V1 = Field(v,0)   *)
      val _ = jit_ldxi (jit_, V2, V0, wsz * 1) (* V2 = Field(v,1)  *)
      val () = jit_binop (jit_, R0, V1, V2)
      val _ = jit_retr (jit_, R0)
      val fptr = jit_emit (jit_)
   in
       Ffi.app1 fptr : cptr * cptr -> cptr
   end

fun scm_unary_int jit_unop =
   let open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val () = jit_unop (jit_, R0, V0)
      val _ = jit_lshi (jit_, R0, R0, 1)
      val _ = jit_addi (jit_, R0, R0, 1)
      val _ = jit_retr (jit_, R0)
      val fptr = jit_emit (jit_)
   in
       Ffi.app1 fptr : cptr -> Int.int
   end

fun scm_int_unary jit_unop =
   let open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_rshi (jit_, V0, V0, 1)
      val () = jit_unop (jit_, R0, V0)
      val _ = jit_retr (jit_, R0)
      val fptr = jit_emit (jit_)
   in
       Ffi.app1 fptr : Int.int -> cptr
   end

fun scm_unary_word jit_unop =
   let open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val () = jit_unop (jit_, R0, V0)
      val _ = jit_lshi (jit_, R0, R0, 1)
      val _ = jit_addi (jit_, R0, R0, 1)
      val _ = jit_retr (jit_, R0)
      val fptr = jit_emit (jit_)
   in
       Ffi.app1 fptr : cptr -> Word.word
   end

fun scm_word_unary jit_unop =
   let open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_rshi (jit_, V0, V0, 1)
      val () = jit_unop (jit_, R0, V0)
      val _ = jit_retr (jit_, R0)
      val fptr = jit_emit (jit_)
   in
       Ffi.app1 fptr : Word.word -> cptr
   end

fun scm_cptr_unary jit_unop =
   let open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val () = jit_unop (jit_, R0, V0)
      val _ = jit_retr (jit_, R0)
      val fptr = jit_emit (jit_)
   in
       Ffi.app1 fptr : Dynlib.cptr -> cptr
   end

local
   open Jit
   fun jit_unopf scm_funp =
      fn (jit_,r1,r0) =>
         let
             val _ = jit_prepare (jit_)
             val _ = jit_pushargr (jit_, r0)
             val _ = jit_finishi (jit_, scm_funp)
             val _ = jit_retval (jit_, r1)
         in () end
   fun jit_val_long (jit_,r0) =
         let
             val _ = jit_lshi (jit_, r0, r0, 1)
             val _ = jit_addi (jit_, r0, r0, 1)
         in () end
   fun jit_intconv scm_funp jit_unop vmin vmax =
      fn (jit_,r1,r0) =>
         let
             val _ = jit_prepare (jit_)
             val _ = jit_pushargr (jit_, r0)
             val _ = jit_pushargi (jit_, vmin)
             val _ = jit_pushargi (jit_, ~1)
             val _ = jit_pushargi (jit_, vmax)
             val _ = jit_pushargi (jit_, 0)
             val _ = jit_finishi (jit_, scm_funp)
             val _ = jit_retval (jit_, r0)
             val _ = jit_unop (jit_, r1, r0)
         in () end
   fun jit_uintconv scm_funp jit_unop vmin vmax =
      fn (jit_,r1,r0) =>
         let
             val _ = jit_prepare (jit_)
             val _ = jit_pushargr (jit_, r0)
             val _ = jit_pushargi_u (jit_, vmin)
             val _ = jit_pushargi_u (jit_, 0w0)
             val _ = jit_pushargi_u (jit_, vmax)
             val _ = jit_pushargi_u (jit_, 0w0)
             val _ = jit_finishi (jit_, scm_funp)
             val _ = jit_retval (jit_, r0)
             val _ = jit_unop (jit_, r1, r0)
         in () end
   fun jit_binop scm_funp =
      fn (jit_,r2,r1,r0) =>
         let
             val _ = jit_prepare (jit_)
             val _ = jit_pushargr (jit_, r1)
             val _ = jit_pushargr (jit_, r0)
             val _ = jit_finishi (jit_, scm_funp)
             val _ = jit_retval (jit_, r2)
         in () end
   fun jit_nop (jit_,r1,r0) =
         let val _ = jit_movr (jit_, r1, r0)
         in () end
    val jit_uintp = jit_uintconv scm_is_unsigned_integerp jit_nop 0w0 (Word.fromInt (~1))
    val jit_intp  = jit_intconv scm_is_signed_integerp jit_nop 
                       (Option.valOf Int.minInt)
                       (Option.valOf Int.maxInt)
in
   val scm_boolean_p = scm_pred1 (jit_unopf scm_boolean_pp)  
   val scm_is_true = scm_pred1 jit_nop
   val scm_char_p = scm_pred1 (jit_unopf scm_char_pp)
   val scm_string_p = scm_pred1 (jit_unopf scm_string_pp)  
   val scm_keyword_p = scm_pred1 (jit_unopf scm_keyword_pp)  
   val scm_symbol_p = scm_pred1 (jit_unopf scm_symbol_pp)  
   val scm_bytevector_p = scm_pred1 (jit_unopf scm_bytevector_pp)  
   val scm_pair_p = scm_pred1 (jit_unopf scm_pair_pp)  
   val scm_list_p = scm_pred1 (jit_unopf scm_list_pp)  
   val scm_integer_p = scm_pred1 (jit_unopf scm_integer_pp)
   val scm_exact_integer_p = scm_pred1 (jit_unopf scm_exact_integer_pp)  
   val scm_unsigned_integer_p = scm_c_pred1 jit_uintp
   val scm_signed_integer_p = scm_c_pred1 jit_intp
   val scm_real_p = scm_pred1 (jit_unopf scm_real_pp)
   val scm_rational_p = scm_pred1 (jit_unopf scm_rational_pp)
   val scm_finite_p = scm_pred1 (jit_unopf scm_finite_pp)
   val scm_inf_p = scm_pred1 (jit_unopf scm_inf_pp)
   val scm_nan_p = scm_pred1 (jit_unopf scm_nan_pp)
   val scm_null_p = scm_pred1 (jit_unopf scm_null_pp)
   val scm_variable_p = scm_pred1 (jit_unopf scm_variable_pp)
   val scm_variable_bound_p = scm_pred1 (jit_unopf scm_variable_bound_pp)
   val scm_array_p = scm_pred2 (jit_binop scm_array_pp)
   val scm_equal_p = scm_pred2 (jit_binop scm_equal_pp)  
   val scm_eq_p = scm_pred2 (jit_binop scm_eq_pp)  
   val scm_typed_array_p = scm_pred2 (jit_binop scm_typed_array_pp)
   val scm_defined_p = scm_pred2 (jit_binop scm_defined_pp)
   val scm_bytevector_to_pointer = scm_binary (jit_binop scm_bytevector_to_pointerp)
   val scm_to_pointer = scm_unary (jit_unopf scm_to_pointerp)
   val scm_cons = scm_binary (jit_binop scm_consp)
   val scm_car = scm_unary (jit_unopf scm_carp)
   val scm_cdr = scm_unary (jit_unopf scm_cdrp)
   val scm_symbol_to_string = scm_unary (jit_unopf scm_symbol_to_stringp)
   val scm_string_to_symbol = scm_unary (jit_unopf scm_string_to_symbolp)
   val scm_symbol_to_keyword = scm_unary (jit_unopf scm_symbol_to_keywordp)
   val scm_keyword_to_symbol = scm_unary (jit_unopf scm_keyword_to_symbolp)
   val scm_utf8_to_string = scm_unary (jit_unopf scm_utf8_to_stringp)
   val scm_string_to_utf8 = scm_unary (jit_unopf scm_string_to_utf8p)
   val scm_primitive_eval = scm_unary (jit_unopf scm_primitive_evalp)
   val scm_variable_ref = scm_unary (jit_unopf scm_variable_refp)
   val scm_to_ulong = scm_unary_word (jit_unopf scm_to_ulongp)
   val scm_to_long = scm_unary_int (jit_unopf scm_to_longp)
   val scm_from_ulong = scm_word_unary (jit_unopf scm_from_ulongp)
   val scm_from_long = scm_int_unary (jit_unopf scm_from_longp)
   val scm_from_mpz = scm_cptr_unary (jit_unopf scm_from_mpzp)
   val scm_c_bytevector_length = scm_unary_int (jit_unopf scm_c_bytevector_lengthp)
   val scm_public_variable = scm_binary (jit_binop scm_public_variablep)
end

fun scm_c_const s =
   let val scm_consts = [("TRUE",4),("FALSE",0), (* see libguile/tags.h *)
                         ("NIL",1),("EOL",3),
                         ("UNSPECIFIED",8),
                         ("UNDEFINED",9),("EOF",10),
                         ("UNBOUND",11)]
       val n = List.find (fn (s',_) => s'= s) scm_consts
       prim_val cptrToScm : Dynlib.cptr -> cptr = 1 "identity"
   in
      case n of NONE => raise Fail ("scm_const: "^s^" is not the name of a known constant")
              | SOME (_,n) => cptrToScm (Ffi.svec_setcptrword
                                 (Word.+(Word.<<(Word.fromInt n,0w8),0w4))) (* Don't ask! *)
   end

val scm_true = scm_c_const "TRUE"
val scm_false = scm_c_const "FALSE"
val scm_nil = scm_c_const "EOL"
val scm_eof = scm_c_const "EOF"
val scm_undefined = scm_c_const "UNDEFINED"
val scm_unspecified = scm_c_const "UNSPECIFIED"

fun scm_is_array a = scm_array_p (a,scm_undefined)

val scm_string = scm_utf8_to_string o sml_string_to_bytevector 
val scm_symbol = scm_string_to_symbol o scm_string
val sml_string = sml_string_from_bytevector o scm_string_to_utf8

val scm_list = List.foldr scm_cons scm_nil

fun scm_pub_ref module varname =
   let val modlist = scm_list o (List.map scm_symbol)
       val modsym = modlist module
       val varsym = scm_symbol varname
       val variable = scm_public_variable (modsym,varsym)
   in if scm_boolean_p variable
         andalso scm_eq_p(variable,scm_false)
      then raise Fail ("scm_pub_ref: not bound: "^varname)
      else scm_variable_ref variable
   end

val scm_display_proc = scm_pub_ref ["guile"] "display"
val scm_read_proc =    scm_pub_ref ["guile"] "read"
val scm_newline_proc = scm_pub_ref ["guile"] "newline"
val scm_callwis_proc = scm_pub_ref ["guile"] "call-with-input-string"

fun scm_read s = scm_call_n (scm_callwis_proc,#[scm_string s,scm_read_proc])
val scm_readq = scm_read o qlToString

local
   val scmdisplayproc = scm_evalq ` 
                        (lambda (x)
		           (with-fluids ((%default-port-encoding "UTF-8"))
		              (call-with-output-string
                                (lambda (p)
                                  (display x p)))))`
 (* Will these local <cptr> SCM objects be protected from GC by the
      Guile collector?  I need to understand how variable binding is
      implemented in the CAML bytecode interpreter. If there are C
      stack pointers to bound object values, then this is OK, because
      the Guile GC will be tracing the CAML runtime's C stack looking
      for references during its mark phase.
      But whether or not this is a problem: a better way to do this
      sort of thing is to use R6RS modules to bind references to
      functions like these. So we define a "red-october" module at
      init-time, and this contains all the useful procs and things we
      want to rely on being around for the whole time the Scheme
      sub-system is loaded. *)
in
   fun sml_string_display scm = sml_string (scm_call_n (scmdisplayproc,#[scm]))
end

val _ = scm_evalq `
    (define (factorial n)           
       (let loop ((n n) (product 1))
          (if (= n 0)
               product
               (loop (- n 1) ( * product n)))))
`;

exception Scheme of Dynlib.cptr

fun scm_repl () =
   let val quitv = scm_list [scm_symbol "quit"]
       val readv = scm_list [scm_symbol "read"]
       fun read p = (print p; scm_primitive_eval readv)
       fun loop sexp =
         if scm_equal_p (quitv,sexp) orelse scm_equal_p (scm_eof,sexp)
            then ()
            else let val res = scm_primitive_eval sexp
                 in if scm_eq_p (res,scm_unspecified)
                       then () 
                       else print ((sml_string_display res)^"\n");
                    loop (read "> ")
                 end handle Language scm => 
                       (print ("Uncaught scheme exception: "^(sml_string_display scm)^".\n");
                        loop (read "> "))
   in
      loop (read "Moscow ML Guile REPL\nType `(quit)' to exit.\n> ")
   end

fun sml_largeint i =
   let val mpz = IntInf.init2 (0x16,0x0);
       val mpzp = IntInf.getCptr mpz;
       val _ = scm_to_mpz (i,mpzp);
   in mpz
   end

val scmrnum = scm_evalq `(factorial 50)`
val mpz = sml_largeint scmrnum;
val fact50 = IntInf.toString mpz;
val mpzscm = scm_from_mpz (IntInf.getCptr mpz)
val true = fact50 = sml_string_display mpzscm

fun scm_start_repl_server () =
   scm_evalq `(use-modules (system repl server))
              (spawn-server)
              (display "A Guile REPL is now running on localhost:37146\n")`;

val scmstr = scm_evalq `"Hello, World\n"`;
val scmbv = scm_string_to_utf8 scmstr;
val str = sml_string_from_cptr (scm_c_bytevector_contents scmbv, scm_c_bytevector_length scmbv);
val w8v = sml_vector_from_cptr (scm_c_bytevector_contents scmbv, scm_c_bytevector_length scmbv);
val chlst = Word8Vector.foldr (fn (w,a) => (Char.chr(Word8.toInt w))::a) [] w8v;

val scmrnum = scm_call_n (scm_display_proc,#[scm_eval_string "(list 'red 'yellow 'green)"])
val _ = scm_call_n (scm_newline_proc,#[]);
val scmrlist = scm_call_n (scm_callwis_proc, #[scm_eval_string "\"(list 1 2 3)\"", scm_read_proc])
val bools = scm_list [scm_true, scm_false, scm_false]
val boolsstr = scm_call_n (scm_display_proc,#[bools])
val _ = scm_call_n (scm_newline_proc,#[]);
 
(* This seems to segfault mosml:
val sv = Ffi.svec_from_string "read";
val svcptr = Ffi.svec_getbuffercptr sv;
val scmcptr = scm_from_pointer (svcptr,Ffi.NULL);
val bv = scm_pointer_to_bytevector (scmcptr,scm_eval_string "4",
                                      scm_c_const "UNDEFINED",
                                      scm_c_const "UNDEFINED");
*)

val scmbplusdefd = scm_defined_p(scm_symbol "+",scm_c_const "UNDEFINED");

val scm_shellp = Jit.Pointer (Ffi.svec_getcptrvalue (Dynlib.cptr (Dynlib.dlsym dlxh "scm_shell")))

local open Jit
   val jit_ = Jit.jit_new_state ()
   val () = jit_prolog (jit_)
   val v = jit_arg (jit_)
   val () = jit_getarg (jit_, V0, v)
   val _ = jit_ldxi (jit_, V1, V0, wsz * 0) (* V1 = Field(v,0)   *)
   val _ = jit_rshi (jit_, V1, V1, 1)       (* V1 = Long_val(V1) *)
   val _ = jit_ldxi (jit_, V2, V0, wsz * 1) (* V2 = Field(v,1)  *)
   val _ = jit_prepare (jit_)
   val _ = jit_pushargr (jit_, V1)
   val _ = jit_pushargr (jit_, V2)
   val _ = jit_finishi (jit_, scm_shellp) 
   val _ = jit_atom0 (jit_, R0)
   val _ = jit_retr (jit_, R0)
   val scm_shellptr = jit_emit (jit_)
in
   val scm_shell : int * (Dynlib.cptr * Dynlib.cptr) -> unit = Ffi.app1 scm_shellptr
end

val argv0ptr = Ffi.svec_getbuffercptr Jit.argv0svec;
val NULLptr = Ffi.svec_setcptrvalue Ffi.NULLvec;

(*
val _ = scm_shell (1,(argv0ptr,NULLptr));

(* Never reached *)
*)

fun mkregmap l =
    scm_list (List.map 
                 (fn s => scm_cons (scm_symbol s,
                                    scm_from_ulong (Ffi.jit_get_constant s))) l)

val gprm = mkregmap ["R0","R1","R2","V0","V1","V2","FP"];
val fprm = mkregmap ["F0","F1","F2","F3","F4","F5","F6"];

val defs = scm_readq `(list
         (use-modules (system foreign))
         (use-modules (rnrs enums))
         (define-enumeration lgt-gpr (list R0 R1 R2 V0 V1 V2 FP) mk-lgt-gpr)
         (define-enumeration lgt-fpr (list F0 F1 F2 F3 F4 F5 F6) mk-lgt-fpr)
         (define liblightning (dynamic-link "liblightning"))
         (define lgt-init (pointer->procedure void (dynamic-func "init_jit" liblightning) (list '(* *))))
         (define argv0 (make-c-struct (list '* '*)
                                      (list (string->pointer "/home/ian3/usr/bin/guile") %null-pointer)))
         (lgt-init argv0)
         (define-wrapped-pointer-type lgt-state
            lgt-state?
            lgt-wrap-state lgt-unwrap-state
            (lambda (s p)
              (format p "#<lgt-state ~x>"
                      (pointer-address (lgt-unwrap-state s)))))
         (define-wrapped-pointer-type lgt-node
            lgt-node?
            lgt-wrap-node lgt-unwrap-node
            (lambda (s p)
              (format p "#<lgt-node ~x>"
                      (pointer-address (lgt-unwrap-node s)))))
         (define lgt-new-state
            ;; Wrapper for jit_new_state.
            (let ((jit-new-state (pointer->procedure '* (dynamic-func "jit_new_state" liblightning) '())))
              (lambda ()
                "Return a new JIT state."
                (lgt-wrap-state (jit-new-state)))))
         (define lgt-emit
            ;; Wrapper for jit_emit.
            (let ((jit-emit (pointer->procedure '* (dynamic-func "_jit_emit" liblightning) (list '*))))
              (lambda (jit-state)
                "Return the compiled code."
                (jit-emit (lgt-unwrap-state jit-state)))))
          (write (lgt-new-state))
)`

val scmwriteproc = scm_evalq ` 
                        (lambda (x)
		           (with-fluids ((%default-port-encoding "UTF-8"))
	 	              (call-with-output-string
                                (lambda (p)
                                  (write x p)))))`

fun scm_write exp = sml_string (scm_call_n (scmwriteproc,#[exp]));

fun scm_type t =
   case t
     of "noderef" => scm_symbol "*"
      | "state" => scm_symbol "*"
      | "code" => scm_symbol "uint32"
      | "void" => scm_symbol "()"
      | "int" => scm_symbol "int"
      | "float" => scm_symbol "float"
      | "double" => scm_symbol "double"
      | "pointer" => scm_symbol "*"
      | "word" => scm_symbol "unsigned-long"
      | "fpr" => scm_symbol "uint32"
      | "gpr" => scm_symbol "uint32"
      | s => raise Fail ("scm_type: no case "^s)

val scm_argtypes =
   let fun iter acc [] = acc
         | iter (pnms,atys) ((p as (v,t))::ps) = 
               iter (scm_cons (scm_symbol v, pnms),
                     scm_cons (scm_type t,   atys)) ps
   in iter (scm_nil, scm_nil)
   end

fun scm_qlToString l =
   let fun iter r [] = r
         | iter r ((QUOTE s)::fs) = iter (r^s) fs
         | iter r ((ANTIQUOTE s)::fs) = iter (r^(scm_write s)) fs
   in iter "" l
   end;

val scm_readq = scm_read o scm_qlToString

fun defwrapper l =
   let fun iter acc [] = acc
         | iter acc ((retval, name, args, (call,_))::rest) =
      let val rvt = scm_type retval
          val (ans,ats) = scm_argtypes args
          val cln = scm_string call
          val nam = scm_symbol name
      in iter (scm_cons (
         scm_readq `
           (define ^(nam))`,acc)) rest
      end
   in
      iter scm_nil l
   end

val infra =
[("pointer", "jit_address", [("_jit", "state"), ("node", "noderef")],
  ("_jit_address", ["_jit", "node"])),
 ("int", "jit_allocai", [("_jit", "state"), ("u", "word")],
  ("_jit_allocai", ["_jit", "u"])),
 ("noderef", "jit_arg", [("_jit", "state")], ("_jit_arg", ["_jit"])),
 ("noderef", "jit_arg_d", [("_jit", "state")], ("_jit_arg_d", ["_jit"])),
 ("noderef", "jit_arg_f", [("_jit", "state")], ("_jit_arg_f", ["_jit"])),
 ("void", "jit_clear_state", [("_jit", "state")],
  ("_jit_clear_state", ["_jit"])),
 ("void", "jit_destroy_state", [("_jit", "state")],
  ("_jit_destroy_state", ["_jit"])),
 ("void", "jit_disassemble", [("_jit", "state")],
  ("_jit_disassemble", ["_jit"])),
 ("void", "jit_ellipsis", [("_jit", "state")], ("_jit_ellipsis", ["_jit"])),
 ("pointer", "jit_emit", [("_jit", "state")], ("_jit_emit", ["_jit"])),
 ("void", "jit_epilog", [("_jit", "state")], ("_jit_epilog", ["_jit"])),
 ("noderef", "jit_finishi", [("_jit", "state"), ("u", "pointer")],
  ("_jit_finishi", ["_jit", "u"])),
 ("void", "jit_finishr", [("_jit", "state"), ("u", "gpr")],
  ("_jit_finishr", ["_jit", "u"])),
 ("void", "jit_forward", [("_jit", "state")], ("_jit_forward", ["_jit"])),
 ("void", "jit_getarg_c", [("_jit", "state"), ("u", "gpr"), ("v", "noderef")],
  ("_jit_getarg_c", ["_jit", "u", "v"])),
 ("void", "jit_getarg_d", [("_jit", "state"), ("u", "fpr"), ("v", "noderef")],
  ("_jit_getarg_d", ["_jit", "u", "v"])),
 ("void", "jit_getarg_f", [("_jit", "state"), ("u", "fpr"), ("v", "noderef")],
  ("_jit_getarg_f", ["_jit", "u", "v"])),
 ("void", "jit_getarg_i", [("_jit", "state"), ("u", "gpr"), ("v", "noderef")],
  ("_jit_getarg_i", ["_jit", "u", "v"])),
 ("void", "jit_getarg_s", [("_jit", "state"), ("u", "gpr"), ("v", "noderef")],
  ("_jit_getarg_s", ["_jit", "u", "v"])),
 ("void", "jit_getarg_uc", [("_jit", "state"), ("u", "gpr"), ("v", "noderef")],
  ("_jit_getarg_uc", ["_jit", "u", "v"])),
 ("void", "jit_getarg_us", [("_jit", "state"), ("u", "gpr"), ("v", "noderef")],
  ("_jit_getarg_us", ["_jit", "u", "v"])),
 ("noderef", "jit_indirect", [("_jit", "state")], ("_jit_indirect", ["_jit"])),
 ("void", "jit_label", [("_jit", "state")], ("_jit_label", ["_jit"])),
 ("void", "jit_patch", [("_jit", "state"), ("u", "noderef")],
  ("_jit_patch", ["_jit", "u"])),
 ("void", "jit_patch_abs",
  [("_jit", "state"), ("u", "noderef"), ("v", "pointer")],
  ("_jit_patch_abs", ["_jit", "u", "v"])),
 ("void", "jit_patch_at",
  [("_jit", "state"), ("u", "noderef"), ("v", "noderef")],
  ("_jit_patch_at", ["_jit", "u", "v"])),
 ("void", "jit_prepare", [("_jit", "state")], ("_jit_prepare", ["_jit"])),
 ("void", "jit_print", [("_jit", "state")], ("_jit_print", ["_jit"])),
 ("void", "jit_prolog", [("_jit", "state")], ("_jit_prolog", ["_jit"])),
 ("void", "jit_pushargi", [("_jit", "state"), ("u", "word")],
  ("_jit_pushargi", ["_jit", "u"])),
 ("void", "jit_pushargi_d", [("_jit", "state"), ("u", "double")],
  ("_jit_pushargi_d", ["_jit", "u"])),
 ("void", "jit_pushargi_f", [("_jit", "state"), ("u", "float")],
  ("_jit_pushargi_f", ["_jit", "u"])),
 ("void", "jit_pushargr", [("_jit", "state"), ("u", "gpr")],
  ("_jit_pushargr", ["_jit", "u"])),
 ("void", "jit_pushargr_d", [("_jit", "state"), ("u", "fpr")],
  ("_jit_pushargr_d", ["_jit", "u"])),
 ("void", "jit_pushargr_f", [("_jit", "state"), ("u", "fpr")],
  ("_jit_pushargr_f", ["_jit", "u"])),
 ("void", "jit_realize", [("_jit", "state")], ("_jit_realize", ["_jit"])),
 ("void", "jit_ret", [("_jit", "state")], ("_jit_ret", ["_jit"])),
 ("void", "jit_reti", [("_jit", "state"), ("u", "word")],
  ("_jit_reti", ["_jit", "u"])),
 ("void", "jit_reti_d", [("_jit", "state"), ("u", "double")],
  ("_jit_reti_d", ["_jit", "u"])),
 ("void", "jit_reti_f", [("_jit", "state"), ("u", "float")],
  ("_jit_reti_f", ["_jit", "u"])),
 ("void", "jit_retr", [("_jit", "state"), ("u", "gpr")],
  ("_jit_retr", ["_jit", "u"])),
 ("void", "jit_retr_d", [("_jit", "state"), ("u", "fpr")],
  ("_jit_retr_d", ["_jit", "u"])),
 ("void", "jit_retr_f", [("_jit", "state"), ("u", "fpr")],
  ("_jit_retr_f", ["_jit", "u"])),
 ("void", "jit_retval_c", [("_jit", "state"), ("u", "gpr")],
  ("_jit_retval_c", ["_jit", "u"])),
 ("void", "jit_retval_d", [("_jit", "state"), ("u", "fpr")],
  ("_jit_retval_d", ["_jit", "u"])),
 ("void", "jit_retval_f", [("_jit", "state"), ("u", "fpr")],
  ("_jit_retval_f", ["_jit", "u"])),
 ("void", "jit_retval_i", [("_jit", "state"), ("u", "gpr")],
  ("_jit_retval_i", ["_jit", "u"])),
 ("void", "jit_retval_s", [("_jit", "state"), ("u", "gpr")],
  ("_jit_retval_s", ["_jit", "u"])),
 ("void", "jit_retval_uc", [("_jit", "state"), ("u", "gpr")],
  ("_jit_retval_uc", ["_jit", "u"])),
 ("void", "jit_retval_us", [("_jit", "state"), ("u", "gpr")],
  ("_jit_retval_us", ["_jit", "u"]))];

val insns =
[("noderef", "jit_absr_d", [("_jit", "state"), ("u", "fpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_absr_d", "u", "v"])),
 ("noderef", "jit_absr_f", [("_jit", "state"), ("u", "fpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_absr_f", "u", "v"])),
 ("noderef", "jit_addci",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_addci", "u", "v", "w"])),
 ("noderef", "jit_addcr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_addcr", "u", "v", "w"])),
 ("noderef", "jit_addi",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_addi", "u", "v", "w"])),
 ("noderef", "jit_addi_d",
  [("_jit", "state"), ("u", "fpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_addi_d", "u", "v", "w"])),
 ("noderef", "jit_addi_f",
  [("_jit", "state"), ("u", "fpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_addi_f", "u", "v", "w"])),
 ("noderef", "jit_addr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_addr", "u", "v", "w"])),
 ("noderef", "jit_addr_d",
  [("_jit", "state"), ("u", "fpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_addr_d", "u", "v", "w"])),
 ("noderef", "jit_addr_f",
  [("_jit", "state"), ("u", "fpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_addr_f", "u", "v", "w"])),
 ("noderef", "jit_addxi",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_addxi", "u", "v", "w"])),
 ("noderef", "jit_addxr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_addxr", "u", "v", "w"])),
 ("noderef", "jit_andi",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_andi", "u", "v", "w"])),
 ("noderef", "jit_andr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_andr", "u", "v", "w"])),
 ("noderef", "jit_beqi", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_beqi", "NULL", "v", "w"])),
 ("noderef", "jit_beqi_d", [("_jit", "state"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_pwd", ["_jit", "jit_code_beqi_d", "NULL", "v", "w"])),
 ("noderef", "jit_beqi_f", [("_jit", "state"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_pwf", ["_jit", "jit_code_beqi_f", "NULL", "v", "w"])),
 ("noderef", "jit_beqr", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_beqr", "NULL", "v", "w"])),
 ("noderef", "jit_beqr_d", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_beqr_d", "NULL", "v", "w"])),
 ("noderef", "jit_beqr_f", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_beqr_f", "NULL", "v", "w"])),
 ("noderef", "jit_bgei", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_bgei", "NULL", "v", "w"])),
 ("noderef", "jit_bgei_d", [("_jit", "state"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_pwd", ["_jit", "jit_code_bgei_d", "NULL", "v", "w"])),
 ("noderef", "jit_bgei_f", [("_jit", "state"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_pwf", ["_jit", "jit_code_bgei_f", "NULL", "v", "w"])),
 ("noderef", "jit_bgei_u", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_bgei_u", "NULL", "v", "w"])),
 ("noderef", "jit_bger", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bger", "NULL", "v", "w"])),
 ("noderef", "jit_bger_d", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bger_d", "NULL", "v", "w"])),
 ("noderef", "jit_bger_f", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bger_f", "NULL", "v", "w"])),
 ("noderef", "jit_bger_u", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bger_u", "NULL", "v", "w"])),
 ("noderef", "jit_bgti", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_bgti", "NULL", "v", "w"])),
 ("noderef", "jit_bgti_d", [("_jit", "state"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_pwd", ["_jit", "jit_code_bgti_d", "NULL", "v", "w"])),
 ("noderef", "jit_bgti_f", [("_jit", "state"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_pwf", ["_jit", "jit_code_bgti_f", "NULL", "v", "w"])),
 ("noderef", "jit_bgti_u", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_bgti_u", "NULL", "v", "w"])),
 ("noderef", "jit_bgtr", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bgtr", "NULL", "v", "w"])),
 ("noderef", "jit_bgtr_d", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bgtr_d", "NULL", "v", "w"])),
 ("noderef", "jit_bgtr_f", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bgtr_f", "NULL", "v", "w"])),
 ("noderef", "jit_bgtr_u", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bgtr_u", "NULL", "v", "w"])),
 ("noderef", "jit_blei", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_blei", "NULL", "v", "w"])),
 ("noderef", "jit_blei_d", [("_jit", "state"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_pwd", ["_jit", "jit_code_blei_d", "NULL", "v", "w"])),
 ("noderef", "jit_blei_f", [("_jit", "state"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_pwf", ["_jit", "jit_code_blei_f", "NULL", "v", "w"])),
 ("noderef", "jit_blei_u", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_blei_u", "NULL", "v", "w"])),
 ("noderef", "jit_bler", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bler", "NULL", "v", "w"])),
 ("noderef", "jit_bler_d", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bler_d", "NULL", "v", "w"])),
 ("noderef", "jit_bler_f", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bler_f", "NULL", "v", "w"])),
 ("noderef", "jit_bler_u", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bler_u", "NULL", "v", "w"])),
 ("noderef", "jit_bltgti_d",
  [("_jit", "state"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_pwd", ["_jit", "jit_code_bltgti_d", "NULL", "v", "w"])),
 ("noderef", "jit_bltgti_f", [("_jit", "state"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_pwf", ["_jit", "jit_code_bltgti_f", "NULL", "v", "w"])),
 ("noderef", "jit_bltgtr_d", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bltgtr_d", "NULL", "v", "w"])),
 ("noderef", "jit_bltgtr_f", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bltgtr_f", "NULL", "v", "w"])),
 ("noderef", "jit_blti", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_blti", "NULL", "v", "w"])),
 ("noderef", "jit_blti_d", [("_jit", "state"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_pwd", ["_jit", "jit_code_blti_d", "NULL", "v", "w"])),
 ("noderef", "jit_blti_f", [("_jit", "state"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_pwf", ["_jit", "jit_code_blti_f", "NULL", "v", "w"])),
 ("noderef", "jit_blti_u", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_blti_u", "NULL", "v", "w"])),
 ("noderef", "jit_bltr", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bltr", "NULL", "v", "w"])),
 ("noderef", "jit_bltr_d", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bltr_d", "NULL", "v", "w"])),
 ("noderef", "jit_bltr_f", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bltr_f", "NULL", "v", "w"])),
 ("noderef", "jit_bltr_u", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bltr_u", "NULL", "v", "w"])),
 ("noderef", "jit_bmci", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_bmci", "NULL", "v", "w"])),
 ("noderef", "jit_bmcr", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bmcr", "NULL", "v", "w"])),
 ("noderef", "jit_bmsi", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_bmsi", "NULL", "v", "w"])),
 ("noderef", "jit_bmsr", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bmsr", "NULL", "v", "w"])),
 ("noderef", "jit_bnei", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_bnei", "NULL", "v", "w"])),
 ("noderef", "jit_bnei_d", [("_jit", "state"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_pwd", ["_jit", "jit_code_bnei_d", "NULL", "v", "w"])),
 ("noderef", "jit_bnei_f", [("_jit", "state"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_pwf", ["_jit", "jit_code_bnei_f", "NULL", "v", "w"])),
 ("noderef", "jit_bner", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bner", "NULL", "v", "w"])),
 ("noderef", "jit_bner_d", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bner_d", "NULL", "v", "w"])),
 ("noderef", "jit_bner_f", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bner_f", "NULL", "v", "w"])),
 ("noderef", "jit_boaddi", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_boaddi", "NULL", "v", "w"])),
 ("noderef", "jit_boaddi_u", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_boaddi_u", "NULL", "v", "w"])),
 ("noderef", "jit_boaddr", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_boaddr", "NULL", "v", "w"])),
 ("noderef", "jit_boaddr_u", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_boaddr_u", "NULL", "v", "w"])),
 ("noderef", "jit_bordi_d", [("_jit", "state"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_pwd", ["_jit", "jit_code_bordi_d", "NULL", "v", "w"])),
 ("noderef", "jit_bordi_f", [("_jit", "state"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_pwf", ["_jit", "jit_code_bordi_f", "NULL", "v", "w"])),
 ("noderef", "jit_bordr_d", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bordr_d", "NULL", "v", "w"])),
 ("noderef", "jit_bordr_f", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bordr_f", "NULL", "v", "w"])),
 ("noderef", "jit_bosubi", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_bosubi", "NULL", "v", "w"])),
 ("noderef", "jit_bosubi_u", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_bosubi_u", "NULL", "v", "w"])),
 ("noderef", "jit_bosubr", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bosubr", "NULL", "v", "w"])),
 ("noderef", "jit_bosubr_u", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bosubr_u", "NULL", "v", "w"])),
 ("noderef", "jit_buneqi_d",
  [("_jit", "state"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_pwd", ["_jit", "jit_code_buneqi_d", "NULL", "v", "w"])),
 ("noderef", "jit_buneqi_f", [("_jit", "state"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_pwf", ["_jit", "jit_code_buneqi_f", "NULL", "v", "w"])),
 ("noderef", "jit_buneqr_d", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_buneqr_d", "NULL", "v", "w"])),
 ("noderef", "jit_buneqr_f", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_buneqr_f", "NULL", "v", "w"])),
 ("noderef", "jit_bungei_d",
  [("_jit", "state"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_pwd", ["_jit", "jit_code_bungei_d", "NULL", "v", "w"])),
 ("noderef", "jit_bungei_f", [("_jit", "state"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_pwf", ["_jit", "jit_code_bungei_f", "NULL", "v", "w"])),
 ("noderef", "jit_bunger_d", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bunger_d", "NULL", "v", "w"])),
 ("noderef", "jit_bunger_f", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bunger_f", "NULL", "v", "w"])),
 ("noderef", "jit_bungti_d",
  [("_jit", "state"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_pwd", ["_jit", "jit_code_bungti_d", "NULL", "v", "w"])),
 ("noderef", "jit_bungti_f", [("_jit", "state"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_pwf", ["_jit", "jit_code_bungti_f", "NULL", "v", "w"])),
 ("noderef", "jit_bungtr_d", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bungtr_d", "NULL", "v", "w"])),
 ("noderef", "jit_bungtr_f", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bungtr_f", "NULL", "v", "w"])),
 ("noderef", "jit_bunlei_d",
  [("_jit", "state"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_pwd", ["_jit", "jit_code_bunlei_d", "NULL", "v", "w"])),
 ("noderef", "jit_bunlei_f", [("_jit", "state"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_pwf", ["_jit", "jit_code_bunlei_f", "NULL", "v", "w"])),
 ("noderef", "jit_bunler_d", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bunler_d", "NULL", "v", "w"])),
 ("noderef", "jit_bunler_f", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bunler_f", "NULL", "v", "w"])),
 ("noderef", "jit_bunlti_d",
  [("_jit", "state"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_pwd", ["_jit", "jit_code_bunlti_d", "NULL", "v", "w"])),
 ("noderef", "jit_bunlti_f", [("_jit", "state"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_pwf", ["_jit", "jit_code_bunlti_f", "NULL", "v", "w"])),
 ("noderef", "jit_bunltr_d", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bunltr_d", "NULL", "v", "w"])),
 ("noderef", "jit_bunltr_f", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bunltr_f", "NULL", "v", "w"])),
 ("noderef", "jit_bunordi_d",
  [("_jit", "state"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_pwd", ["_jit", "jit_code_bunordi_d", "NULL", "v", "w"])),
 ("noderef", "jit_bunordi_f",
  [("_jit", "state"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_pwf", ["_jit", "jit_code_bunordi_f", "NULL", "v", "w"])),
 ("noderef", "jit_bunordr_d", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bunordr_d", "NULL", "v", "w"])),
 ("noderef", "jit_bunordr_f", [("_jit", "state"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bunordr_f", "NULL", "v", "w"])),
 ("noderef", "jit_bxaddi", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_bxaddi", "NULL", "v", "w"])),
 ("noderef", "jit_bxaddi_u", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_bxaddi_u", "NULL", "v", "w"])),
 ("noderef", "jit_bxaddr", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bxaddr", "NULL", "v", "w"])),
 ("noderef", "jit_bxaddr_u", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bxaddr_u", "NULL", "v", "w"])),
 ("noderef", "jit_bxsubi", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_bxsubi", "NULL", "v", "w"])),
 ("noderef", "jit_bxsubi_u", [("_jit", "state"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_pww", ["_jit", "jit_code_bxsubi_u", "NULL", "v", "w"])),
 ("noderef", "jit_bxsubr", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bxsubr", "NULL", "v", "w"])),
 ("noderef", "jit_bxsubr_u", [("_jit", "state"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_pww", ["_jit", "jit_code_bxsubr_u", "NULL", "v", "w"])),
 ("noderef", "jit_calli", [("_jit", "state"), ("u", "pointer")],
  ("jit_new_node_p", ["_jit", "jit_code_calli", "u"])),
 ("noderef", "jit_callr", [("_jit", "state"), ("u", "gpr")],
  ("jit_new_node_w", ["_jit", "jit_code_callr", "u"])),
 ("noderef", "jit_comr", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_comr", "u", "v"])),
 ("noderef", "jit_divi",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_divi", "u", "v", "w"])),
 ("noderef", "jit_divi_d",
  [("_jit", "state"), ("u", "fpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_divi_d", "u", "v", "w"])),
 ("noderef", "jit_divi_f",
  [("_jit", "state"), ("u", "fpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_divi_f", "u", "v", "w"])),
 ("noderef", "jit_divi_u",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_divi_u", "u", "v", "w"])),
 ("noderef", "jit_divr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_divr", "u", "v", "w"])),
 ("noderef", "jit_divr_d",
  [("_jit", "state"), ("u", "fpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_divr_d", "u", "v", "w"])),
 ("noderef", "jit_divr_f",
  [("_jit", "state"), ("u", "fpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_divr_f", "u", "v", "w"])),
 ("noderef", "jit_divr_u",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_divr_u", "u", "v", "w"])),
 ("noderef", "jit_eqi",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_eqi", "u", "v", "w"])),
 ("noderef", "jit_eqi_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_eqi_d", "u", "v", "w"])),
 ("noderef", "jit_eqi_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_eqi_f", "u", "v", "w"])),
 ("noderef", "jit_eqr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_eqr", "u", "v", "w"])),
 ("noderef", "jit_eqr_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_eqr_d", "u", "v", "w"])),
 ("noderef", "jit_eqr_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_eqr_f", "u", "v", "w"])),
 ("noderef", "jit_extr_c", [("_jit", "state"), ("u", "gpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_extr_c", "u", "v"])),
 ("noderef", "jit_extr_d", [("_jit", "state"), ("u", "fpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_extr_d", "u", "v"])),
 ("noderef", "jit_extr_d_f", [("_jit", "state"), ("u", "fpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_extr_d_f", "u", "v"])),
 ("noderef", "jit_extr_f", [("_jit", "state"), ("u", "fpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_extr_f", "u", "v"])),
 ("noderef", "jit_extr_f_d", [("_jit", "state"), ("u", "fpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_extr_f_d", "u", "v"])),
 ("noderef", "jit_extr_s", [("_jit", "state"), ("u", "gpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_extr_s", "u", "v"])),
 ("noderef", "jit_extr_uc", [("_jit", "state"), ("u", "gpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_extr_uc", "u", "v"])),
 ("noderef", "jit_extr_us", [("_jit", "state"), ("u", "gpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_extr_us", "u", "v"])),
 ("noderef", "jit_gei",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_gei", "u", "v", "w"])),
 ("noderef", "jit_gei_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_gei_d", "u", "v", "w"])),
 ("noderef", "jit_gei_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_gei_f", "u", "v", "w"])),
 ("noderef", "jit_gei_u",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_gei_u", "u", "v", "w"])),
 ("noderef", "jit_ger",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ger", "u", "v", "w"])),
 ("noderef", "jit_ger_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ger_d", "u", "v", "w"])),
 ("noderef", "jit_ger_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ger_f", "u", "v", "w"])),
 ("noderef", "jit_ger_u",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ger_u", "u", "v", "w"])),
 ("noderef", "jit_gti",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_gti", "u", "v", "w"])),
 ("noderef", "jit_gti_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_gti_d", "u", "v", "w"])),
 ("noderef", "jit_gti_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_gti_f", "u", "v", "w"])),
 ("noderef", "jit_gti_u",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_gti_u", "u", "v", "w"])),
 ("noderef", "jit_gtr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_gtr", "u", "v", "w"])),
 ("noderef", "jit_gtr_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_gtr_d", "u", "v", "w"])),
 ("noderef", "jit_gtr_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_gtr_f", "u", "v", "w"])),
 ("noderef", "jit_gtr_u",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_gtr_u", "u", "v", "w"])),
 ("noderef", "jit_htonr", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_htonr", "u", "v"])),
 ("noderef", "jit_jmpi", [("_jit", "state")],
  ("jit_new_node_p", ["_jit", "jit_code_jmpi", "NULL"])),
 ("noderef", "jit_jmpr", [("_jit", "state"), ("u", "gpr")],
  ("jit_new_node_w", ["_jit", "jit_code_jmpr", "u"])),
 ("noderef", "jit_ldi_c", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_wp", ["_jit", "jit_code_ldi_c", "u", "v"])),
 ("noderef", "jit_ldi_d", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_wp", ["_jit", "jit_code_ldi_d", "u", "v"])),
 ("noderef", "jit_ldi_f", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_wp", ["_jit", "jit_code_ldi_f", "u", "v"])),
 ("noderef", "jit_ldi_i", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_wp", ["_jit", "jit_code_ldi_i", "u", "v"])),
 ("noderef", "jit_ldi_s", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_wp", ["_jit", "jit_code_ldi_s", "u", "v"])),
 ("noderef", "jit_ldi_uc", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_wp", ["_jit", "jit_code_ldi_uc", "u", "v"])),
 ("noderef", "jit_ldi_us", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_wp", ["_jit", "jit_code_ldi_us", "u", "v"])),
 ("noderef", "jit_ldr_c", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_ldr_c", "u", "v"])),
 ("noderef", "jit_ldr_d", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_ldr_d", "u", "v"])),
 ("noderef", "jit_ldr_f", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_ldr_f", "u", "v"])),
 ("noderef", "jit_ldr_i", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_ldr_i", "u", "v"])),
 ("noderef", "jit_ldr_s", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_ldr_s", "u", "v"])),
 ("noderef", "jit_ldr_uc", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_ldr_uc", "u", "v"])),
 ("noderef", "jit_ldr_us", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_ldr_us", "u", "v"])),
 ("noderef", "jit_ldxi_c",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxi_c", "u", "v", "w"])),
 ("noderef", "jit_ldxi_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxi_d", "u", "v", "w"])),
 ("noderef", "jit_ldxi_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxi_f", "u", "v", "w"])),
 ("noderef", "jit_ldxi_i",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxi_i", "u", "v", "w"])),
 ("noderef", "jit_ldxi_s",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxi_s", "u", "v", "w"])),
 ("noderef", "jit_ldxi_uc",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxi_uc", "u", "v", "w"])),
 ("noderef", "jit_ldxi_us",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxi_us", "u", "v", "w"])),
 ("noderef", "jit_ldxr_c",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxr_c", "u", "v", "w"])),
 ("noderef", "jit_ldxr_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxr_d", "u", "v", "w"])),
 ("noderef", "jit_ldxr_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxr_f", "u", "v", "w"])),
 ("noderef", "jit_ldxr_i",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxr_i", "u", "v", "w"])),
 ("noderef", "jit_ldxr_s",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxr_s", "u", "v", "w"])),
 ("noderef", "jit_ldxr_uc",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxr_uc", "u", "v", "w"])),
 ("noderef", "jit_ldxr_us",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxr_us", "u", "v", "w"])),
 ("noderef", "jit_lei",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_lei", "u", "v", "w"])),
 ("noderef", "jit_lei_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_lei_d", "u", "v", "w"])),
 ("noderef", "jit_lei_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_lei_f", "u", "v", "w"])),
 ("noderef", "jit_lei_u",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_lei_u", "u", "v", "w"])),
 ("noderef", "jit_ler",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ler", "u", "v", "w"])),
 ("noderef", "jit_ler_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ler_d", "u", "v", "w"])),
 ("noderef", "jit_ler_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ler_f", "u", "v", "w"])),
 ("noderef", "jit_ler_u",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ler_u", "u", "v", "w"])),
 ("noderef", "jit_live", [("_jit", "state"), ("u", "gpr")],
  ("jit_new_node_w", ["_jit", "jit_code_live", "u"])),
 ("noderef", "jit_lshi",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_lshi", "u", "v", "w"])),
 ("noderef", "jit_lshr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_lshr", "u", "v", "w"])),
 ("noderef", "jit_ltgti_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_ltgti_d", "u", "v", "w"])),
 ("noderef", "jit_ltgti_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_ltgti_f", "u", "v", "w"])),
 ("noderef", "jit_ltgtr_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ltgtr_d", "u", "v", "w"])),
 ("noderef", "jit_ltgtr_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ltgtr_f", "u", "v", "w"])),
 ("noderef", "jit_lti",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_lti", "u", "v", "w"])),
 ("noderef", "jit_lti_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_lti_d", "u", "v", "w"])),
 ("noderef", "jit_lti_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_lti_f", "u", "v", "w"])),
 ("noderef", "jit_lti_u",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_lti_u", "u", "v", "w"])),
 ("noderef", "jit_ltr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ltr", "u", "v", "w"])),
 ("noderef", "jit_ltr_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ltr_d", "u", "v", "w"])),
 ("noderef", "jit_ltr_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ltr_f", "u", "v", "w"])),
 ("noderef", "jit_ltr_u",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ltr_u", "u", "v", "w"])),
 ("noderef", "jit_movi", [("_jit", "state"), ("u", "gpr"), ("v", "word")],
  ("jit_new_node_ww", ["_jit", "jit_code_movi", "u", "v"])),
 ("noderef", "jit_movi_d", [("_jit", "state"), ("u", "fpr"), ("v", "double")],
  ("jit_new_node_wd", ["_jit", "jit_code_movi_d", "u", "v"])),
 ("noderef", "jit_movi_d_w",
  [("_jit", "state"), ("u", "gpr"), ("v", "double")],
  ("jit_new_node_wd", ["_jit", "jit_code_movi_d_w", "u", "v"])),
 ("noderef", "jit_movi_d_ww",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_movi_d_ww", "u", "v", "w"])),
 ("noderef", "jit_movi_f", [("_jit", "state"), ("u", "fpr"), ("v", "float")],
  ("jit_new_node_wf", ["_jit", "jit_code_movi_f", "u", "v"])),
 ("noderef", "jit_movi_f_w", [("_jit", "state"), ("u", "gpr"), ("v", "float")],
  ("jit_new_node_wf", ["_jit", "jit_code_movi_f_w", "u", "v"])),
 ("noderef", "jit_movr", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_movr", "u", "v"])),
 ("noderef", "jit_movr_d", [("_jit", "state"), ("u", "fpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_movr_d", "u", "v"])),
 ("noderef", "jit_movr_d_w", [("_jit", "state"), ("u", "gpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_movr_d_w", "u", "v"])),
 ("noderef", "jit_movr_d_ww",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_movr_d_ww", "u", "v", "w"])),
 ("noderef", "jit_movr_f", [("_jit", "state"), ("u", "fpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_movr_f", "u", "v"])),
 ("noderef", "jit_movr_f_w", [("_jit", "state"), ("u", "gpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_movr_f_w", "u", "v"])),
 ("noderef", "jit_movr_w_d", [("_jit", "state"), ("u", "fpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_movr_w_d", "u", "v"])),
 ("noderef", "jit_movr_w_f", [("_jit", "state"), ("u", "fpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_movr_w_f", "u", "v"])),
 ("noderef", "jit_movr_ww_d",
  [("_jit", "state"), ("u", "fpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_movr_ww_d", "u", "v", "w"])),
 ("noderef", "jit_muli",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_muli", "u", "v", "w"])),
 ("noderef", "jit_muli_d",
  [("_jit", "state"), ("u", "fpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_muli_d", "u", "v", "w"])),
 ("noderef", "jit_muli_f",
  [("_jit", "state"), ("u", "fpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_muli_f", "u", "v", "w"])),
 ("noderef", "jit_mulr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_mulr", "u", "v", "w"])),
 ("noderef", "jit_mulr_d",
  [("_jit", "state"), ("u", "fpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_mulr_d", "u", "v", "w"])),
 ("noderef", "jit_mulr_f",
  [("_jit", "state"), ("u", "fpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_mulr_f", "u", "v", "w"])),
 ("noderef", "jit_negr", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_negr", "u", "v"])),
 ("noderef", "jit_negr_d", [("_jit", "state"), ("u", "fpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_negr_d", "u", "v"])),
 ("noderef", "jit_negr_f", [("_jit", "state"), ("u", "fpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_negr_f", "u", "v"])),
 ("noderef", "jit_nei",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_nei", "u", "v", "w"])),
 ("noderef", "jit_nei_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_nei_d", "u", "v", "w"])),
 ("noderef", "jit_nei_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_nei_f", "u", "v", "w"])),
 ("noderef", "jit_ner",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ner", "u", "v", "w"])),
 ("noderef", "jit_ner_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ner_d", "u", "v", "w"])),
 ("noderef", "jit_ner_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ner_f", "u", "v", "w"])),
 ("noderef", "jit_ntohr", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_htonr", "u", "v"])),
 ("noderef", "jit_ordi_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_ordi_d", "u", "v", "w"])),
 ("noderef", "jit_ordi_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_ordi_f", "u", "v", "w"])),
 ("noderef", "jit_ordr_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ordr_d", "u", "v", "w"])),
 ("noderef", "jit_ordr_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ordr_f", "u", "v", "w"])),
 ("noderef", "jit_ori",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_ori", "u", "v", "w"])),
 ("noderef", "jit_orr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_orr", "u", "v", "w"])),
 ("noderef", "jit_qdivi",
  [("_jit", "state"), ("l", "gpr"), ("h", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_qww", ["_jit", "jit_code_qdivi", "l", "h", "v", "w"])),
 ("noderef", "jit_qdivi_u",
  [("_jit", "state"), ("l", "gpr"), ("h", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_qww", ["_jit", "jit_code_qdivi_u", "l", "h", "v", "w"])),
 ("noderef", "jit_qdivr",
  [("_jit", "state"), ("l", "gpr"), ("h", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_qww", ["_jit", "jit_code_qdivr", "l", "h", "v", "w"])),
 ("noderef", "jit_qdivr_u",
  [("_jit", "state"), ("l", "gpr"), ("h", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_qww", ["_jit", "jit_code_qdivr_u", "l", "h", "v", "w"])),
 ("noderef", "jit_qmuli",
  [("_jit", "state"), ("l", "gpr"), ("h", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_qww", ["_jit", "jit_code_qmuli", "l", "h", "v", "w"])),
 ("noderef", "jit_qmuli_u",
  [("_jit", "state"), ("l", "gpr"), ("h", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_qww", ["_jit", "jit_code_qmuli_u", "l", "h", "v", "w"])),
 ("noderef", "jit_qmulr",
  [("_jit", "state"), ("l", "gpr"), ("h", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_qww", ["_jit", "jit_code_qmulr", "l", "h", "v", "w"])),
 ("noderef", "jit_qmulr_u",
  [("_jit", "state"), ("l", "gpr"), ("h", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_qww", ["_jit", "jit_code_qmulr_u", "l", "h", "v", "w"])),
 ("noderef", "jit_remi",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_remi", "u", "v", "w"])),
 ("noderef", "jit_remi_u",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_remi_u", "u", "v", "w"])),
 ("noderef", "jit_remr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_remr", "u", "v", "w"])),
 ("noderef", "jit_remr_u",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_remr_u", "u", "v", "w"])),
 ("noderef", "jit_rshi",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_rshi", "u", "v", "w"])),
 ("noderef", "jit_rshi_u",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_rshi_u", "u", "v", "w"])),
 ("noderef", "jit_rshr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_rshr", "u", "v", "w"])),
 ("noderef", "jit_rshr_u",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_rshr_u", "u", "v", "w"])),
 ("noderef", "jit_sqrtr_d", [("_jit", "state"), ("u", "fpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_sqrtr_d", "u", "v"])),
 ("noderef", "jit_sqrtr_f", [("_jit", "state"), ("u", "fpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_sqrtr_f", "u", "v"])),
 ("noderef", "jit_sti_c", [("_jit", "state"), ("u", "pointer"), ("v", "gpr")],
  ("jit_new_node_pw", ["_jit", "jit_code_sti_c", "u", "v"])),
 ("noderef", "jit_sti_d", [("_jit", "state"), ("u", "pointer"), ("v", "gpr")],
  ("jit_new_node_pw", ["_jit", "jit_code_sti_d", "u", "v"])),
 ("noderef", "jit_sti_f", [("_jit", "state"), ("u", "pointer"), ("v", "gpr")],
  ("jit_new_node_pw", ["_jit", "jit_code_sti_f", "u", "v"])),
 ("noderef", "jit_sti_i", [("_jit", "state"), ("u", "pointer"), ("v", "gpr")],
  ("jit_new_node_pw", ["_jit", "jit_code_sti_i", "u", "v"])),
 ("noderef", "jit_sti_s", [("_jit", "state"), ("u", "pointer"), ("v", "gpr")],
  ("jit_new_node_pw", ["_jit", "jit_code_sti_s", "u", "v"])),
 ("noderef", "jit_str_c", [("_jit", "state"), ("u", "pointer"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_str_c", "u", "v"])),
 ("noderef", "jit_str_d", [("_jit", "state"), ("u", "pointer"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_str_d", "u", "v"])),
 ("noderef", "jit_str_f", [("_jit", "state"), ("u", "pointer"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_str_f", "u", "v"])),
 ("noderef", "jit_str_i", [("_jit", "state"), ("u", "pointer"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_str_i", "u", "v"])),
 ("noderef", "jit_str_s", [("_jit", "state"), ("u", "pointer"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_str_s", "u", "v"])),
 ("noderef", "jit_stxi_c",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxi_c", "u", "v", "w"])),
 ("noderef", "jit_stxi_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxi_d", "u", "v", "w"])),
 ("noderef", "jit_stxi_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxi_f", "u", "v", "w"])),
 ("noderef", "jit_stxi_i",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxi_i", "u", "v", "w"])),
 ("noderef", "jit_stxi_s",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxi_s", "u", "v", "w"])),
 ("noderef", "jit_stxr_c",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxr_c", "u", "v", "w"])),
 ("noderef", "jit_stxr_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxr_d", "u", "v", "w"])),
 ("noderef", "jit_stxr_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxr_f", "u", "v", "w"])),
 ("noderef", "jit_stxr_i",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxr_i", "u", "v", "w"])),
 ("noderef", "jit_stxr_s",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxr_s", "u", "v", "w"])),
 ("noderef", "jit_subci",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_subci", "u", "v", "w"])),
 ("noderef", "jit_subcr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_subcr", "u", "v", "w"])),
 ("noderef", "jit_subi",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_subi", "u", "v", "w"])),
 ("noderef", "jit_subi_d",
  [("_jit", "state"), ("u", "fpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_subi_d", "u", "v", "w"])),
 ("noderef", "jit_subi_f",
  [("_jit", "state"), ("u", "fpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_subi_f", "u", "v", "w"])),
 ("noderef", "jit_subr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_subr", "u", "v", "w"])),
 ("noderef", "jit_subr_d",
  [("_jit", "state"), ("u", "fpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_subr_d", "u", "v", "w"])),
 ("noderef", "jit_subr_f",
  [("_jit", "state"), ("u", "fpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_subr_f", "u", "v", "w"])),
 ("noderef", "jit_subxi",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_subxi", "u", "v", "w"])),
 ("noderef", "jit_subxr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_subxr", "u", "v", "w"])),
 ("noderef", "jit_truncr_d_i", [("_jit", "state"), ("u", "gpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_truncr_d_i", "u", "v"])),
 ("noderef", "jit_truncr_f_i", [("_jit", "state"), ("u", "gpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_truncr_f_i", "u", "v"])),
 ("noderef", "jit_uneqi_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_uneqi_d", "u", "v", "w"])),
 ("noderef", "jit_uneqi_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_uneqi_f", "u", "v", "w"])),
 ("noderef", "jit_uneqr_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_uneqr_d", "u", "v", "w"])),
 ("noderef", "jit_uneqr_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_uneqr_f", "u", "v", "w"])),
 ("noderef", "jit_ungei_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_ungei_d", "u", "v", "w"])),
 ("noderef", "jit_ungei_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_ungei_f", "u", "v", "w"])),
 ("noderef", "jit_unger_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_unger_d", "u", "v", "w"])),
 ("noderef", "jit_unger_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_unger_f", "u", "v", "w"])),
 ("noderef", "jit_ungti_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_ungti_d", "u", "v", "w"])),
 ("noderef", "jit_ungti_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_ungti_f", "u", "v", "w"])),
 ("noderef", "jit_ungtr_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ungtr_d", "u", "v", "w"])),
 ("noderef", "jit_ungtr_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ungtr_f", "u", "v", "w"])),
 ("noderef", "jit_unlei_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_unlei_d", "u", "v", "w"])),
 ("noderef", "jit_unlei_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_unlei_f", "u", "v", "w"])),
 ("noderef", "jit_unler_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_unler_d", "u", "v", "w"])),
 ("noderef", "jit_unler_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_unler_f", "u", "v", "w"])),
 ("noderef", "jit_unlti_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_unlti_d", "u", "v", "w"])),
 ("noderef", "jit_unlti_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_unlti_f", "u", "v", "w"])),
 ("noderef", "jit_unltr_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_unltr_d", "u", "v", "w"])),
 ("noderef", "jit_unltr_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_unltr_f", "u", "v", "w"])),
 ("noderef", "jit_unordi_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "double")],
  ("jit_new_node_wwd", ["_jit", "jit_code_unordi_d", "u", "v", "w"])),
 ("noderef", "jit_unordi_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "float")],
  ("jit_new_node_wwf", ["_jit", "jit_code_unordi_f", "u", "v", "w"])),
 ("noderef", "jit_unordr_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_unordr_d", "u", "v", "w"])),
 ("noderef", "jit_unordr_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "fpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_unordr_f", "u", "v", "w"])),
 ("noderef", "jit_xori",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_xori", "u", "v", "w"])),
 ("noderef", "jit_xorr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_xorr", "u", "v", "w"]))];

val mknodes =
    [("noderef", "jit_new_node_p",
      [("_jit", "state"), ("c", "code"), ("u", "pointer")],
      ("_jit_new_node_p", ["_jit", "c", "u"])),
     ("noderef", "jit_new_node_pw",
      [("_jit", "state"), ("c", "code"), ("u", "pointer"), ("v", "word")],
      ("_jit_new_node_pw", ["_jit", "c", "u", "v"])),
     ("noderef", "jit_new_node_pwd",
      [("_jit", "state"), ("c", "code"), ("u", "pointer"), ("v", "word"),
       ("w", "double")], ("_jit_new_node_pwd", ["_jit", "c", "u", "v", "w"])),
     ("noderef", "jit_new_node_pwf",
      [("_jit", "state"), ("c", "code"), ("u", "pointer"), ("v", "word"),
       ("w", "float")], ("_jit_new_node_pwf", ["_jit", "c", "u", "v", "w"])),
     ("noderef", "jit_new_node_pww",
      [("_jit", "state"), ("c", "code"), ("u", "pointer"), ("v", "word"),
       ("w", "word")], ("_jit_new_node_pww", ["_jit", "c", "u", "v", "w"])),
     ("noderef", "jit_new_node_qww",
      [("_jit", "state"), ("c", "code"), ("l", "word"), ("h", "word"),
       ("v", "word"), ("w", "word")],
      ("_jit_new_node_qww", ["_jit", "c", "l", "h", "v", "w"])),
     ("noderef", "jit_new_node_w",
      [("_jit", "state"), ("c", "code"), ("u", "word")],
      ("_jit_new_node_w", ["_jit", "c", "u"])),
     ("noderef", "jit_new_node_wd",
      [("_jit", "state"), ("c", "code"), ("u", "word"), ("v", "double")],
      ("_jit_new_node_wd", ["_jit", "c", "u", "v"])),
     ("noderef", "jit_new_node_wf",
      [("_jit", "state"), ("c", "code"), ("u", "word"), ("v", "float")],
      ("_jit_new_node_wf", ["_jit", "c", "u", "v"])),
     ("noderef", "jit_new_node_wp",
      [("_jit", "state"), ("c", "code"), ("u", "word"), ("v", "pointer")],
      ("_jit_new_node_wp", ["_jit", "c", "u", "v"])),
     ("noderef", "jit_new_node_ww",
      [("_jit", "state"), ("c", "code"), ("u", "word"), ("v", "word")],
      ("_jit_new_node_ww", ["_jit", "c", "u", "v"])),
     ("noderef", "jit_new_node_wwd",
      [("_jit", "state"), ("c", "code"), ("u", "word"), ("v", "word"),
       ("w", "double")], ("_jit_new_node_wwd", ["_jit", "c", "u", "v", "w"])),
     ("noderef", "jit_new_node_wwf",
      [("_jit", "state"), ("c", "code"), ("u", "word"), ("v", "word"),
       ("w", "float")], ("_jit_new_node_wwf", ["_jit", "c", "u", "v", "w"])),
     ("noderef", "jit_new_node_www",
      [("_jit", "state"), ("c", "code"), ("u", "word"), ("v", "word"),
       ("w", "word")], ("_jit_new_node_www", ["_jit", "c", "u", "v", "w"]))];

