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
val scm_make_bytevectorp = jitptr dlxh "scm_make_bytevector"
val scm_bytevector_pp = jitptr dlxh "scm_bytevector_p"
val scm_bytevector_eq_pp = jitptr dlxh "scm_bytevector_eq_p"
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

val scm_bytevector_s32_native_refp = jitptr dlxh "scm_bytevector_s32_native_ref"
val scm_bytevector_u32_native_refp = jitptr dlxh "scm_bytevector_u32_native_ref"
val scm_bytevector_s64_native_refp = jitptr dlxh "scm_bytevector_s64_native_ref"
val scm_bytevector_u64_native_refp = jitptr dlxh "scm_bytevector_u64_native_ref"

val scm_bytevector_s32_native_set_xp = jitptr dlxh "scm_bytevector_s32_native_set_x"
val scm_bytevector_u32_native_set_xp = jitptr dlxh "scm_bytevector_u32_native_set_x"
val scm_bytevector_s64_native_set_xp = jitptr dlxh "scm_bytevector_s64_native_set_x"
val scm_bytevector_u64_native_set_xp = jitptr dlxh "scm_bytevector_u64_native_set_x"

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

fun jit_fprolog jit_ =
   let open Jit
       val node = jit_note (jit_, NULLp, 0w0)
       val _ = jit_prolog(jit_)
   in node
   end

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
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0) (* V1 = Field(v,0) *)
      val _ = jit_ldxi (jit_, V2, V0, wsz * 1) (* V2 = Field(v,1) *)
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
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0) (* V1 = Field(v,0) *)
      val _ = jit_ldxi (jit_, V2, V0, wsz * 1) (* V2 = Field(v,1) *)
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
             cptr * cptr * cptr * cptr -> cptr
   end

fun jit_scm_three_args funp =
  let open Jit
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
      val _ = jit_finishi (jit_, funp)
      val _ = jit_retval (jit_, R1)
      val _ = jit_retr (jit_, R1)
      val fptr = jit_emit (jit_)
   in
      Ffi.app1 fptr : cptr * cptr * cptr -> cptr
   end

fun jit_scm_two_args funp =
  let open Jit
      val jit_ = Jit.jit_new_state ()
      val () = jit_prolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_prepare (jit_)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0) (* V1 = Field(v,0) *)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 1) (* V1 = Field(v,1) *)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_finishi (jit_, funp)
      val _ = jit_retval (jit_, R1)
      val _ = jit_retr (jit_, R1)
      val fptr = jit_emit (jit_)
   in
      Ffi.app1 fptr : cptr * cptr -> cptr
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
   val scm_make_bytevector = scm_binary (jit_binop scm_make_bytevectorp)
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

val scm_bytevector_u32_native_set_x = jit_scm_three_args scm_bytevector_u32_native_set_xp;
val scm_bytevector_u32_native_ref = jit_scm_two_args scm_bytevector_u32_native_refp;
val scm_bytevector_s32_native_set_x = jit_scm_three_args scm_bytevector_s32_native_set_xp;
val scm_bytevector_s32_native_ref = jit_scm_two_args scm_bytevector_s32_native_refp;

val scm_bytevector_u64_native_set_x = jit_scm_three_args scm_bytevector_u64_native_set_xp;
val scm_bytevector_u64_native_ref = jit_scm_two_args scm_bytevector_u64_native_refp;
val scm_bytevector_s64_native_set_x = jit_scm_three_args scm_bytevector_s64_native_set_xp;
val scm_bytevector_s64_native_ref = jit_scm_two_args scm_bytevector_s64_native_refp;

fun scm_mk_bytevector n = scm_make_bytevector (scm_from_ulong n,scm_from_ulong 0w0);

fun scm_long_bytevector scm =
   let val bv = scm_mk_bytevector (Word.fromInt(Jit.WORDSIZE div 8))
   in if Jit.WORDSIZE = 4 
      then scm_bytevector_s32_native_set_x (bv, scm_from_ulong 0w0, scm)
      else scm_bytevector_s64_native_set_x (bv, scm_from_ulong 0w0, scm);
      bv
   end

fun scm_ulong_bytevector scm =
   let val bv = scm_mk_bytevector (Word.fromInt(Jit.WORDSIZE div 8))
   in if Jit.WORDSIZE = 4 
      then scm_bytevector_u32_native_set_x (bv, scm_from_ulong 0w0, scm)
      else scm_bytevector_u64_native_set_x (bv, scm_from_ulong 0w0, scm);
      bv
   end

val bv = scm_mk_bytevector (Word.fromInt(Jit.WORDSIZE div 8));
val res' = scm_bytevector_s32_native_set_x (bv, scm_from_ulong 0w0, scm_from_long 0x2a);
val res'' = scm_bytevector_s32_native_ref (bv, scm_from_ulong 0w0);
val res''' = scm_to_ulong res'';

val scm_list = List.foldr scm_cons scm_nil;

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

fun scm_read s =
   let val scm_read_proc =    scm_pub_ref ["guile"] "read"
       val scm_callwis_proc = scm_pub_ref ["guile"] "call-with-input-string"
   in scm_call_n (scm_callwis_proc,#[scm_string s,scm_read_proc])
   end

val scm_readq = scm_read o qlToString

local
 (* Will these local <cptr> SCM objects be protected from GC by the
      Guile collector?  (The answer seems to be "no!")

      I need to understand how variable binding is
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
   fun sml_string_display scm =
     let val scmdisplayproc = scm_evalq `
                        (lambda (x)
		           (with-fluids ((%default-port-encoding "UTF-8"))
		              (call-with-output-string
                                (lambda (p)
                                  (display x p)))))`
     in sml_string (scm_call_n (scmdisplayproc,#[scm]))
     end
end

fun scm_write exp =
   let val scmwriteproc = scm_evalq ` 
                        (lambda (x)
		           (with-fluids ((%default-port-encoding "UTF-8"))
	 	              (call-with-output-string
                                (lambda (p)
                                  (write x p)))))`
   in  sml_string (scm_call_n (scmwriteproc,#[exp]))
   end

fun scm_qlToString l =
   let fun iter r [] = r
         | iter r ((QUOTE s)::fs) = iter (r^s) fs
         | iter r ((ANTIQUOTE s)::fs) = iter (r^(scm_write s)) fs
   in iter "" l
   end;

val scm_evalqq = scm_eval_string o scm_qlToString

val scm_readqq = scm_read o scm_qlToString

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
   let val mpz = IntInf.init2 (0x10,0x0);
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
   val _ = jit_ldxi (jit_, V2, V0, wsz * 1) (* V2 = Field(v,1)   *)
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

val infra = [("pointer", "jit_address", [("_jit", "state"), ("node", "noderef")],
  ("_jit_address", ["_jit", "node"])),
 ("int", "jit_allocai", [("_jit", "state"), ("u", "word")],
  ("_jit_allocai", ["_jit", "u"])),
 ("noderef", "jit_arg", [("_jit", "state")], ("_jit_arg", ["_jit"])),
 ("noderef", "jit_arg_d", [("_jit", "state")], ("_jit_arg_d", ["_jit"])),
 ("noderef", "jit_arg_f", [("_jit", "state")], ("_jit_arg_f", ["_jit"])),
 ("void", "jit_clear_state", [("_jit", "")], ("_jit_clear_state", ["_jit"])),
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
 ("noderef", "jit_new_node", [("_jit", "state"), ("c", "code")],
  ("_jit_new_node", ["_jit", "c"])),
 ("noderef", "jit_note", [("_jit", "state"), ("u", "pointer"), ("v", "word")],
  ("_jit_note", ["_jit", "u", "v"])),
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

val mknodes = [("noderef", "jit_new_node_p",
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

val insns = [("noderef", "jit_absr_d", [("_jit", "state"), ("u", "fpr"), ("v", "fpr")],
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
 ("noderef", "jit_ldi_c", [("_jit", "state"), ("u", "gpr"), ("v", "pointer")],
  ("jit_new_node_wp", ["_jit", "jit_code_ldi_c", "u", "v"])),
 ("noderef", "jit_ldi_d", [("_jit", "state"), ("u", "fpr"), ("v", "pointer")],
  ("jit_new_node_wp", ["_jit", "jit_code_ldi_d", "u", "v"])),
 ("noderef", "jit_ldi_f", [("_jit", "state"), ("u", "fpr"), ("v", "pointer")],
  ("jit_new_node_wp", ["_jit", "jit_code_ldi_f", "u", "v"])),
 ("noderef", "jit_ldi_i", [("_jit", "state"), ("u", "gpr"), ("v", "pointer")],
  ("jit_new_node_wp", ["_jit", "jit_code_ldi_i", "u", "v"])),
 ("noderef", "jit_ldi_s", [("_jit", "state"), ("u", "gpr"), ("v", "pointer")],
  ("jit_new_node_wp", ["_jit", "jit_code_ldi_s", "u", "v"])),
 ("noderef", "jit_ldi_uc", [("_jit", "state"), ("u", "gpr"), ("v", "pointer")],
  ("jit_new_node_wp", ["_jit", "jit_code_ldi_uc", "u", "v"])),
 ("noderef", "jit_ldi_us", [("_jit", "state"), ("u", "gpr"), ("v", "pointer")],
  ("jit_new_node_wp", ["_jit", "jit_code_ldi_us", "u", "v"])),
 ("noderef", "jit_ldr_c", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_ldr_c", "u", "v"])),
 ("noderef", "jit_ldr_d", [("_jit", "state"), ("u", "fpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_ldr_d", "u", "v"])),
 ("noderef", "jit_ldr_f", [("_jit", "state"), ("u", "fpr"), ("v", "gpr")],
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
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "pointer")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxi_c", "u", "v", "w"])),
 ("noderef", "jit_ldxi_d",
  [("_jit", "state"), ("u", "fpr"), ("v", "gpr"), ("w", "pointer")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxi_d", "u", "v", "w"])),
 ("noderef", "jit_ldxi_f",
  [("_jit", "state"), ("u", "fpr"), ("v", "gpr"), ("w", "pointer")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxi_f", "u", "v", "w"])),
 ("noderef", "jit_ldxi_i",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "pointer")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxi_i", "u", "v", "w"])),
 ("noderef", "jit_ldxi_s",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "pointer")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxi_s", "u", "v", "w"])),
 ("noderef", "jit_ldxi_uc",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "pointer")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxi_uc", "u", "v", "w"])),
 ("noderef", "jit_ldxi_us",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "pointer")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxi_us", "u", "v", "w"])),
 ("noderef", "jit_ldxr_c",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxr_c", "u", "v", "w"])),
 ("noderef", "jit_ldxr_d",
  [("_jit", "state"), ("u", "fpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_ldxr_d", "u", "v", "w"])),
 ("noderef", "jit_ldxr_f",
  [("_jit", "state"), ("u", "fpr"), ("v", "gpr"), ("w", "gpr")],
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
 ("noderef", "jit_sti_d", [("_jit", "state"), ("u", "pointer"), ("v", "fpr")],
  ("jit_new_node_pw", ["_jit", "jit_code_sti_d", "u", "v"])),
 ("noderef", "jit_sti_f", [("_jit", "state"), ("u", "pointer"), ("v", "fpr")],
  ("jit_new_node_pw", ["_jit", "jit_code_sti_f", "u", "v"])),
 ("noderef", "jit_sti_i", [("_jit", "state"), ("u", "pointer"), ("v", "gpr")],
  ("jit_new_node_pw", ["_jit", "jit_code_sti_i", "u", "v"])),
 ("noderef", "jit_sti_s", [("_jit", "state"), ("u", "pointer"), ("v", "gpr")],
  ("jit_new_node_pw", ["_jit", "jit_code_sti_s", "u", "v"])),
 ("noderef", "jit_str_c", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_str_c", "u", "v"])),
 ("noderef", "jit_str_d", [("_jit", "state"), ("u", "gpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_str_d", "u", "v"])),
 ("noderef", "jit_str_f", [("_jit", "state"), ("u", "gpr"), ("v", "fpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_str_f", "u", "v"])),
 ("noderef", "jit_str_i", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_str_i", "u", "v"])),
 ("noderef", "jit_str_s", [("_jit", "state"), ("u", "gpr"), ("v", "gpr")],
  ("jit_new_node_ww", ["_jit", "jit_code_str_s", "u", "v"])),
 ("noderef", "jit_stxi_c",
  [("_jit", "state"), ("u", "pointer"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxi_c", "u", "v", "w"])),
 ("noderef", "jit_stxi_d",
  [("_jit", "state"), ("u", "pointer"), ("v", "gpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxi_d", "u", "v", "w"])),
 ("noderef", "jit_stxi_f",
  [("_jit", "state"), ("u", "pointer"), ("v", "gpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxi_f", "u", "v", "w"])),
 ("noderef", "jit_stxi_i",
  [("_jit", "state"), ("u", "pointer"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxi_i", "u", "v", "w"])),
 ("noderef", "jit_stxi_s",
  [("_jit", "state"), ("u", "pointer"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxi_s", "u", "v", "w"])),
 ("noderef", "jit_stxr_c",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxr_c", "u", "v", "w"])),
 ("noderef", "jit_stxr_d",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "fpr")],
  ("jit_new_node_www", ["_jit", "jit_code_stxr_d", "u", "v", "w"])),
 ("noderef", "jit_stxr_f",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "fpr")],
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

val dataflow = [("jit_absr_d", [([1], [2])]), ("jit_absr_f", [([1], [2])]),
 ("jit_addci", [([1], [2])]), ("jit_addcr", [([1], [2, 3])]),
 ("jit_addi", [([1], [2])]), ("jit_addi_d", [([1], [2])]),
 ("jit_addi_f", [([1], [2])]), ("jit_addr", [([1], [2, 3])]),
 ("jit_addr_d", [([1], [2, 3])]), ("jit_addr_f", [([1], [2, 3])]),
 ("jit_addxi", [([1], [2])]), ("jit_addxr", [([1], [2, 3])]),
 ("jit_andi", [([1], [2])]), ("jit_andr", [([1], [2, 3])]),
 ("jit_beqi", [([], [1])]), ("jit_beqi_d", [([], [1])]),
 ("jit_beqi_f", [([], [1])]), ("jit_beqr", [([], [1, 2])]),
 ("jit_beqr_d", [([], [1, 2])]), ("jit_beqr_f", [([], [1, 2])]),
 ("jit_bgei", [([], [1])]), ("jit_bgei_d", [([], [1])]),
 ("jit_bgei_f", [([], [1])]), ("jit_bgei_u", [([], [1])]),
 ("jit_bger", [([], [1, 2])]), ("jit_bger_d", [([], [1, 2])]),
 ("jit_bger_f", [([], [1, 2])]), ("jit_bger_u", [([], [1, 2])]),
 ("jit_bgti", [([], [1])]), ("jit_bgti_d", [([], [1])]),
 ("jit_bgti_f", [([], [1])]), ("jit_bgti_u", [([], [1])]),
 ("jit_bgtr", [([], [1, 2])]), ("jit_bgtr_d", [([], [1, 2])]),
 ("jit_bgtr_f", [([], [1, 2])]), ("jit_bgtr_u", [([], [1, 2])]),
 ("jit_blei", [([], [1])]), ("jit_blei_d", [([], [1])]),
 ("jit_blei_f", [([], [1])]), ("jit_blei_u", [([], [1])]),
 ("jit_bler", [([], [1, 2])]), ("jit_bler_d", [([], [1, 2])]),
 ("jit_bler_f", [([], [1, 2])]), ("jit_bler_u", [([], [1, 2])]),
 ("jit_bltgti_d", [([], [1])]), ("jit_bltgti_f", [([], [1])]),
 ("jit_bltgtr_d", [([], [1, 2])]), ("jit_bltgtr_f", [([], [1, 2])]),
 ("jit_blti", [([], [1])]), ("jit_blti_d", [([], [1])]),
 ("jit_blti_f", [([], [1])]), ("jit_blti_u", [([], [1])]),
 ("jit_bltr", [([], [1, 2])]), ("jit_bltr_d", [([], [1, 2])]),
 ("jit_bltr_f", [([], [1, 2])]), ("jit_bltr_u", [([], [1, 2])]),
 ("jit_bmci", [([1], [])]), ("jit_bmcr", [([1], [2])]),
 ("jit_bmsi", [([1], [])]), ("jit_bmsr", [([1], [2])]),
 ("jit_bnei", [([], [1])]), ("jit_bnei_d", [([], [1])]),
 ("jit_bnei_f", [([], [1])]), ("jit_bner", [([], [1, 2])]),
 ("jit_bner_d", [([], [1, 2])]), ("jit_bner_f", [([], [1, 2])]),
 ("jit_boaddi", [([1], [])]), ("jit_boaddi_u", [([1], [])]),
 ("jit_boaddr", [([1], [2])]), ("jit_boaddr_u", [([1], [2])]),
 ("jit_bordi_d", [([1], []), ([], [1])]),
 ("jit_bordi_f", [([1], []), ([], [1])]),
 ("jit_bordr_d", [([1], [2]), ([], [1, 2])]),
 ("jit_bordr_f", [([1], [2]), ([], [1, 2])]), ("jit_bosubi", [([1], [])]),
 ("jit_bosubi_u", [([1], [])]), ("jit_bosubr", [([1], [2])]),
 ("jit_bosubr_u", [([1], [2])]), ("jit_buneqi_d", [([], [1])]),
 ("jit_buneqi_f", [([], [1])]), ("jit_buneqr_d", [([], [1, 2])]),
 ("jit_buneqr_f", [([], [1, 2])]), ("jit_bungei_d", [([], [1])]),
 ("jit_bungei_f", [([], [1])]), ("jit_bunger_d", [([], [1, 2])]),
 ("jit_bunger_f", [([], [1, 2])]), ("jit_bungti_d", [([], [1])]),
 ("jit_bungti_f", [([], [1])]), ("jit_bungtr_d", [([], [1, 2])]),
 ("jit_bungtr_f", [([], [1, 2])]), ("jit_bunlei_d", [([], [1])]),
 ("jit_bunlei_f", [([], [1])]), ("jit_bunler_d", [([], [1, 2])]),
 ("jit_bunler_f", [([], [1, 2])]), ("jit_bunlti_d", [([], [1])]),
 ("jit_bunlti_f", [([], [1])]), ("jit_bunltr_d", [([], [1, 2])]),
 ("jit_bunltr_f", [([], [1, 2])]), ("jit_bunordi_d", [([1], []), ([], [1])]),
 ("jit_bunordi_f", [([1], []), ([], [1])]),
 ("jit_bunordr_d", [([1], [2]), ([], [1, 2])]),
 ("jit_bunordr_f", [([1], [2]), ([], [1, 2])]), ("jit_bxaddi", [([1], [])]),
 ("jit_bxaddi_u", [([1], [])]), ("jit_bxaddr", [([1], [2])]),
 ("jit_bxaddr_u", [([1], [2])]), ("jit_bxsubi", [([1], [])]),
 ("jit_bxsubi_u", [([1], [])]), ("jit_bxsubr", [([1], [2])]),
 ("jit_bxsubr_u", [([1], [2])]), ("jit_calli", [([], [])]),
 ("jit_callr", [([], [1])]), ("jit_comr", [([1], [2])]),
 ("jit_divi", [([1], [2])]), ("jit_divi_d", [([1], [2])]),
 ("jit_divi_f", [([1], [2])]), ("jit_divi_u", [([1], [2])]),
 ("jit_divr", [([1], [2, 3])]), ("jit_divr_d", [([1], [2, 3])]),
 ("jit_divr_f", [([1], [2, 3])]), ("jit_divr_u", [([1], [2, 3])]),
 ("jit_eqi", [([1], [2])]), ("jit_eqi_d", [([1], [2])]),
 ("jit_eqi_f", [([1], [2])]), ("jit_eqr", [([1], [2, 3])]),
 ("jit_eqr_d", [([1], [2, 3])]), ("jit_eqr_f", [([1], [2, 3])]),
 ("jit_extr_c", [([1], [2])]), ("jit_extr_d", [([1], [2])]),
 ("jit_extr_d_f", [([1], [2])]), ("jit_extr_f", [([1], [2])]),
 ("jit_extr_f_d", [([1], [2])]), ("jit_extr_s", [([1], [2])]),
 ("jit_extr_uc", [([1], [2])]), ("jit_extr_us", [([1], [2])]),
 ("jit_gei", [([1], [2])]), ("jit_gei_d", [([1], [2])]),
 ("jit_gei_f", [([1], [2])]), ("jit_gei_u", [([1], [2])]),
 ("jit_ger", [([1], [2, 3])]), ("jit_ger_d", [([1], [2, 3])]),
 ("jit_ger_f", [([1], [2, 3])]), ("jit_ger_u", [([1], [2, 3])]),
 ("jit_gti", [([1], [2])]), ("jit_gti_d", [([1], [2])]),
 ("jit_gti_f", [([1], [2])]), ("jit_gti_u", [([1], [2])]),
 ("jit_gtr", [([1], [2, 3])]), ("jit_gtr_d", [([1], [2, 3])]),
 ("jit_gtr_f", [([1], [2, 3])]), ("jit_gtr_u", [([1], [2, 3])]),
 ("jit_htonr", [([1], [2])]), ("jit_jmpi", []), ("jit_jmpr", [([], [1])]),
 ("jit_ldi_c", [([1], [2, 3])]), ("jit_ldi_d", [([1], [2, 3])]),
 ("jit_ldi_f", [([1], [2, 3])]), ("jit_ldi_i", [([1], [2, 3])]),
 ("jit_ldi_s", [([1], [2, 3])]), ("jit_ldi_uc", [([1], [2, 3])]),
 ("jit_ldi_us", [([1], [2, 3])]), ("jit_ldr_c", [([1], [2])]),
 ("jit_ldr_d", [([1], [2])]), ("jit_ldr_f", [([1], [2])]),
 ("jit_ldr_i", [([1], [2])]), ("jit_ldr_s", [([1], [2])]),
 ("jit_ldr_uc", [([1], [2])]), ("jit_ldr_us", [([1], [2])]),
 ("jit_ldxi_c", [([1], [2, 3])]), ("jit_ldxi_d", [([1], [2, 3])]),
 ("jit_ldxi_f", [([1], [2, 3])]), ("jit_ldxi_i", [([1], [2, 3])]),
 ("jit_ldxi_s", [([1], [2, 3])]), ("jit_ldxi_uc", [([1], [2, 3])]),
 ("jit_ldxi_us", [([1], [2, 3])]), ("jit_ldxr_c", [([1], [2])]),
 ("jit_ldxr_d", [([1], [2])]), ("jit_ldxr_f", [([1], [2])]),
 ("jit_ldxr_i", [([1], [2])]), ("jit_ldxr_s", [([1], [2])]),
 ("jit_ldxr_uc", [([1], [2])]), ("jit_ldxr_us", [([1], [2])]),
 ("jit_lei", [([1], [2])]), ("jit_lei_d", [([1], [2])]),
 ("jit_lei_f", [([1], [2])]), ("jit_lei_u", [([1], [2])]),
 ("jit_ler", [([1], [2, 3])]), ("jit_ler_d", [([1], [2, 3])]),
 ("jit_ler_f", [([1], [2, 3])]), ("jit_ler_u", [([1], [2, 3])]),
 ("jit_live", [([], [1])]), ("jit_lshi", [([1], [2])]),
 ("jit_lshr", [([1], [2, 3])]), ("jit_ltgti_d", [([1], [2])]),
 ("jit_ltgti_f", [([1], [2])]), ("jit_ltgtr_d", [([1], [2, 3])]),
 ("jit_ltgtr_f", [([1], [2, 3])]), ("jit_lti", [([1], [2])]),
 ("jit_lti_d", [([1], [2])]), ("jit_lti_f", [([1], [2])]),
 ("jit_lti_u", [([1], [2])]), ("jit_ltr", [([1], [2, 3])]),
 ("jit_ltr_d", [([1], [2, 3])]), ("jit_ltr_f", [([1], [2, 3])]),
 ("jit_ltr_u", [([1], [2, 3])]), ("jit_movi", [([1], [])]),
 ("jit_movi_d", [([1], [])]), ("jit_movi_d_w", [([1], [])]),
 ("jit_movi_d_ww", [([1, 2], [])]), ("jit_movi_f", [([1], [])]),
 ("jit_movi_f_w", [([1], [])]), ("jit_movr", [([1], [2])]),
 ("jit_movr_d", [([1], [2])]), ("jit_movr_d_w", [([1], [2])]),
 ("jit_movr_d_ww", [([1, 2], [3])]), ("jit_movr_f", [([1], [2])]),
 ("jit_movr_f_w", [([1], [2])]), ("jit_movr_w_d", [([1], [2])]),
 ("jit_movr_w_f", [([1], [2])]), ("jit_movr_ww_d", [([1], [2, 3])]),
 ("jit_muli", [([1], [2])]), ("jit_muli_d", [([1], [2])]),
 ("jit_muli_f", [([1], [2])]), ("jit_mulr", [([1], [2, 3])]),
 ("jit_mulr_d", [([1], [2, 3])]), ("jit_mulr_f", [([1], [2, 3])]),
 ("jit_negr", [([1], [2])]), ("jit_negr_d", [([1], [2])]),
 ("jit_negr_f", [([1], [2])]), ("jit_nei", [([1], [2])]),
 ("jit_nei_d", [([1], [2])]), ("jit_nei_f", [([1], [2])]),
 ("jit_ner", [([1], [2, 3])]), ("jit_ner_d", [([1], [2, 3])]),
 ("jit_ner_f", [([1], [2, 3])]), ("jit_ntohr", [([1], [2])]),
 ("jit_ordi_d", [([1], [2])]), ("jit_ordi_f", [([1], [2])]),
 ("jit_ordr_d", [([1], [2, 3])]), ("jit_ordr_f", [([1], [2, 3])]),
 ("jit_ori", [([1], [2])]), ("jit_orr", [([1], [2, 3])]),
 ("jit_qdivi", [([1, 2], [3])]), ("jit_qdivi_u", [([1, 2], [3])]),
 ("jit_qdivr", [([1, 2], [3, 4])]), ("jit_qdivr_u", [([1, 2], [3, 4])]),
 ("jit_qmuli", [([1, 2], [3])]), ("jit_qmuli_u", [([1, 2], [3])]),
 ("jit_qmulr", [([1, 2], [3, 4])]), ("jit_qmulr_u", [([1, 2], [3, 4])]),
 ("jit_remi", [([1], [2])]), ("jit_remi_u", [([1], [2])]),
 ("jit_remr", [([1], [2, 3])]), ("jit_remr_u", [([1], [2, 3])]),
 ("jit_rshi", [([1], [2])]), ("jit_rshi_u", [([1], [2])]),
 ("jit_rshr", [([1], [2, 3])]), ("jit_rshr_u", [([1], [2, 3])]),
 ("jit_sqrtr_d", [([1], [2])]), ("jit_sqrtr_f", [([1], [2])]),
 ("jit_sti_c", [([], [1, 2])]), ("jit_sti_d", [([], [1, 2])]),
 ("jit_sti_f", [([], [1, 2])]), ("jit_sti_i", [([], [1, 2])]),
 ("jit_sti_s", [([], [1, 2])]), ("jit_str_c", [([], [1])]),
 ("jit_str_d", [([], [1])]), ("jit_str_f", [([], [1])]),
 ("jit_str_i", [([], [1])]), ("jit_str_s", [([], [1])]),
 ("jit_stxi_c", [([], [1, 2, 3])]), ("jit_stxi_d", [([], [1, 2, 3])]),
 ("jit_stxi_f", [([], [1, 2, 3])]), ("jit_stxi_i", [([], [1, 2, 3])]),
 ("jit_stxi_s", [([], [1, 2, 3])]), ("jit_stxr_c", [([], [1, 2])]),
 ("jit_stxr_d", [([], [1, 2])]), ("jit_stxr_f", [([], [1, 2])]),
 ("jit_stxr_i", [([], [1, 2])]), ("jit_stxr_s", [([], [1, 2])]),
 ("jit_subci", [([1], [2])]), ("jit_subcr", [([1], [2, 3])]),
 ("jit_subi", [([1], [2])]), ("jit_subi_d", [([1], [2])]),
 ("jit_subi_f", [([1], [2])]), ("jit_subr", [([1], [2, 3])]),
 ("jit_subr_d", [([1], [2, 3])]), ("jit_subr_f", [([1], [2, 3])]),
 ("jit_subxi", [([1], [2])]), ("jit_subxr", [([1], [2, 3])]),
 ("jit_truncr_d_i", [([1], [2])]), ("jit_truncr_f_i", [([1], [2])]),
 ("jit_uneqi_d", [([1], [2])]), ("jit_uneqi_f", [([1], [2])]),
 ("jit_uneqr_d", [([1], [2, 3])]), ("jit_uneqr_f", [([1], [2, 3])]),
 ("jit_ungei_d", [([1], [2])]), ("jit_ungei_f", [([1], [2])]),
 ("jit_unger_d", [([1], [2, 3])]), ("jit_unger_f", [([1], [2, 3])]),
 ("jit_ungti_d", [([1], [2])]), ("jit_ungti_f", [([1], [2])]),
 ("jit_ungtr_d", [([1], [2, 3])]), ("jit_ungtr_f", [([1], [2, 3])]),
 ("jit_unlei_d", [([1], [2])]), ("jit_unlei_f", [([1], [2])]),
 ("jit_unler_d", [([1], [2, 3])]), ("jit_unler_f", [([1], [2, 3])]),
 ("jit_unlti_d", [([1], [2])]), ("jit_unlti_f", [([1], [2])]),
 ("jit_unltr_d", [([1], [2, 3])]), ("jit_unltr_f", [([1], [2, 3])]),
 ("jit_unordi_d", [([1], [2])]), ("jit_unordi_f", [([1], [2])]),
 ("jit_unordr_d", [([1], [2, 3])]), ("jit_unordr_f", [([1], [2, 3])]),
 ("jit_xori", [([1], [2])]), ("jit_xorr", [([1], [2, 3])]),
 ("jit_finishr", [([], [1])]), ("jit_getarg_c", [([1], [])]),
 ("jit_getarg_d", [([1], [])]), ("jit_getarg_f", [([1], [])]),
 ("jit_getarg_i", [([1], [])]), ("jit_getarg_s", [([1], [])]),
 ("jit_getarg_uc", [([1], [])]), ("jit_getarg_us", [([1], [])]),
 ("jit_pushargr", [([], [1])]), ("jit_pushargr_d", [([], [1])]),
 ("jit_pushargr_f", [([], [1])]), ("jit_retr", [([], [1])]),
 ("jit_retr_d", [([], [1])]), ("jit_retr_f", [([], [1])]),
 ("jit_retval_c", [([1], [])]), ("jit_retval_d", [([1], [])]),
 ("jit_retval_f", [([1], [])]), ("jit_retval_i", [([1], [])]),
 ("jit_retval_s", [([1], [])]), ("jit_retval_uc", [([1], [])]),
 ("jit_retval_us", [([1], [])])];

structure jit_code_t = 
struct
   val jit_code_data = 0
   val jit_code_live = 1
   val jit_code_save = 2
   val jit_code_load = 3
   val jit_code_name = 4
   val jit_code_note = 5
   val jit_code_label = 6
   val jit_code_prolog = 7
   val jit_code_arg = 8
   val jit_code_addr = 9
   val jit_code_addi = 10
   val jit_code_addcr = 11
   val jit_code_addci = 12
   val jit_code_addxr = 13
   val jit_code_addxi = 14
   val jit_code_subr = 15
   val jit_code_subi = 16
   val jit_code_subcr = 17
   val jit_code_subci = 18
   val jit_code_subxr = 19
   val jit_code_subxi = 20
   val jit_code_mulr = 21
   val jit_code_muli = 22
   val jit_code_qmulr = 23
   val jit_code_qmuli = 24
   val jit_code_qmulr_u = 25
   val jit_code_qmuli_u = 26
   val jit_code_divr = 27
   val jit_code_divi = 28
   val jit_code_divr_u = 29
   val jit_code_divi_u = 30
   val jit_code_qdivr = 31
   val jit_code_qdivi = 32
   val jit_code_qdivr_u = 33
   val jit_code_qdivi_u = 34
   val jit_code_remr = 35
   val jit_code_remi = 36
   val jit_code_remr_u = 37
   val jit_code_remi_u = 38
   val jit_code_andr = 39
   val jit_code_andi = 40
   val jit_code_orr = 41
   val jit_code_ori = 42
   val jit_code_xorr = 43
   val jit_code_xori = 44
   val jit_code_lshr = 45
   val jit_code_lshi = 46
   val jit_code_rshr = 47
   val jit_code_rshi = 48
   val jit_code_rshr_u = 49
   val jit_code_rshi_u = 50
   val jit_code_negr = 51
   val jit_code_comr = 52
   val jit_code_ltr = 53
   val jit_code_lti = 54
   val jit_code_ltr_u = 55
   val jit_code_lti_u = 56
   val jit_code_ler = 57
   val jit_code_lei = 58
   val jit_code_ler_u = 59
   val jit_code_lei_u = 60
   val jit_code_eqr = 61
   val jit_code_eqi = 62
   val jit_code_ger = 63
   val jit_code_gei = 64
   val jit_code_ger_u = 65
   val jit_code_gei_u = 66
   val jit_code_gtr = 67
   val jit_code_gti = 68
   val jit_code_gtr_u = 69
   val jit_code_gti_u = 70
   val jit_code_ner = 71
   val jit_code_nei = 72
   val jit_code_movr = 73
   val jit_code_movi = 74
   val jit_code_extr_c = 75
   val jit_code_extr_uc = 76
   val jit_code_extr_s = 77
   val jit_code_extr_us = 78
   val jit_code_extr_i = 79
   val jit_code_extr_ui = 80
   val jit_code_htonr = 81
   val jit_code_ldr_c = 82
   val jit_code_ldi_c = 83
   val jit_code_ldr_uc = 84
   val jit_code_ldi_uc = 85
   val jit_code_ldr_s = 86
   val jit_code_ldi_s = 87
   val jit_code_ldr_us = 88
   val jit_code_ldi_us = 89
   val jit_code_ldr_i = 90
   val jit_code_ldi_i = 91
   val jit_code_ldr_ui = 92
   val jit_code_ldi_ui = 93
   val jit_code_ldr_l = 94
   val jit_code_ldi_l = 95
   val jit_code_ldxr_c = 96
   val jit_code_ldxi_c = 97
   val jit_code_ldxr_uc = 98
   val jit_code_ldxi_uc = 99
   val jit_code_ldxr_s = 100
   val jit_code_ldxi_s = 101
   val jit_code_ldxr_us = 102
   val jit_code_ldxi_us = 103
   val jit_code_ldxr_i = 104
   val jit_code_ldxi_i = 105
   val jit_code_ldxr_ui = 106
   val jit_code_ldxi_ui = 107
   val jit_code_ldxr_l = 108
   val jit_code_ldxi_l = 109
   val jit_code_str_c = 110
   val jit_code_sti_c = 111
   val jit_code_str_s = 112
   val jit_code_sti_s = 113
   val jit_code_str_i = 114
   val jit_code_sti_i = 115
   val jit_code_str_l = 116
   val jit_code_sti_l = 117
   val jit_code_stxr_c = 118
   val jit_code_stxi_c = 119
   val jit_code_stxr_s = 120
   val jit_code_stxi_s = 121
   val jit_code_stxr_i = 122
   val jit_code_stxi_i = 123
   val jit_code_stxr_l = 124
   val jit_code_stxi_l = 125
   val jit_code_bltr = 126
   val jit_code_blti = 127
   val jit_code_bltr_u = 128
   val jit_code_blti_u = 129
   val jit_code_bler = 130
   val jit_code_blei = 131
   val jit_code_bler_u = 132
   val jit_code_blei_u = 133
   val jit_code_beqr = 134
   val jit_code_beqi = 135
   val jit_code_bger = 136
   val jit_code_bgei = 137
   val jit_code_bger_u = 138
   val jit_code_bgei_u = 139
   val jit_code_bgtr = 140
   val jit_code_bgti = 141
   val jit_code_bgtr_u = 142
   val jit_code_bgti_u = 143
   val jit_code_bner = 144
   val jit_code_bnei = 145
   val jit_code_bmsr = 146
   val jit_code_bmsi = 147
   val jit_code_bmcr = 148
   val jit_code_bmci = 149
   val jit_code_boaddr = 150
   val jit_code_boaddi = 151
   val jit_code_boaddr_u = 152
   val jit_code_boaddi_u = 153
   val jit_code_bxaddr = 154
   val jit_code_bxaddi = 155
   val jit_code_bxaddr_u = 156
   val jit_code_bxaddi_u = 157
   val jit_code_bosubr = 158
   val jit_code_bosubi = 159
   val jit_code_bosubr_u = 160
   val jit_code_bosubi_u = 161
   val jit_code_bxsubr = 162
   val jit_code_bxsubi = 163
   val jit_code_bxsubr_u = 164
   val jit_code_bxsubi_u = 165
   val jit_code_jmpr = 166
   val jit_code_jmpi = 167
   val jit_code_callr = 168
   val jit_code_calli = 169
   val jit_code_epilog = 170
   val jit_code_arg_f = 171
   val jit_code_addr_f = 172
   val jit_code_addi_f = 173
   val jit_code_subr_f = 174
   val jit_code_subi_f = 175
   val jit_code_mulr_f = 176
   val jit_code_muli_f = 177
   val jit_code_divr_f = 178
   val jit_code_divi_f = 179
   val jit_code_negr_f = 180
   val jit_code_absr_f = 181
   val jit_code_sqrtr_f = 182
   val jit_code_ltr_f = 183
   val jit_code_lti_f = 184
   val jit_code_ler_f = 185
   val jit_code_lei_f = 186
   val jit_code_eqr_f = 187
   val jit_code_eqi_f = 188
   val jit_code_ger_f = 189
   val jit_code_gei_f = 190
   val jit_code_gtr_f = 191
   val jit_code_gti_f = 192
   val jit_code_ner_f = 193
   val jit_code_nei_f = 194
   val jit_code_unltr_f = 195
   val jit_code_unlti_f = 196
   val jit_code_unler_f = 197
   val jit_code_unlei_f = 198
   val jit_code_uneqr_f = 199
   val jit_code_uneqi_f = 200
   val jit_code_unger_f = 201
   val jit_code_ungei_f = 202
   val jit_code_ungtr_f = 203
   val jit_code_ungti_f = 204
   val jit_code_ltgtr_f = 205
   val jit_code_ltgti_f = 206
   val jit_code_ordr_f = 207
   val jit_code_ordi_f = 208
   val jit_code_unordr_f = 209
   val jit_code_unordi_f = 210
   val jit_code_truncr_f_i = 211
   val jit_code_truncr_f_l = 212
   val jit_code_extr_f = 213
   val jit_code_extr_d_f = 214
   val jit_code_movr_f = 215
   val jit_code_movi_f = 216
   val jit_code_ldr_f = 217
   val jit_code_ldi_f = 218
   val jit_code_ldxr_f = 219
   val jit_code_ldxi_f = 220
   val jit_code_str_f = 221
   val jit_code_sti_f = 222
   val jit_code_stxr_f = 223
   val jit_code_stxi_f = 224
   val jit_code_bltr_f = 225
   val jit_code_blti_f = 226
   val jit_code_bler_f = 227
   val jit_code_blei_f = 228
   val jit_code_beqr_f = 229
   val jit_code_beqi_f = 230
   val jit_code_bger_f = 231
   val jit_code_bgei_f = 232
   val jit_code_bgtr_f = 233
   val jit_code_bgti_f = 234
   val jit_code_bner_f = 235
   val jit_code_bnei_f = 236
   val jit_code_bunltr_f = 237
   val jit_code_bunlti_f = 238
   val jit_code_bunler_f = 239
   val jit_code_bunlei_f = 240
   val jit_code_buneqr_f = 241
   val jit_code_buneqi_f = 242
   val jit_code_bunger_f = 243
   val jit_code_bungei_f = 244
   val jit_code_bungtr_f = 245
   val jit_code_bungti_f = 246
   val jit_code_bltgtr_f = 247
   val jit_code_bltgti_f = 248
   val jit_code_bordr_f = 249
   val jit_code_bordi_f = 250
   val jit_code_bunordr_f = 251
   val jit_code_bunordi_f = 252
   val jit_code_arg_d = 253
   val jit_code_addr_d = 254
   val jit_code_addi_d = 255
   val jit_code_subr_d = 256
   val jit_code_subi_d = 257
   val jit_code_mulr_d = 258
   val jit_code_muli_d = 259
   val jit_code_divr_d = 260
   val jit_code_divi_d = 261
   val jit_code_negr_d = 262
   val jit_code_absr_d = 263
   val jit_code_sqrtr_d = 264
   val jit_code_ltr_d = 265
   val jit_code_lti_d = 266
   val jit_code_ler_d = 267
   val jit_code_lei_d = 268
   val jit_code_eqr_d = 269
   val jit_code_eqi_d = 270
   val jit_code_ger_d = 271
   val jit_code_gei_d = 272
   val jit_code_gtr_d = 273
   val jit_code_gti_d = 274
   val jit_code_ner_d = 275
   val jit_code_nei_d = 276
   val jit_code_unltr_d = 277
   val jit_code_unlti_d = 278
   val jit_code_unler_d = 279
   val jit_code_unlei_d = 280
   val jit_code_uneqr_d = 281
   val jit_code_uneqi_d = 282
   val jit_code_unger_d = 283
   val jit_code_ungei_d = 284
   val jit_code_ungtr_d = 285
   val jit_code_ungti_d = 286
   val jit_code_ltgtr_d = 287
   val jit_code_ltgti_d = 288
   val jit_code_ordr_d = 289
   val jit_code_ordi_d = 290
   val jit_code_unordr_d = 291
   val jit_code_unordi_d = 292
   val jit_code_truncr_d_i = 293
   val jit_code_truncr_d_l = 294
   val jit_code_extr_d = 295
   val jit_code_extr_f_d = 296
   val jit_code_movr_d = 297
   val jit_code_movi_d = 298
   val jit_code_ldr_d = 299
   val jit_code_ldi_d = 300
   val jit_code_ldxr_d = 301
   val jit_code_ldxi_d = 302
   val jit_code_str_d = 303
   val jit_code_sti_d = 304
   val jit_code_stxr_d = 305
   val jit_code_stxi_d = 306
   val jit_code_bltr_d = 307
   val jit_code_blti_d = 308
   val jit_code_bler_d = 309
   val jit_code_blei_d = 310
   val jit_code_beqr_d = 311
   val jit_code_beqi_d = 312
   val jit_code_bger_d = 313
   val jit_code_bgei_d = 314
   val jit_code_bgtr_d = 315
   val jit_code_bgti_d = 316
   val jit_code_bner_d = 317
   val jit_code_bnei_d = 318
   val jit_code_bunltr_d = 319
   val jit_code_bunlti_d = 320
   val jit_code_bunler_d = 321
   val jit_code_bunlei_d = 322
   val jit_code_buneqr_d = 323
   val jit_code_buneqi_d = 324
   val jit_code_bunger_d = 325
   val jit_code_bungei_d = 326
   val jit_code_bungtr_d = 327
   val jit_code_bungti_d = 328
   val jit_code_bltgtr_d = 329
   val jit_code_bltgti_d = 330
   val jit_code_bordr_d = 331
   val jit_code_bordi_d = 332
   val jit_code_bunordr_d = 333
   val jit_code_bunordi_d = 334
   val jit_code_movr_w_f = 335
   val jit_code_movr_ww_d = 336
   val jit_code_movr_w_d = 337
   val jit_code_movr_f_w = 338
   val jit_code_movi_f_w = 339
   val jit_code_movr_d_ww = 340
   val jit_code_movi_d_ww = 341
   val jit_code_movr_d_w = 342
   val jit_code_movi_d_w = 343
   val jit_code_x86_retval_f = 344
   val jit_code_x86_retval_d = 345

   val index =
     #[("jit_code_absr_d", 0wx107),
       ("jit_code_absr_f", 0wxB5),
       ("jit_code_addci", 0wxC),
       ("jit_code_addcr", 0wxB),
       ("jit_code_addi", 0wxA),
       ("jit_code_addi_d", 0wxFF),
       ("jit_code_addi_f", 0wxAD),
       ("jit_code_addr", 0wx9),
       ("jit_code_addr_d", 0wxFE),
       ("jit_code_addr_f", 0wxAC),
       ("jit_code_addxi", 0wxE),
       ("jit_code_addxr", 0wxD),
       ("jit_code_andi", 0wx28),
       ("jit_code_andr", 0wx27),
       ("jit_code_arg", 0wx8),
       ("jit_code_arg_d", 0wxFD),
       ("jit_code_arg_f", 0wxAB),
       ("jit_code_beqi", 0wx87),
       ("jit_code_beqi_d", 0wx138),
       ("jit_code_beqi_f", 0wxE6),
       ("jit_code_beqr", 0wx86),
       ("jit_code_beqr_d", 0wx137),
       ("jit_code_beqr_f", 0wxE5),
       ("jit_code_bgei", 0wx89),
       ("jit_code_bgei_d", 0wx13A),
       ("jit_code_bgei_f", 0wxE8),
       ("jit_code_bgei_u", 0wx8B),
       ("jit_code_bger", 0wx88),
       ("jit_code_bger_d", 0wx139),
       ("jit_code_bger_f", 0wxE7),
       ("jit_code_bger_u", 0wx8A),
       ("jit_code_bgti", 0wx8D),
       ("jit_code_bgti_d", 0wx13C),
       ("jit_code_bgti_f", 0wxEA),
       ("jit_code_bgti_u", 0wx8F),
       ("jit_code_bgtr", 0wx8C),
       ("jit_code_bgtr_d", 0wx13B),
       ("jit_code_bgtr_f", 0wxE9),
       ("jit_code_bgtr_u", 0wx8E),
       ("jit_code_blei", 0wx83),
       ("jit_code_blei_d", 0wx136),
       ("jit_code_blei_f", 0wxE4),
       ("jit_code_blei_u", 0wx85),
       ("jit_code_bler", 0wx82),
       ("jit_code_bler_d", 0wx135),
       ("jit_code_bler_f", 0wxE3),
       ("jit_code_bler_u", 0wx84),
       ("jit_code_bltgti_d", 0wx14A),
       ("jit_code_bltgti_f", 0wxF8),
       ("jit_code_bltgtr_d", 0wx149),
       ("jit_code_bltgtr_f", 0wxF7),
       ("jit_code_blti", 0wx7F),
       ("jit_code_blti_d", 0wx134),
       ("jit_code_blti_f", 0wxE2),
       ("jit_code_blti_u", 0wx81),
       ("jit_code_bltr", 0wx7E),
       ("jit_code_bltr_d", 0wx133),
       ("jit_code_bltr_f", 0wxE1),
       ("jit_code_bltr_u", 0wx80),
       ("jit_code_bmci", 0wx95),
       ("jit_code_bmcr", 0wx94),
       ("jit_code_bmsi", 0wx93),
       ("jit_code_bmsr", 0wx92),
       ("jit_code_bnei", 0wx91),
       ("jit_code_bnei_d", 0wx13E),
       ("jit_code_bnei_f", 0wxEC),
       ("jit_code_bner", 0wx90),
       ("jit_code_bner_d", 0wx13D),
       ("jit_code_bner_f", 0wxEB),
       ("jit_code_boaddi", 0wx97),
       ("jit_code_boaddi_u", 0wx99),
       ("jit_code_boaddr", 0wx96),
       ("jit_code_boaddr_u", 0wx98),
       ("jit_code_bordi_d", 0wx14C),
       ("jit_code_bordi_f", 0wxFA),
       ("jit_code_bordr_d", 0wx14B),
       ("jit_code_bordr_f", 0wxF9),
       ("jit_code_bosubi", 0wx9F),
       ("jit_code_bosubi_u", 0wxA1),
       ("jit_code_bosubr", 0wx9E),
       ("jit_code_bosubr_u", 0wxA0),
       ("jit_code_buneqi_d", 0wx144),
       ("jit_code_buneqi_f", 0wxF2),
       ("jit_code_buneqr_d", 0wx143),
       ("jit_code_buneqr_f", 0wxF1),
       ("jit_code_bungei_d", 0wx146),
       ("jit_code_bungei_f", 0wxF4),
       ("jit_code_bunger_d", 0wx145),
       ("jit_code_bunger_f", 0wxF3),
       ("jit_code_bungti_d", 0wx148),
       ("jit_code_bungti_f", 0wxF6),
       ("jit_code_bungtr_d", 0wx147),
       ("jit_code_bungtr_f", 0wxF5),
       ("jit_code_bunlei_d", 0wx142),
       ("jit_code_bunlei_f", 0wxF0),
       ("jit_code_bunler_d", 0wx141),
       ("jit_code_bunler_f", 0wxEF),
       ("jit_code_bunlti_d", 0wx140),
       ("jit_code_bunlti_f", 0wxEE),
       ("jit_code_bunltr_d", 0wx13F),
       ("jit_code_bunltr_f", 0wxED),
       ("jit_code_bunordi_d", 0wx14E),
       ("jit_code_bunordi_f", 0wxFC),
       ("jit_code_bunordr_d", 0wx14D),
       ("jit_code_bunordr_f", 0wxFB),
       ("jit_code_bxaddi", 0wx9B),
       ("jit_code_bxaddi_u", 0wx9D),
       ("jit_code_bxaddr", 0wx9A),
       ("jit_code_bxaddr_u", 0wx9C),
       ("jit_code_bxsubi", 0wxA3),
       ("jit_code_bxsubi_u", 0wxA5),
       ("jit_code_bxsubr", 0wxA2),
       ("jit_code_bxsubr_u", 0wxA4),
       ("jit_code_calli", 0wxA9),
       ("jit_code_callr", 0wxA8),
       ("jit_code_comr", 0wx34),
       ("jit_code_data", 0wx0),
       ("jit_code_divi", 0wx1C),
       ("jit_code_divi_d", 0wx105),
       ("jit_code_divi_f", 0wxB3),
       ("jit_code_divi_u", 0wx1E),
       ("jit_code_divr", 0wx1B),
       ("jit_code_divr_d", 0wx104),
       ("jit_code_divr_f", 0wxB2),
       ("jit_code_divr_u", 0wx1D),
       ("jit_code_epilog", 0wxAA),
       ("jit_code_eqi", 0wx3E),
       ("jit_code_eqi_d", 0wx10E),
       ("jit_code_eqi_f", 0wxBC),
       ("jit_code_eqr", 0wx3D),
       ("jit_code_eqr_d", 0wx10D),
       ("jit_code_eqr_f", 0wxBB),
       ("jit_code_extr_c", 0wx4B),
       ("jit_code_extr_d", 0wx127),
       ("jit_code_extr_d_f", 0wxD6),
       ("jit_code_extr_f", 0wxD5),
       ("jit_code_extr_f_d", 0wx128),
       ("jit_code_extr_i", 0wx4F),
       ("jit_code_extr_s", 0wx4D),
       ("jit_code_extr_uc", 0wx4C),
       ("jit_code_extr_ui", 0wx50),
       ("jit_code_extr_us", 0wx4E),
       ("jit_code_gei", 0wx40),
       ("jit_code_gei_d", 0wx110),
       ("jit_code_gei_f", 0wxBE),
       ("jit_code_gei_u", 0wx42),
       ("jit_code_ger", 0wx3F),
       ("jit_code_ger_d", 0wx10F),
       ("jit_code_ger_f", 0wxBD),
       ("jit_code_ger_u", 0wx41),
       ("jit_code_gti", 0wx44),
       ("jit_code_gti_d", 0wx112),
       ("jit_code_gti_f", 0wxC0),
       ("jit_code_gti_u", 0wx46),
       ("jit_code_gtr", 0wx43),
       ("jit_code_gtr_d", 0wx111),
       ("jit_code_gtr_f", 0wxBF),
       ("jit_code_gtr_u", 0wx45),
       ("jit_code_htonr", 0wx51),
       ("jit_code_jmpi", 0wxA7),
       ("jit_code_jmpr", 0wxA6),
       ("jit_code_label", 0wx6),
       ("jit_code_ldi_c", 0wx53),
       ("jit_code_ldi_d", 0wx12C),
       ("jit_code_ldi_f", 0wxDA),
       ("jit_code_ldi_i", 0wx5B),
       ("jit_code_ldi_l", 0wx5F),
       ("jit_code_ldi_s", 0wx57),
       ("jit_code_ldi_uc", 0wx55),
       ("jit_code_ldi_ui", 0wx5D),
       ("jit_code_ldi_us", 0wx59),
       ("jit_code_ldr_c", 0wx52),
       ("jit_code_ldr_d", 0wx12B),
       ("jit_code_ldr_f", 0wxD9),
       ("jit_code_ldr_i", 0wx5A),
       ("jit_code_ldr_l", 0wx5E),
       ("jit_code_ldr_s", 0wx56),
       ("jit_code_ldr_uc", 0wx54),
       ("jit_code_ldr_ui", 0wx5C),
       ("jit_code_ldr_us", 0wx58),
       ("jit_code_ldxi_c", 0wx61),
       ("jit_code_ldxi_d", 0wx12E),
       ("jit_code_ldxi_f", 0wxDC),
       ("jit_code_ldxi_i", 0wx69),
       ("jit_code_ldxi_l", 0wx6D),
       ("jit_code_ldxi_s", 0wx65),
       ("jit_code_ldxi_uc", 0wx63),
       ("jit_code_ldxi_ui", 0wx6B),
       ("jit_code_ldxi_us", 0wx67),
       ("jit_code_ldxr_c", 0wx60),
       ("jit_code_ldxr_d", 0wx12D),
       ("jit_code_ldxr_f", 0wxDB),
       ("jit_code_ldxr_i", 0wx68),
       ("jit_code_ldxr_l", 0wx6C),
       ("jit_code_ldxr_s", 0wx64),
       ("jit_code_ldxr_uc", 0wx62),
       ("jit_code_ldxr_ui", 0wx6A),
       ("jit_code_ldxr_us", 0wx66),
       ("jit_code_lei", 0wx3A),
       ("jit_code_lei_d", 0wx10C),
       ("jit_code_lei_f", 0wxBA),
       ("jit_code_lei_u", 0wx3C),
       ("jit_code_ler", 0wx39),
       ("jit_code_ler_d", 0wx10B),
       ("jit_code_ler_f", 0wxB9),
       ("jit_code_ler_u", 0wx3B),
       ("jit_code_live", 0wx1),
       ("jit_code_load", 0wx3),
       ("jit_code_lshi", 0wx2E),
       ("jit_code_lshr", 0wx2D),
       ("jit_code_ltgti_d", 0wx120),
       ("jit_code_ltgti_f", 0wxCE),
       ("jit_code_ltgtr_d", 0wx11F),
       ("jit_code_ltgtr_f", 0wxCD),
       ("jit_code_lti", 0wx36),
       ("jit_code_lti_d", 0wx10A),
       ("jit_code_lti_f", 0wxB8),
       ("jit_code_lti_u", 0wx38),
       ("jit_code_ltr", 0wx35),
       ("jit_code_ltr_d", 0wx109),
       ("jit_code_ltr_f", 0wxB7),
       ("jit_code_ltr_u", 0wx37),
       ("jit_code_movi", 0wx4A),
       ("jit_code_movi_d", 0wx12A),
       ("jit_code_movi_d_w", 0wx157),
       ("jit_code_movi_d_ww", 0wx155),
       ("jit_code_movi_f", 0wxD8),
       ("jit_code_movi_f_w", 0wx153),
       ("jit_code_movr", 0wx49),
       ("jit_code_movr_d", 0wx129),
       ("jit_code_movr_d_w", 0wx156),
       ("jit_code_movr_d_ww", 0wx154),
       ("jit_code_movr_f", 0wxD7),
       ("jit_code_movr_f_w", 0wx152),
       ("jit_code_movr_w_d", 0wx151),
       ("jit_code_movr_w_f", 0wx14F),
       ("jit_code_movr_ww_d", 0wx150),
       ("jit_code_muli", 0wx16),
       ("jit_code_muli_d", 0wx103),
       ("jit_code_muli_f", 0wxB1),
       ("jit_code_mulr", 0wx15),
       ("jit_code_mulr_d", 0wx102),
       ("jit_code_mulr_f", 0wxB0),
       ("jit_code_name", 0wx4),
       ("jit_code_negr", 0wx33),
       ("jit_code_negr_d", 0wx106),
       ("jit_code_negr_f", 0wxB4),
       ("jit_code_nei", 0wx48),
       ("jit_code_nei_d", 0wx114),
       ("jit_code_nei_f", 0wxC2),
       ("jit_code_ner", 0wx47),
       ("jit_code_ner_d", 0wx113),
       ("jit_code_ner_f", 0wxC1),
       ("jit_code_note", 0wx5),
       ("jit_code_ordi_d", 0wx122),
       ("jit_code_ordi_f", 0wxD0),
       ("jit_code_ordr_d", 0wx121),
       ("jit_code_ordr_f", 0wxCF),
       ("jit_code_ori", 0wx2A),
       ("jit_code_orr", 0wx29),
       ("jit_code_prolog", 0wx7),
       ("jit_code_qdivi", 0wx20),
       ("jit_code_qdivi_u", 0wx22),
       ("jit_code_qdivr", 0wx1F),
       ("jit_code_qdivr_u", 0wx21),
       ("jit_code_qmuli", 0wx18),
       ("jit_code_qmuli_u", 0wx1A),
       ("jit_code_qmulr", 0wx17),
       ("jit_code_qmulr_u", 0wx19),
       ("jit_code_remi", 0wx24),
       ("jit_code_remi_u", 0wx26),
       ("jit_code_remr", 0wx23),
       ("jit_code_remr_u", 0wx25),
       ("jit_code_rshi", 0wx30),
       ("jit_code_rshi_u", 0wx32),
       ("jit_code_rshr", 0wx2F),
       ("jit_code_rshr_u", 0wx31),
       ("jit_code_save", 0wx2),
       ("jit_code_sqrtr_d", 0wx108),
       ("jit_code_sqrtr_f", 0wxB6),
       ("jit_code_sti_c", 0wx6F),
       ("jit_code_sti_d", 0wx130),
       ("jit_code_sti_f", 0wxDE),
       ("jit_code_sti_i", 0wx73),
       ("jit_code_sti_l", 0wx75),
       ("jit_code_sti_s", 0wx71),
       ("jit_code_str_c", 0wx6E),
       ("jit_code_str_d", 0wx12F),
       ("jit_code_str_f", 0wxDD),
       ("jit_code_str_i", 0wx72),
       ("jit_code_str_l", 0wx74),
       ("jit_code_str_s", 0wx70),
       ("jit_code_stxi_c", 0wx77),
       ("jit_code_stxi_d", 0wx132),
       ("jit_code_stxi_f", 0wxE0),
       ("jit_code_stxi_i", 0wx7B),
       ("jit_code_stxi_l", 0wx7D),
       ("jit_code_stxi_s", 0wx79),
       ("jit_code_stxr_c", 0wx76),
       ("jit_code_stxr_d", 0wx131),
       ("jit_code_stxr_f", 0wxDF),
       ("jit_code_stxr_i", 0wx7A),
       ("jit_code_stxr_l", 0wx7C),
       ("jit_code_stxr_s", 0wx78),
       ("jit_code_subci", 0wx12),
       ("jit_code_subcr", 0wx11),
       ("jit_code_subi", 0wx10),
       ("jit_code_subi_d", 0wx101),
       ("jit_code_subi_f", 0wxAF),
       ("jit_code_subr", 0wxF),
       ("jit_code_subr_d", 0wx100),
       ("jit_code_subr_f", 0wxAE),
       ("jit_code_subxi", 0wx14),
       ("jit_code_subxr", 0wx13),
       ("jit_code_truncr_d_i", 0wx125),
       ("jit_code_truncr_d_l", 0wx126),
       ("jit_code_truncr_f_i", 0wxD3),
       ("jit_code_truncr_f_l", 0wxD4),
       ("jit_code_uneqi_d", 0wx11A),
       ("jit_code_uneqi_f", 0wxC8),
       ("jit_code_uneqr_d", 0wx119),
       ("jit_code_uneqr_f", 0wxC7),
       ("jit_code_ungei_d", 0wx11C),
       ("jit_code_ungei_f", 0wxCA),
       ("jit_code_unger_d", 0wx11B),
       ("jit_code_unger_f", 0wxC9),
       ("jit_code_ungti_d", 0wx11E),
       ("jit_code_ungti_f", 0wxCC),
       ("jit_code_ungtr_d", 0wx11D),
       ("jit_code_ungtr_f", 0wxCB),
       ("jit_code_unlei_d", 0wx118),
       ("jit_code_unlei_f", 0wxC6),
       ("jit_code_unler_d", 0wx117),
       ("jit_code_unler_f", 0wxC5),
       ("jit_code_unlti_d", 0wx116),
       ("jit_code_unlti_f", 0wxC4),
       ("jit_code_unltr_d", 0wx115),
       ("jit_code_unltr_f", 0wxC3),
       ("jit_code_unordi_d", 0wx124),
       ("jit_code_unordi_f", 0wxD2),
       ("jit_code_unordr_d", 0wx123),
       ("jit_code_unordr_f", 0wxD1),
       ("jit_code_x86_retval_d", 0wx159),
       ("jit_code_x86_retval_f", 0wx158),
       ("jit_code_xori", 0wx2C),
       ("jit_code_xorr", 0wx2B)]

   val xedni =
        Array.vector
          (Vector.foldri
              (fn (i,x,a) => (Array.update (a,Word.toInt (#2 x), i);a))
              (Array.array(Vector.length index,0))
              index)

   fun indexToString i = #1 (Vector.sub(index,Vector.sub(xedni,i)))

   (* Is that a Galois connection I wonder? *)

   fun search comp (vec : (string * 'a) Vector.vector) =
      fn s =>
         let fun iter l h =
                let val m = l + (h - l) div 2
                    val p = Vector.sub(vec,m)
                    val c = comp (s,#1 p)
                in if c = EQUAL
                   then SOME (#2 p)
                   else if h = l
                           then NONE
                           else if c = LESS 
                                   then iter l m
                                   else iter m h
                end
         in iter 0 (Vector.length vec - 1)
         end
   val indexFromString = search String.compare index
end

fun mkregmap l =
    scm_list (List.map 
                 (fn s => scm_list [scm_symbol s,
                                    scm_from_ulong (Ffi.jit_get_constant s)]) l)

val lookup_code = Option.valOf o jit_code_t.indexFromString
val scm_lookup_code = scm_from_ulong o lookup_code
val scm_code = scm_from_ulong o lookup_code

val defs = scm_qlToString `
         (use-modules (system foreign))
         (use-modules (rnrs enums))
         (define jit-gpr-num
            (lambda (x)
               (cadr (assq x (quote ^(mkregmap ["R0","R1","R2","V0","V1","V2","FP"]))))))
         (define jit-fpr-num
            (lambda (x)
               (cadr (assq x (quote ^(mkregmap ["F0","F1","F2","F3","F4","F5","F6"]))))))
         (define liblightning (dynamic-link "liblightning"))
         (define init-jit-sym (dynamic-func "init_jit" liblightning))
         (define init-jit (pointer->procedure void init-jit-sym (list (list '* '*))))
         (define argv0 (make-c-struct (list '* '*)
                                      (list (string->pointer (car (command-line)))
                                             %null-pointer)))
         (init-jit argv0)
         (define-wrapped-pointer-type jit-state
            jit-state?
            jit-wrap-state jit-unwrap-state
            (lambda (s p)
              (format p "#<jit-state ~x>"
                      (pointer-address (jit-unwrap-state s)))))
         (define-wrapped-pointer-type jit-node
            jit-node?
            jit-wrap-node jit-unwrap-node
            (lambda (s p)
              (format p "#<jit-node ~x>"
                      (pointer-address (jit-unwrap-node s)))))
         (define jit-new-state
            ;; Wrapper for jit_new_state.
            (let* ((jit-new-state-sym (dynamic-func "jit_new_state" liblightning))
                   (jit-new-state-proc (pointer->procedure '* jit-new-state-sym '())))
              (lambda ()
                "Return a new JIT state."
                (jit-wrap-state (jit-new-state-proc)))))
         (define jit-emit
            ;; Wrapper for jit_emit.
            (let* ((jit-emit-sym (dynamic-func "_jit_emit" liblightning))
                   (jit-emit-proc (pointer->procedure '* jit-emit-sym (list '*))))
              (lambda (jit-state)
                "Return the compiled code."
                (jit-emit-proc (jit-unwrap-state jit-state)))))
         (define jit-prolog
            ;; Wrapper for jit_prolog.
            (let* ((jit-prolog-sym (dynamic-func "_jit_prolog" liblightning))
                   (jit-prolog-proc (pointer->procedure void jit-prolog-sym
                                                               (list '*))))
              (lambda (jit-)
                "Start a function definition."
                (jit-prolog-proc (jit-unwrap-state jit-)))))
         (define jit-arg
            ;; Wrapper for jit_arg.
            (let* ((jit-arg-sym (dynamic-func "_jit_arg" liblightning))
                   (jit-arg-proc (pointer->procedure '* jit-arg-sym 
                                                        (list '*))))
              (lambda (jit-)
                "Get a pointer to the next argument."
                (jit-wrap-node (jit-arg-proc (jit-unwrap-state jit-))))))
         (define jit-getarg-i
            ;; Wrapper for jit_getarg_i.
            (let* ((jit-getarg-i-sym (dynamic-func "_jit_getarg_i" liblightning))
                   (jit-getarg-i-proc (pointer->procedure void jit-getarg-i-sym
                                                         (list '* unsigned-long '*))))
              (lambda (jit- gpr node)
                "Load the integer argument into a register."
                (jit-getarg-i-proc (jit-unwrap-state jit-)
                                   (jit-gpr-num gpr)
                                   (jit-unwrap-node node)))))
         (define jit-retr
            ;; Wrapper for jit_retr.
            (let* ((jit-retr-sym (dynamic-func "_jit_retr" liblightning))
                   (jit-retr-proc (pointer->procedure void jit-retr-sym
                                                         (list '* unsigned-long))))
              (lambda (jit- gpr)
                "Return the register contents."
                (jit-retr-proc (jit-unwrap-state jit-)
                               (jit-gpr-num gpr)))))
         (define jit-new-node-www
            ;; Wrapper for jit_new-node-www.
            (let* ((jit-new-node-www-sym (dynamic-func "_jit_new_node_www" liblightning))
                   (jit-new-node-www-proc (pointer->procedure '* jit-new-node-www-sym 
                                                        (list '*
                                                              unsigned-long
                                                              unsigned-long
                                                              unsigned-long
                                                              unsigned-long))))
              (lambda (jit- c u v w)
                "Return the next jit node."
                (jit-wrap-node (jit-new-node-www-proc (jit-unwrap-state jit-) c u v w)))))
         (define jit-addr
             (let ((jit-code-addr ^(scm_code "jit_code_addr")))
                  (lambda (jit- r1 r2 r3)
                       "Generate addr instruction(s)."
                       (jit-new-node-www
                            jit-
                            jit-code-addr
                           (jit-gpr-num r1)
                           (jit-gpr-num r2)
                           (jit-gpr-num r3)))))
         (define jit-addi
             (let ((jit-code-addi ^(scm_code "jit_code_addi")))
                  (lambda (jit- r1 r2 n)
                       "Generate addi instruction(s)."
                       (jit-new-node-www
                            jit-
                            jit-code-addi
                           (jit-gpr-num r1)
                           (jit-gpr-num r2)
                            n))))
`;

(*
(define successor
 (let ((ptr
   (let ((st (jit-new-state)))
             (jit-prolog st)
   (let ((v  (jit-arg st)))
             (jit-getarg-i st 'R0 v)
             (jit-addi st 'R1 'R0 1)
             (jit-retr st 'R1)
             (jit-emit st)))))
        (pointer->procedure
           unsigned-long
           ptr
          (list unsigned-long))))
*)

(*
 ("void", "jit_prolog", [("_jit", "state")], ("_jit_prolog", ["_jit"])),
 ("void", "jit_getarg_i", [("_jit", "state"), ("u", "gpr"), ("v", "noderef")],
  ("_jit_getarg_i", ["_jit", "u", "v"])),
 ("noderef", "jit_arg", [("_jit", "state")], ("_jit_arg", ["_jit"])),
 ("void", "jit_retr", [("_jit", "state"), ("u", "gpr")],
  ("_jit_retr", ["_jit", "u"])),
 ("noderef", "jit_new_node_www",
  [("_jit", "state"), ("c", "code"), ("u", "word"), ("v", "word"),
   ("w", "word")], ("_jit_new_node_www", ["_jit", "c", "u", "v", "w"]))];
 ("noderef", "jit_addr",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "gpr")],
  ("jit_new_node_www", ["_jit", "jit_code_addr", "u", "v", "w"])),
 ("noderef", "jit_addi",
  [("_jit", "state"), ("u", "gpr"), ("v", "gpr"), ("w", "word")],
  ("jit_new_node_www", ["_jit", "jit_code_addi", "u", "v", "w"])),
*)

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

val scm_rev =
   let fun iter acc = fn scm =>
      if (scm_eq_p (scm, scm_nil))
         then acc
         else
            let val hd = scm_car scm
                val tl = scm_cdr scm
            in iter (scm_cons(hd,acc)) tl
            end
   in iter scm_nil
   end
 
val scm_argtypes =
   let fun iter (pnms,atys) [] = (scm_rev pnms,scm_rev atys)
         | iter (pnms,atys) ((p as (v,t))::ps) = 
               iter (scm_cons (scm_symbol v, pnms),
                     scm_cons (scm_type t,   atys)) ps
   in iter (scm_nil, scm_nil)
   end

(* Having thought about this a bit more ... Since _we_ can compile
   bindings, why don't we just compile them for Guile?  In general,
   all we need to do is provide a _simple_ function call interface so
   that our hosts can define their calling conventions, and then we
   can build them a binary API for anything for which we have an
   intensional representation.

   Surprisingly, assembler is a much more concise and a simpler language
   for specifying such things. C is too ambiguous: what is the
   difference, _really,_ between a function which takes a structure
   containing an integer and a pointer to a pointer to a character as
   an argument, and a function which takes two arguments: an integer,
   and a pointer to an array of pointers to characters? It is easier
   to specify the semantics of an assembly language in terms of
   concrete contents of registers and memory locations etc. The
   semantics of C, on the other hand, seem inextricably wound up with
   how things are construed: for example an array is construed as a
   pointer when it appears fully specified in the argument to a
   function.

   But to be portable it needs to be an abstract assembler with some
   very simple and well-specified semantics. GNU lightning fulfils one
   out of these three requirements.
*)

fun tPair mkPair mkL mkR =
   fn (x1,x2) => mkPair (mkL x1,mkR x2)

fun tList mkCons mkNil mkElt =
   let fun mkTail [] = mkNil
         | mkTail (e::es) = mkCons (mkElt e, mkTail es)
   in mkTail
   end

fun mkPairJit mkPair =
   tPair (fn (x,x') => mkPair x x')

local
   open Jit
   val spr = ref ~1
   val max = ref ~1
in
   fun jit_alloc_stack (jit_, n) =
      let val sp = jit_allocai (jit_, n * wsz)
          val _ = max := sp + (n * wsz)
          val _ = spr := sp
      in ()
      end
   fun jit_lvar (jit_, n) =
      let val sp = !spr
          val sp' = sp + (n * wsz)
      in if sp' >= (!max)
            then raise Fail "Dude! You blew your stack!"
            else sp before spr := sp'
      end
   fun jit_lass (jit_, v, r0) =
      let val _ = jit_stxi (jit_, v, FP, r0)
      in ()
      end
   fun jit_lref (jit_, r0, v) =
      let val _ = jit_ldxi (jit_, r0, FP, v)
      in ()
      end
end

fun jit_binary jit_op jit_car jit_cdr jit_mkl jit_mkr =
   fn (jit_,r0,v1,v0) =>
          let open Jit
              val v0' = jit_lvar (jit_, 1)
              val v1' = jit_lvar (jit_, 1)
              val _ = jit_lass (jit_, v0', v0)
              val _ = jit_car (jit_, r0, v0)
              val _ = jit_mkl (jit_, v1, r0)
              val _ = jit_lass (jit_, v1', v1)
              val _ = jit_lref (jit_, v0, v0')
              val _ = jit_cdr (jit_, r0, v0)
              val _ = jit_mkr (jit_, v0, r0)
              val _ = jit_lref (jit_, v1, v1')
              val () = jit_op (jit_, r0, v1, v0)
          in ()
        end

fun jit_car (jit_,r1,r0) =
   let val _ = Jit.jit_ldxi (jit_, r1, r0, wsz * 0)
   in ()
   end

fun jit_cdr (jit_,r1,r0) =
   let val _ = Jit.jit_ldxi (jit_, r1, r0, wsz * 1)
   in ()
   end

fun jit_binopf funp =
   fn (jit_,r2,r1,r0) =>
       let open Jit
          val _ = jit_prepare (jit_)
          val _ = jit_pushargr (jit_, r1)
          val _ = jit_pushargr (jit_, r0)
          val _ = jit_finishi (jit_, funp)
          val _ = jit_retval (jit_, r2)
       in ()
       end

fun jit_unary jit_dst jit_con =
   fn (jit_,r0,v0) =>
      let open Jit
         val _ = jit_dst (jit_, r0, v0)
         val _ = jit_con (jit_, v0, r0)
         val _ = jit_movr (jit_, r0, v0)
      in ()
      end

fun jit_unopf funp =
   fn (jit_,r1,r0) =>
      let open Jit
         val _ = jit_prepare (jit_)
         val _ = jit_pushargr (jit_, r0)
         val _ = jit_finishi (jit_, funp)
         val _ = jit_retval (jit_, r1)
      in ()
      end

fun mkPairScm mkL mkR =
   jit_binary (jit_binopf scm_consp) jit_car jit_cdr mkL mkR

fun quote l =
   let fun iter r [] = r
         | iter r ((QUOTE s)::fs)     = iter (r^s) fs
         | iter r ((ANTIQUOTE s)::fs) = iter (r^s) fs
   in iter "" l
   end

val fPairScm  = quote `(mkPairJit mkPairScm)`;
val fListScm  = quote `(mkListJit mkConsScm mkNilScm)`;
val fStrScm   = quote `(mkStrJit  mkStrScm)`;
val fRealScm  = quote `(mkRealJit mkRealScm)`;
val fIntScm   = quote `(mkIntJit  mkIntScm)`;

val fPairCaml = quote `(mkPairJit mkPairCaml)`;
val fListCaml = quote `(mkListJit mkConsCaml mkNilCaml)`;
val fStrCaml  = quote `(mkStrJit  mkStrCaml)`;
val fRealCaml = quote `(mkRealJit mkRealCaml)`;
val fIntCaml  = quote `(mkIntJit  mkIntCaml)`;

val fList' = fn fname => fn s =>
                 quote ` let val fList' = ^(fname)
                         in fList'
                         end^(s)`;

val fPair' = fn fname => fn s => fn s' => 
                 quote ` (let val fPair' = ^(fname)
                          in fPair'
                          end^(s)^(s'))`;

val fStr'  = fn fname => " "^fname
val fReal' = fn fname => " "^fname
val fInt'  = fn fname => " "^fname;

fun mkformat fstr =
    fn v =>
       let val decl = quote `val ^(v) =^(fstr)`
           val ok = Meta.exec decl
       in if not ok
             then raise Fail ("Internal error compiling: "^fstr)
             else ()
       end;

val mkScmList = fList' fListScm
val mkScmPair = fPair' fPairScm
val mkScmStr  = fStr'  fStrScm
val mkScmReal = fReal' fRealScm
val mkScmInt  = fInt'  fIntScm

val mkCamlList = fList' fListCaml
val mkCamlPair = fPair' fPairCaml
val mkCamlStr  = fStr'  fStrCaml
val mkCamlReal = fReal' fRealCaml
val mkCamlInt  = fInt'  fIntCaml

datatype scm_atom =
   scm_INT of cptr
 | scm_STR of cptr
 | scm_GPR of cptr
 | scm_REAL of cptr
 | scm_WORD of cptr
 | scm_PTR of cptr
 | scm_CODE of cptr

datatype scm_tag =
   scm_tINT
 | scm_tSTR
 | scm_tGPR
 | scm_tREAL
 | scm_tWORD
 | scm_tPTR
 | scm_tCODE

datatype caml_atom =
   caml_INT of int
 | caml_STR of string
 | caml_GPR of int
 | caml_REAL of real
 | caml_WORD of word
 | caml_PTR of cptr
 | caml_CODE of string

datatype caml_tag =
   caml_tINT
 | caml_tSTR
 | caml_tGPR
 | caml_tREAL
 | caml_tWORD
 | caml_tPTR
 | caml_tCODE

datatype 'a tagType =
   ATOM of 'a
 | LST of 'a tagType list
 | FUN of 'a tagType -> 'a tagType
 | PR of 'a tagType * 'a tagType
 | TR of 'a tagType * 'a tagType * 'a tagType

datatype 'a typeExp = 
   tATOM of 'a
 | tLST of 'a typeExp
 | tFUN of 'a typeExp * 'a typeExp
 | tPR of 'a typeExp * 'a typeExp  
 | tTR of 'a typeExp * 'a typeExp * 'a typeExp  

val scm_Int = (fn n => scm_INT n,
           fn (scm_INT n) => n
            | _ => raise Fail "Int: internal error.",
           scm_tINT)

val scm_Real = (fn x => scm_REAL x,
           fn (scm_REAL x) => x
            | _ => raise Fail "Real: internal error.",
           scm_tREAL)

val scm_Word = (fn x => scm_WORD x,
           fn (scm_WORD x) => x
            | _ => raise Fail "Word: internal error.",
           scm_tWORD)

val scm_Str = (fn s => scm_STR s,
           fn (scm_STR s) => s
            | _ => raise Fail "Str: internal error.",
           scm_tSTR)

val scm_Gpr = (fn r => scm_GPR r,
           fn (scm_GPR r) => r
            | _ => raise Fail "Gpr: internal error.",
           scm_tGPR)

val scm_Ptr = (fn r => scm_PTR r,
           fn (scm_PTR r) => r
            | _ => raise Fail "Ptr: internal error.",
           scm_tPTR)

val scm_Code = (fn r => scm_CODE r,
           fn (scm_CODE r) => r
            | _ => raise Fail "Code: internal error.",
           scm_tCODE)

val caml_Int = (fn n => caml_INT n,
           fn (caml_INT n) => n
            | _ => raise Fail "Int: internal error.",
           caml_tINT)

val caml_Real = (fn x => caml_REAL x,
           fn (caml_REAL x) => x
            | _ => raise Fail "Real: internal error.",
           caml_tREAL)

val caml_Word = (fn x => caml_WORD x,
           fn (caml_WORD x) => x
            | _ => raise Fail "Word: internal error.",
           caml_tWORD)

val caml_Str = (fn s => caml_STR s,
           fn (caml_STR s) => s
            | _ => raise Fail "Str: internal error.",
           caml_tSTR)

val caml_Gpr = (fn r => caml_GPR r,
           fn (caml_GPR r) => r
            | _ => raise Fail "Gpr: internal error.",
           caml_tGPR)

val caml_Ptr = (fn r => caml_PTR r,
           fn (caml_PTR r) => r
            | _ => raise Fail "Ptr: internal error.",
           caml_tPTR)

val caml_Code = (fn r => caml_CODE r,
           fn (caml_CODE r) => r
            | _ => raise Fail "Code: internal error.",
           caml_tCODE)

fun List (T as (emb_T, proj_T, tE_T)) =
      (fn l => LST (List.map emb_T l),
       fn (LST l) => List.map proj_T l
        | _ => raise Fail "List: internal error.",
       tLST tE_T)

fun Pair (T as (emb_T, proj_T, tE_T),
          T' as (emb_T', proj_T', tE_T')) =
      (fn (v,v') => PR (emb_T v,emb_T' v'),
       fn (PR (e,e')) => (proj_T e,proj_T' e')
        | _ => raise Fail "Pair: internal error.",
       tPR (tE_T,tE_T'))

fun Triple (T as (emb_T, proj_T, tE_T),
            T' as (emb_T', proj_T', tE_T'),
            T'' as (emb_T'', proj_T'', tE_T'')) =
      (fn (v,v',v'') => TR (emb_T v,emb_T' v',emb_T'' v''),
       fn (TR (e,e',e'')) => (proj_T e,proj_T' e',proj_T'' e'')
        | _ => raise Fail "Pair: internal error.",
       tTR (tE_T,tE_T',tE_T''));

infixr 5 -->;

fun (T as (emb_T, proj_T, tE_T)) --> 
    (T' as (emb_T', proj_T', tE_T')) =
      (fn f => FUN (fn t => emb_T' (f (proj_T t))),
       fn (FUN f) => (fn x => (proj_T' (f (emb_T x))))
        | _ => raise Fail "-->: internal error.",
       tFUN (tE_T,tE_T'));

exception nonSubtype

fun lookup_coerce [] tE1 tE2 =
      raise nonSubtype
  | lookup_coerce ((tATOM t, tATOM t',t2t')::Others) tE1 tE2 =
      if t = tE1 andalso t' = tE2 
         then t2t'
         else lookup_coerce Others tE1 tE2;

fun univ_coerce c1 (tFUN(tE1_T1,tE2_T1))
                   (tFUN(tE1_T2,tE2_T2)) (FUN v) =
      FUN(fn x => univ_coerce c1 tE2_T1 tE2_T2
                 (v (univ_coerce c1 tE1_T2 tE1_T1 x)))
  | univ_coerce c1 (tLST(tE_T1)) (tLST(tE_T2)) (LST v) =
      LST(List.map (univ_coerce c1 tE_T1 tE_T2) v)
  | univ_coerce c1 (tPR(tE1_T1,tE2_T1))
                   (tPR(tE1_T2,tE2_T2)) (PR (v,v')) =
      PR(univ_coerce c1 tE1_T1 tE1_T2 v,
         univ_coerce c1 tE2_T1 tE2_T2 v')
  | univ_coerce c1 (tTR(tE1_T1,tE2_T1,tE3_T1))
                   (tTR(tE1_T2,tE2_T2,tE3_T2)) (TR (v,v',v'')) =
      TR(univ_coerce c1 tE1_T1 tE1_T2 v,
         univ_coerce c1 tE2_T1 tE2_T2 v',
         univ_coerce c1 tE3_T1 tE3_T2 v'')
  | univ_coerce c1 x y v =
      if x = y 
         then v
         else (lookup_coerce c1 x y) v;

fun coerce c1 (T1 as (emb_T1, proj_T1, tE_T1))
              (T2 as (emb_T2, proj_T2, tE_T2)) v =
      proj_T2 (univ_coerce c1 tE_T1 tE_T2 (emb_T1 v));

val scm_C =
         [(tATOM scm_tPTR,
           tATOM scm_tWORD,
           fn (ATOM (scm_PTR x)) => ATOM (scm_WORD (scm_pointer_to_bytevector
                                           (x,scm_from_long 4,
                                            scm_undefined,
                                            scm_undefined)))
            | _ => raise Fail "coerce: PTR->WORD: Internal error"),
          (tATOM scm_tINT,
           tATOM scm_tWORD,
           fn (ATOM (scm_INT x)) =>
               ATOM (scm_WORD(scm_long_bytevector x))
            | _ => raise Fail "coerce: INT->WORD: Internal error"),
          (tATOM scm_tCODE,
           tATOM scm_tWORD,
           fn (ATOM (scm_CODE x)) => ATOM(scm_WORD (scm_lookup_code (sml_string x)))
            | _ => raise Fail "coerce: CODE->WORD: Internal error")];

val caml_C =
         [(tATOM caml_tPTR,
           tATOM caml_tWORD,
           fn (ATOM (caml_PTR x)) => ATOM (caml_WORD (Ffi.svec_getcptrword x))
            | _ => raise Fail "coerce: PTR->WORD: Internal error"),
          (tATOM caml_tINT,
           tATOM caml_tWORD,
           fn (ATOM (caml_INT x)) =>
               ATOM (caml_WORD(caml_long_bytevector x))
            | _ => raise Fail "coerce: INT->WORD: Internal error"),
          (tATOM caml_tCODE,
           tATOM caml_tWORD,
           fn (ATOM (caml_CODE x)) => ATOM(caml_WORD (lookup_code (sml_string x)))
            | _ => raise Fail "coerce: CODE->WORD: Internal error")];

fun defwrapper l =
   let fun iter acc [] = acc
         | iter acc ((retval, name, args, (call,_))::rest) =
      let val rvt = scm_type retval
          val (ans,ats) = scm_argtypes args
          val cln = scm_string call
          val nam = scm_symbol name
      in iter (scm_qlToString `(define ^(nam) (list (quote ^(rvt)) (quote ^(ats))))`::acc) rest
      end
   in
      List.rev (iter [] l)
   end;

val mknodes' = defwrapper mknodes;
