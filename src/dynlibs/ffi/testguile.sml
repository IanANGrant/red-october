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

