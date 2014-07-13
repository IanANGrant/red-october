val libmffi =
       Dynlib.dlopen {lib = "./libmffi.so",
                       flag = Dynlib.RTLD_LAZY,
                       global = false };

val liblightning =
       Dynlib.dlopen {lib = "/home/ian3/usr/lib/liblightning.so",
                       flag = Dynlib.RTLD_LAZY, 
                       global = false };

datatype jit_gpr_t = 
   R0 | R1 | R2 | V0 | V1 | V2 | R of int | V of int

datatype jit_fpr_t = 
   F0 | F1 | F2 | F3 | F4 | F5 | F of int

   datatype jit_sign =
      Unsigned
    | Signed
   datatype jit_typed_value =
      State of Word8Vector.vector
    | Node of Word8Vector.vector
    | String of Word8Vector.vector
    | Code of Word8Vector.vector
    | GPRegister of Word8Vector.vector
    | FPRegister of Word8Vector.vector
    | Label of Word8Vector.vector
    | Character of Word8.word * jit_sign
    | Short of Word8Vector.vector * jit_sign
    | Integer of Word8Vector.vector * jit_sign
    | Long of Word8Vector.vector * jit_sign
    | Float of Word8Vector.vector
    | Double of Word8Vector.vector
    | Boolean of Word8.word
    | Pointer of Word8Vector.vector
    | Structure of Word8Vector.vector list

local
   val argv0 = "/home/ian3/usr/lib/mosml/camlrunm";
   val svec = Ffi.svec_from_string argv0
   val cp = Ffi.svec_getcptrvalue (Ffi.svec_getbuffercptr svec)
in val argv0 = Pointer cp
   val NULLp = Pointer (Ffi.svec_getcptrvalue Ffi.NULL);
end;

local open Ffi
   val jit_r_type = Function(Integer(Signed,Int), SOME FFI_TYPE_INT)
   fun mkargs n_ = svec_setvecword (Word.fromInt n_)
   fun mkretval v = svec_getvecword v
   val jit_r_ = Dynlib.cptr (Dynlib.dlsym libmffi "jit_r_")
in
   val jit_r = ffi_trampoline "jit_r" jit_r_ jit_r_type mkargs mkretval
end;

local open Ffi
   val jit_v_type = Function(Integer(Signed,Int), SOME FFI_TYPE_INT)
   fun mkargs n_ = svec_setvecword (Word.fromInt n_)
   fun mkretval v = svec_getvecword v
   val jit_v_ = Dynlib.cptr (Dynlib.dlsym libmffi "jit_v_")
in
   val jit_v = ffi_trampoline "jit_v" jit_v_ jit_v_type mkargs mkretval
end;

val R_NUM = Word.toInt (Ffi.jit_get_constant("R_NUM"));
val V_NUM = Word.toInt (Ffi.jit_get_constant("V_NUM"));

val MAX_R = R_NUM - 1;
val MAX_V = V_NUM - 1;

val R3 = R(3)
val V3 = V(3)

val F6 = F(6);
val F7 = F(7);

fun jit_gpr (R n) = 
       if n > MAX_R
          then raise Fail
                 ("Invalid R(n) register. (n>"
                     ^(Int.toString (MAX_R))^")")
          else jit_r(n)
  | jit_gpr R0 = jit_r(0)
  | jit_gpr R1 = jit_r(1)
  | jit_gpr R2 = jit_r(2)
  | jit_gpr (V n) = 
       if n > MAX_V
          then raise Fail
                 ("Invalid V(n) register. (n>"
                     ^(Int.toString (MAX_V))^")")
          else jit_v(n)
  | jit_gpr V0 = jit_v(0)
  | jit_gpr V1 = jit_v(1)
  | jit_gpr V2 = jit_v(2);

local open Ffi
in
   val WORDSIZE = jit_get_wordsize();
   val BYTE_ORDER = jit_get_byteorder();
   val LITTLE_ENDIAN = jit_get_little_endian();
   val BIG_ENDIAN = jit_get_big_endian();
end

local
   val init_jit_type =
          Ffi.Function(Ffi.Pointer(Ffi.Const(Ffi.Character(Ffi.Signed))), SOME Ffi.FFI_TYPE_VOID);
   fun mkargs (Pointer p) = p
     | mkargs _ = raise Fail "Jit.init_jit: argument type mismatch: expected (State)."
   fun mkretval _ = ()
   val init_jit_ = Dynlib.cptr (Dynlib.dlsym liblightning "init_jit")
in
   val init_jit = Ffi.ffi_trampoline "init_jit" init_jit_
                      init_jit_type mkargs mkretval
end;

local open Ffi
   val finish_jit_type = Function(Void, SOME FFI_TYPE_VOID);
   fun mkargs () = Word8Vector.fromList []
   fun mkretval _ = ()
   val finish_jit_ = Dynlib.cptr (Dynlib.dlsym liblightning "finish_jit")
in
   val finish_jit = ffi_trampoline "finish_jit" finish_jit_
                          finish_jit_type mkargs mkretval
end;

local open Ffi
   val jit_new_state_type = Function(Void, SOME FFI_TYPE_POINTER)
   fun mkargs () = Word8Vector.fromList []
   fun mkretval v = (State v)
   val jit_new_state_ = Dynlib.cptr (Dynlib.dlsym liblightning "jit_new_state")
in
   val jit_new_state = ffi_trampoline "jit_new_state" jit_new_state_
                               jit_new_state_type mkargs mkretval
end;

local open Ffi
   val jit_clear_state_type = Function(Pointer(Structure([])), SOME FFI_TYPE_VOID)
   fun mkargs (State jit_) = jit_
     | mkargs _ = raise Fail "Jit.jit_clear_state: argument type mismatch: expected (State)."
   fun mkretval _ = ()
   val jit_clear_state = Dynlib.cptr (Dynlib.dlsym liblightning "_jit_clear_state")
in
   val jit_clear_state = ffi_trampoline "jit_clear_state" jit_clear_state
                               jit_clear_state_type mkargs mkretval
end;

local open Ffi
   val jit_destroy_state_type = Function(Pointer(Structure([])), SOME FFI_TYPE_VOID)
   fun mkargs (State jit_) = jit_
     | mkargs _ = raise Fail "Jit.jit_destroy_state: argument type mismatch: expected (State)."
   fun mkretval _ = ()
   val jit_destroy_state = Dynlib.cptr (Dynlib.dlsym liblightning "_jit_destroy_state")
in
   val jit_destroy_state = ffi_trampoline "jit_destroy_state" jit_destroy_state
                                        jit_destroy_state_type mkargs mkretval
end;

local open Ffi
   val jit_label_type = Function(Pointer(Structure([])), SOME FFI_TYPE_POINTER)
   fun mkargs (State jit_) = jit_
     | mkargs _ = raise Fail "Jit.jit_label: argument type mismatch: expected (State)."
   fun mkretval v = Node v
   val jit_label = Dynlib.cptr (Dynlib.dlsym liblightning "_jit_label")
in
   val jit_label = ffi_trampoline "jit_label" jit_label
                           jit_label_type mkargs mkretval
end;

local open Ffi
   val jit_arg_type = Function(Pointer(Structure([])), SOME FFI_TYPE_POINTER)
   fun mkargs (State jit_) = jit_
     | mkargs _ = raise Fail "Jit.jit_arg: argument type mismatch: expected (State)."
   fun mkretval v = Node v
   val jit_arg = Dynlib.cptr (Dynlib.dlsym liblightning "_jit_arg")
in
   val jit_arg = ffi_trampoline "jit_arg" jit_arg jit_arg_type mkargs mkretval
end;

local open Ffi
   val jit_prolog_type = Function(Pointer(Structure([])), SOME FFI_TYPE_VOID)
   fun mkargs (State jit_) = jit_
     | mkargs _ = raise Fail "Jit.jit_prolog: argument type mismatch: expected (State)."
   fun mkretval _ = ()
   val jit_prolog = Dynlib.cptr (Dynlib.dlsym liblightning "_jit_prolog")
in
   val jit_prolog = ffi_trampoline "jit_prolog" jit_prolog
                 jit_prolog_type mkargs mkretval
end;

local open Ffi
   val jit_prepare_type = Function(Pointer(Structure([])), SOME FFI_TYPE_VOID)
   fun mkargs (State jit_) = jit_
     | mkargs _ = raise Fail "Jit.jit_prepare: argument type mismatch: expected (State)."
   fun mkretval _ = ()
   val jit_prepare = Dynlib.cptr (Dynlib.dlsym liblightning "_jit_prepare")
in
   val jit_prepare = ffi_trampoline "jit_prepare" jit_prepare
                 jit_prepare_type mkargs mkretval
end;

local open Ffi
   val jit_disassemble_type = Function(Pointer(Structure([])), SOME FFI_TYPE_VOID)
   fun mkargs (State jit_) = jit_
     | mkargs _ = raise Fail "Jit.jit_disassemble: argument type mismatch: expected (State)."
   fun mkretval _ = ()
   val jit_disassemble = Dynlib.cptr (Dynlib.dlsym liblightning "_jit_disassemble")
in
   val jit_disassemble = ffi_trampoline "jit_disassemble" jit_disassemble
                              jit_disassemble_type mkargs mkretval
end;

local open Ffi
   val jit_pushargr_type = 
              Function(Structure([Pointer(Structure[]),
                                  Integer(Unsigned,Int)]),
                       SOME FFI_TYPE_VOID)
   fun mkargsr (State jit_, gpr_) = 
          let val (argsv,svec) = mkargssvec [jit_ , svec_setvecword (jit_gpr gpr_)] 
          in argsv
          end
     |  mkargsr _ = raise Fail "Jit:jit_pushargr: argument type mismatch: expected (State,Gpr)."
   fun mkargsi (State jit_, word_) = 
          let val (argsv,svec) = mkargssvec [jit_ , Ffi.svec_setvecword word_] 
          in argsv
          end
     |  mkargsi _ = raise Fail "Jit:jit_pushargi: argument type mismatch: expected (State,Word)."
   fun mkretval _ = ()
   val jit_pushargr_sym = "_jit_pushargr"
   val jit_pushargr_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_pushargr_sym)
   val jit_pushargi_sym = "_jit_pushargi"
   val jit_pushargi_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_pushargi_sym)
in
   val jit_pushargr = ffi_trampoline "jit_pushargr" jit_pushargr_
                         jit_pushargr_type mkargsr mkretval
   val jit_pushargi = ffi_trampoline "jit_pushargi" jit_pushargi_
                         jit_pushargr_type mkargsi mkretval
end;

local
   val jit_finishi_type = 
              Ffi.Function(Ffi.Structure([Ffi.Pointer(Ffi.Structure[]),
                                  Ffi.Pointer(Ffi.Structure[])]),
                       SOME Ffi.FFI_TYPE_POINTER)
   fun mkargs (State jit_, Pointer p) =
          let val (argsv,svec) = Ffi.mkargssvec [jit_ , p]
          in argsv
          end
     |  mkargs _ = raise Fail ("Jit:jit_finishi: argument type mismatch: expected"^
                                  " (State,Code,Pointer).")
   fun mkretval v = Node v
   val jit_finishi_sym = "_jit_finishi"
   val jit_finishi_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_finishi_sym)
in
   val jit_finishi = Ffi.ffi_trampoline "jit_finishi" jit_finishi_
                         jit_finishi_type mkargs mkretval
end;

local
   val jit_finishr_type = 
              Ffi.Function(Ffi.Structure([Ffi.Pointer(Ffi.Structure[]),
                                          Ffi.Integer(Ffi.Unsigned,Ffi.Int)]),
                       SOME Ffi.FFI_TYPE_POINTER)
   fun mkargs (State jit_, gpr_) =
          let val (argsv,svec) = Ffi.mkargssvec [jit_ , Ffi.svec_setvecword (jit_gpr gpr_)]
          in argsv
          end
     |  mkargs _ = raise Fail ("Jit:jit_finishr: argument type mismatch: expected"^
                                  " (State,Code,word).")
   fun mkretval v = Node v
   val jit_finishr_sym = "_jit_finishr"
   val jit_finishr_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_finishr_sym)
in
   val jit_finishr = Ffi.ffi_trampoline "jit_finishr" jit_finishr_
                         jit_finishr_type mkargs mkretval
end;

fun only64 fnm thunk =
   if WORDSIZE = 64
      then thunk()
      else raise Fail (fnm^": only on 64 bit machines.")

local open Ffi
   val jit_getarg_type = 
              Function(Structure([Pointer(Structure[]),
                                  Integer(Unsigned,Int),
                                  Pointer(Structure[])]),
                       SOME FFI_TYPE_VOID)
   fun mkargs (State jit_, gpr_, Node node_) = 
          let val (argsv,svec) = mkargssvec [jit_ , svec_setvecword (jit_gpr gpr_), node_] 
          in argsv
          end
     |  mkargs _ = raise Fail "Jit:jit_getarg: argument type mismatch: expected (State,Gpr,Node)."
   fun mkretval _ = ()
   val jit_getarg_sym = "_jit_getarg_"^(if WORDSIZE = 32 then "i" else "l")
   val jit_getarg_c_sym = "_jit_getarg_c"
   val jit_getarg_uc_sym = "_jit_getarg_uc"
   val jit_getarg_s_sym = "_jit_getarg_s"
   val jit_getarg_us_sym = "_jit_getarg_us"
   val jit_getarg_i_sym = "_jit_getarg_i"
   val jit_getarg_ui_sym = "_jit_getarg_ui"
   val jit_getarg_l_sym = "_jit_getarg_l"
   val jit_getarg_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_getarg_sym)
   val jit_getarg_c_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_getarg_c_sym)
   val jit_getarg_uc_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_getarg_uc_sym)
   val jit_getarg_s_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_getarg_s_sym)
   val jit_getarg_us_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_getarg_us_sym)
   val jit_getarg_i_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_getarg_i_sym)
in
   val jit_getarg = ffi_trampoline "jit_getarg" jit_getarg_
                         jit_getarg_type mkargs mkretval
   val jit_getarg_c = ffi_trampoline "jit_getarg_c" jit_getarg_c_
                         jit_getarg_type mkargs mkretval
   val jit_getarg_uc = ffi_trampoline "jit_getarg_uc" jit_getarg_uc_
                         jit_getarg_type mkargs mkretval
   val jit_getarg_s = ffi_trampoline "jit_getarg_s" jit_getarg_s_
                         jit_getarg_type mkargs mkretval
   val jit_getarg_us = ffi_trampoline "jit_getarg_us" jit_getarg_us_
                         jit_getarg_type mkargs mkretval
   val jit_getarg_i = ffi_trampoline "jit_getarg_i" jit_getarg_i_
                         jit_getarg_type mkargs mkretval
   fun jit_getarg_ui (jit,arg,node) = only64 "jit_getarg_ui" (fn () => 
                         ffi_trampoline "jit_getarg_ui" 
                         (Dynlib.cptr (Dynlib.dlsym liblightning jit_getarg_ui_sym))
                         jit_getarg_type mkargs mkretval (jit,arg,node))
   fun jit_getarg_l (jit,arg,node) = only64 "jit_getarg_l" (fn () => 
                         ffi_trampoline "jit_getarg_l" 
                         (Dynlib.cptr (Dynlib.dlsym liblightning jit_getarg_l_sym))
                         jit_getarg_type mkargs mkretval (jit,arg,node))
end;

local open Ffi
   val jit_retval_type = 
              Function(Structure([Pointer(Structure[]),
                                  Integer(Unsigned,Int)]),
                       SOME FFI_TYPE_VOID)
   fun mkargs (State jit_, gpr_) = 
          let val (argsv,svec) = mkargssvec [jit_ , svec_setvecword (jit_gpr gpr_)] 
          in argsv
          end
     |  mkargs _ = raise Fail "Jit:jit_retval: argument type mismatch: expected (State,Gpr)."
   fun mkretval _ = ()
   val jit_retval_sym = "_jit_retval_"^(if WORDSIZE = 32 then "i" else "l")
   val jit_retval_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_retval_sym)
   val jit_retval_c_ =  Dynlib.cptr (Dynlib.dlsym liblightning "_jit_retval_c")
   val jit_retval_uc_ =  Dynlib.cptr (Dynlib.dlsym liblightning "_jit_retval_uc")
   val jit_retval_s_ =  Dynlib.cptr (Dynlib.dlsym liblightning "_jit_retval_s")
   val jit_retval_us_ =  Dynlib.cptr (Dynlib.dlsym liblightning "_jit_retval_us")
   val jit_retval_i_ =  Dynlib.cptr (Dynlib.dlsym liblightning "_jit_retval_i")
   val jit_retval_ui_ = fn () => Dynlib.cptr (Dynlib.dlsym liblightning "_jit_retval_ui")
   val jit_retval_l_ = fn () => Dynlib.cptr (Dynlib.dlsym liblightning "_jit_retval_l")
in
   val jit_retval = ffi_trampoline "jit_retval" jit_retval_
                         jit_retval_type mkargs mkretval
   val jit_retval_c = ffi_trampoline "jit_retval_c" jit_retval_c_
                         jit_retval_type mkargs mkretval
   val jit_retval_uc = ffi_trampoline "jit_retval_uc" jit_retval_uc_
                         jit_retval_type mkargs mkretval
   val jit_retval_s = ffi_trampoline "jit_retval_s" jit_retval_s_
                         jit_retval_type mkargs mkretval
   val jit_retval_us = ffi_trampoline "jit_retval_us" jit_retval_us_
                         jit_retval_type mkargs mkretval
   val jit_retval_i = ffi_trampoline "jit_retval_i" jit_retval_i_
                         jit_retval_type mkargs mkretval
   fun jit_retval_ui (jit,arg) = only64 "jit_retval_ui" (fn () => 
                         ffi_trampoline "jit_retval_ui" (jit_retval_ui_())
                         jit_retval_type mkargs mkretval (jit,arg))
   fun jit_retval_l (jit,arg) = only64 "jit_retval_l" (fn () => 
                         ffi_trampoline "jit_retval_l" (jit_retval_l_())
                         jit_retval_type mkargs mkretval (jit,arg))
end;

local open Ffi
   val jit_patch_type = Function(Structure([Pointer(Structure([])),
                                            Pointer(Structure([]))]), SOME FFI_TYPE_VOID)
   fun mkargs (State jit_, Node node_) =
          let val (argsv,svec) = mkargssvec [jit_ , node_] 
          in argsv
          end
     |  mkargs _ = raise Fail "Jit:jit_patch: argument type mismatch: expected (State,Node)."
   fun mkretval _ = ()
   val jit_patch = Dynlib.cptr (Dynlib.dlsym liblightning "_jit_patch")
in
   val jit_patch = ffi_trampoline "jit_patch" jit_patch jit_patch_type mkargs mkretval
end;

local open Ffi
   val jit_patch_at_type = Function(Structure([Pointer(Structure([])),
                                               Pointer(Structure([])),
                                               Pointer(Structure([]))]), SOME FFI_TYPE_VOID)
   fun mkargs (State jit_, Node node1_, Node node2_) =
          let val (argsv,svec) = mkargssvec [jit_ , node1_, node2_]
          in argsv
          end
     |  mkargs _ = raise Fail "Jit:jit_patch_at: argument type mismatch: expected (State,Node,Node)."
   fun mkretval _ = ()
   val jit_patch_at = Dynlib.cptr (Dynlib.dlsym liblightning "_jit_patch_at")
in
   val jit_patch_at = ffi_trampoline "jit_patch_at" jit_patch_at jit_patch_at_type mkargs mkretval
end;

local open Ffi
   val jit_ret_type = Function(Pointer(Structure([])), SOME FFI_TYPE_VOID)
   fun mkargs (State jit_) = jit_
     | mkargs _ = raise Fail "Jit.ret: argument type mismatch: expected (State)."
   fun mkretval _ = ()
   val jit_ret = Dynlib.cptr (Dynlib.dlsym liblightning "_jit_ret")
in
   val jit_ret = ffi_trampoline "jit_ret" jit_ret
                 jit_ret_type mkargs mkretval
end;

local open Ffi
   val jit_retr_type = Function(Structure([Pointer(Structure([])),
                                           Integer(Unsigned,Int)]), SOME FFI_TYPE_VOID)
   fun mkargs (State jit_, gpr_) =
          let val (argsv,svec) = mkargssvec [jit_ , svec_setvecword (jit_gpr gpr_)]
          in argsv
          end
     | mkargs _ = raise Fail "Jit:jit_retr:  argument type mismatch: expected (State,gpr)"
   fun mkretval _ = ()
   val jit_retr = Dynlib.cptr (Dynlib.dlsym liblightning "_jit_retr")
in
   val jit_retr = ffi_trampoline "jit_retr" jit_retr jit_retr_type mkargs mkretval
end;

local open Ffi
   val jit_reti_type = Function(Structure([Pointer(Structure([])),
                                           Integer(Unsigned,Int)]), SOME FFI_TYPE_VOID)
   fun mkargs (State jit_, word_) =
          let val (argsv,svec) = mkargssvec [jit_ , svec_setvecword word_]
          in argsv
          end
     | mkargs _ = raise Fail "Jit:jit_reti:  argument type mismatch: expected (State,word)"
   fun mkretval _ = ()
   val jit_reti = Dynlib.cptr (Dynlib.dlsym liblightning "_jit_reti")
in
   val jit_reti = ffi_trampoline "jit_reti" jit_reti jit_reti_type mkargs mkretval
end;

local
   val jit_new_node_pw_type = 
              Ffi.Function(Ffi.Structure([Ffi.Pointer(Ffi.Structure[]),
                                  Ffi.Integer(Ffi.Unsigned,Ffi.Int),
                                  Ffi.Pointer(Ffi.Structure[]),
                                  Ffi.Integer(Ffi.Unsigned,Ffi.Int)]),
                       SOME Ffi.FFI_TYPE_POINTER)
   fun mkargs (State jit_, Code code_, Pointer pointer_, word1_) =
          let val (argsv,svec) = Ffi.mkargssvec [jit_ , code_, pointer_,
                                             Ffi.svec_setvecword word1_]
          in argsv
          end
     |  mkargs _ = raise Fail ("Jit:jit_new_node_pw: argument type mismatch: expected"^
                                  " (State,Code,Pointer,word).")
   fun mkretval v = Node v
   val jit_new_node_pw_sym = "_jit_new_node_pw"
   val jit_new_node_pw_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_new_node_pw_sym)
in
   val jit_new_node_pw = Ffi.ffi_trampoline "jit_new_node_pw" jit_new_node_pw_
                         jit_new_node_pw_type mkargs mkretval
end;

local
   val jit_new_node_wp_type = 
              Ffi.Function(Ffi.Structure([Ffi.Pointer(Ffi.Structure[]),
                                  Ffi.Integer(Ffi.Unsigned,Ffi.Int),
                                  Ffi.Integer(Ffi.Unsigned,Ffi.Int),
                                  Ffi.Pointer(Ffi.Structure[])]),
                       SOME Ffi.FFI_TYPE_POINTER)
   fun mkargs (State jit_, Code code_, word1_, Pointer pointer_) =
          let val (argsv,svec) = Ffi.mkargssvec [jit_ , code_, 
                                        Ffi.svec_setvecword word1_, pointer_]
          in argsv
          end
     |  mkargs _ = raise Fail ("Jit:jit_new_node_wp: argument type mismatch: expected"^
                                  " (State,Code,word,Pointer).")
   fun mkretval v = Node v
   val jit_new_node_wp_sym = "_jit_new_node_wp"
   val jit_new_node_wp_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_new_node_wp_sym)
in
   val jit_new_node_wp = Ffi.ffi_trampoline "jit_new_node_wp" jit_new_node_wp_
                         jit_new_node_wp_type mkargs mkretval
end;

local
   val jit_new_node_pww_type = 
              Ffi.Function(Ffi.Structure([Ffi.Pointer(Ffi.Structure[]),
                                  Ffi.Integer(Ffi.Unsigned,Ffi.Int),
                                  Ffi.Pointer(Ffi.Structure[]),
                                  Ffi.Integer(Ffi.Unsigned,Ffi.Int),
                                  Ffi.Integer(Ffi.Unsigned,Ffi.Int)]),
                       SOME Ffi.FFI_TYPE_POINTER)
   fun mkargs (State jit_, Code code_, Pointer pointer_, word1_, word2_) =
          let val (argsv,svec) = Ffi.mkargssvec [jit_ , code_, pointer_,
                                             Ffi.svec_setvecword word1_,
                                             Ffi.svec_setvecword word2_]
          in argsv
          end
     |  mkargs _ = raise Fail ("Jit:jit_new_node_pww: argument type mismatch: expected"^
                                  " (State,Code,Pointer,word,word).")
   fun mkretval v = Node v
   val jit_new_node_pww_sym = "_jit_new_node_pww"
   val jit_new_node_pww_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_new_node_pww_sym)
in
   val jit_new_node_pww = Ffi.ffi_trampoline "jit_new_node_pww" jit_new_node_pww_
                         jit_new_node_pww_type mkargs mkretval
end;

local 
   val jit_blti_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "blti")))
   val jit_blti_u_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "blti_u")))
   val jit_blei_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "blei")))
   val jit_blei_u_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "blei_u")))
   val jit_bgti_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "bgti")))
   val jit_bgti_u_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "bgti_u")))
   val jit_bgei_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "bgei")))
   val jit_bgei_u_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "bgei_u")))
   val jit_beqi_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "beqi")))
   val jit_bnei_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "bnei")))
   val jit_bltr_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "bltr")))
   val jit_bltr_u_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "bltr_u")))
   val jit_bler_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "bler")))
   val jit_bler_u_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "bler_u")))
   val jit_bgtr_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "bgtr")))
   val jit_bgtr_u_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "bgtr_u")))
   val jit_bger_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "bger")))
   val jit_bger_u_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "bger_u")))
   val jit_beqr_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "beqr")))
   val jit_bner_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "bner")))
in
   fun jit_bgti (jit, gpr, n) = 
          jit_new_node_pww (jit, jit_bgti_, Pointer Ffi.NULLvec, jit_gpr gpr, Word.fromInt n)
   fun jit_bgti_u (jit, gpr, n) = 
          jit_new_node_pww (jit, jit_bgti_u_, Pointer Ffi.NULLvec, jit_gpr gpr, Word.fromInt n)
   fun jit_bgei (jit, gpr, n) = 
          jit_new_node_pww (jit, jit_bgei_, Pointer Ffi.NULLvec, jit_gpr gpr, Word.fromInt n)
   fun jit_bgei_u (jit, gpr, n) = 
          jit_new_node_pww (jit, jit_bgei_u_, Pointer Ffi.NULLvec, jit_gpr gpr, Word.fromInt n)
   fun jit_blti (jit, gpr, n) = 
          jit_new_node_pww (jit, jit_blti_, Pointer Ffi.NULLvec, jit_gpr gpr, Word.fromInt n)
   fun jit_blti_u (jit, gpr, n) = 
          jit_new_node_pww (jit, jit_blti_u_, Pointer Ffi.NULLvec, jit_gpr gpr, Word.fromInt n)
   fun jit_blei (jit, gpr, n) = 
          jit_new_node_pww (jit, jit_blei_, Pointer Ffi.NULLvec, jit_gpr gpr, Word.fromInt n)
   fun jit_blei_u (jit, gpr, n) = 
          jit_new_node_pww (jit, jit_blei_u_, Pointer Ffi.NULLvec, jit_gpr gpr, Word.fromInt n)
   fun jit_beqi (jit, gpr, n) = 
          jit_new_node_pww (jit, jit_beqi_, Pointer Ffi.NULLvec, jit_gpr gpr, Word.fromInt n)
   fun jit_bnei (jit, gpr, n) = 
          jit_new_node_pww (jit, jit_bnei_, Pointer Ffi.NULLvec, jit_gpr gpr, Word.fromInt n)
   fun jit_bgtr (jit, gpr, gpr2) = 
          jit_new_node_pww (jit, jit_bgtr_, Pointer Ffi.NULLvec, jit_gpr gpr, jit_gpr gpr2)
   fun jit_bgtr_u (jit, gpr, gpr2) = 
          jit_new_node_pww (jit, jit_bgtr_u_, Pointer Ffi.NULLvec, jit_gpr gpr, jit_gpr gpr2)
   fun jit_bger (jit, gpr, gpr2) = 
          jit_new_node_pww (jit, jit_bger_, Pointer Ffi.NULLvec, jit_gpr gpr, jit_gpr gpr2)
   fun jit_bger_u (jit, gpr, gpr2) = 
          jit_new_node_pww (jit, jit_bger_u_, Pointer Ffi.NULLvec, jit_gpr gpr, jit_gpr gpr2)
   fun jit_bltr (jit, gpr, gpr2) = 
          jit_new_node_pww (jit, jit_bltr_, Pointer Ffi.NULLvec, jit_gpr gpr, jit_gpr gpr2)
   fun jit_bltr_u (jit, gpr, gpr2) = 
          jit_new_node_pww (jit, jit_bltr_u_, Pointer Ffi.NULLvec, jit_gpr gpr, jit_gpr gpr2)
   fun jit_bler (jit, gpr, gpr2) = 
          jit_new_node_pww (jit, jit_bler_, Pointer Ffi.NULLvec, jit_gpr gpr, jit_gpr gpr2)
   fun jit_bler_u (jit, gpr, gpr2) = 
          jit_new_node_pww (jit, jit_bler_u_, Pointer Ffi.NULLvec, jit_gpr gpr, jit_gpr gpr2)
   fun jit_beqr (jit, gpr, gpr2) = 
          jit_new_node_pww (jit, jit_beqr_, Pointer Ffi.NULLvec, jit_gpr gpr, jit_gpr gpr2)
   fun jit_bner (jit, gpr, gpr2) = 
          jit_new_node_pww (jit, jit_bner_, Pointer Ffi.NULLvec, jit_gpr gpr, jit_gpr gpr2)
end;

local open Ffi
   val jit_new_node_www_type = 
              Function(Structure([Pointer(Structure[]),
                                  Integer(Unsigned,Int),
                                  Integer(Unsigned,Int),
                                  Integer(Unsigned,Int),
                                  Integer(Unsigned,Int)]),
                       SOME FFI_TYPE_POINTER)
   fun mkargs (State jit_, Code code_, word1_, word2_, word3_) =
          let val (argsv,svec) = mkargssvec [jit_ , code_,
                                                    svec_setvecword word1_,
                                                    svec_setvecword word2_,
                                                    svec_setvecword word3_]
          in argsv
          end
     |  mkargs _ = raise Fail ("Jit:jit_new_node_www: argument type mismatch: expected"^
                                  " (State,Code,word,word,word).")
   fun mkretval v = Node v
   val jit_new_node_www_sym = "_jit_new_node_www"
   val jit_new_node_www_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_new_node_www_sym)
in
   val jit_new_node_www = ffi_trampoline "jit_new_node_www" jit_new_node_www_
                         jit_new_node_www_type mkargs mkretval
end;

local 
   val jit_new_node_wwp_type = 
              Ffi.Function(Ffi.Structure([Ffi.Pointer(Ffi.Structure[]),
                                  Ffi.Integer(Ffi.Unsigned,Ffi.Int),
                                  Ffi.Integer(Ffi.Unsigned,Ffi.Int),
                                  Ffi.Integer(Ffi.Unsigned,Ffi.Int),
                                  Ffi.Integer(Ffi.Unsigned,Ffi.Int)]),
                       SOME Ffi.FFI_TYPE_POINTER)
   fun mkargs (State jit_, Code code_, word1_, word2_, Pointer p) =
          let val (argsv,svec) = Ffi.mkargssvec [jit_ , code_,
                                                    Ffi.svec_setvecword word1_,
                                                    Ffi.svec_setvecword word2_,
                                                    p]
          in argsv
          end
     |  mkargs _ = raise Fail ("Jit:jit_new_node_wwp: argument type mismatch: expected"^
                                  " (State,Code,word,word,Pointer).")
   fun mkretval v = Node v
   val jit_new_node_wwp_sym = "_jit_new_node_www" (* Same lightning fn, different types *)
   val jit_new_node_wwp_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_new_node_wwp_sym)
in
   val jit_new_node_wwp = Ffi.ffi_trampoline "jit_new_node_wwp" jit_new_node_wwp_
                         jit_new_node_wwp_type mkargs mkretval
end;


local 
   val jit_lti_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "lti")))
   val jit_lti_u_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "lti_u")))
   val jit_lei_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "lei")))
   val jit_lei_u_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "lei_u")))
   val jit_gti_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "gti")))
   val jit_gti_u_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "gti_u")))
   val jit_gei_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "gei")))
   val jit_gei_u_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "gei_u")))
   val jit_eqi_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "eqi")))
   val jit_nei_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "nei")))
   val jit_ltr_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "ltr")))
   val jit_ltr_u_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "ltr_u")))
   val jit_ler_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "ler")))
   val jit_ler_u_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "ler_u")))
   val jit_gtr_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "gtr")))
   val jit_gtr_u_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "gtr_u")))
   val jit_ger_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "ger")))
   val jit_ger_u_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "ger_u")))
   val jit_eqr_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "eqr")))
   val jit_ner_ = Code (Ffi.svec_setvecword (Word.fromInt (Ffi.jit_code "ner")))
in
   fun jit_gti (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_gti_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n)
   fun jit_gti_u (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_gti_u_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n)
   fun jit_gei (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_gei_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n)
   fun jit_gei_u (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_gei_u_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n)
   fun jit_lti (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_lti_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n)
   fun jit_lti_u (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_lti_u_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n)
   fun jit_lei (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_lei_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n)
   fun jit_lei_u (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_lei_u_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n)
   fun jit_eqi (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_eqi_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n)
   fun jit_nei (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_nei_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n)
   fun jit_gtr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_gtr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_gtr_u (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_gtr_u_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_ger (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_ger_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_ger_u (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_ger_u_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_ltr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_ltr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_ltr_u (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_ltr_u_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_ler (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_ler_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_ler_u (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_ler_u_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_eqr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_eqr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_ner (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_ner_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3)
end;

local 
   open Ffi
   val jit_subi_ = Code (svec_setvecword (Word.fromInt (jit_code "subi")))
   val jit_addi_ = Code (svec_setvecword (Word.fromInt (jit_code "addi")))
   val jit_subci_ = Code (svec_setvecword (Word.fromInt (jit_code "subci")))
   val jit_addci_ = Code (svec_setvecword (Word.fromInt (jit_code "addci")))
   val jit_subxi_ = Code (svec_setvecword (Word.fromInt (jit_code "subxi")))
   val jit_addxi_ = Code (svec_setvecword (Word.fromInt (jit_code "addxi")))
   val jit_muli_ = Code (svec_setvecword (Word.fromInt (jit_code "muli")))
   val jit_divi_ = Code (svec_setvecword (Word.fromInt (jit_code "divi")))
   val jit_remi_ = Code (svec_setvecword (Word.fromInt (jit_code "remi")))
   val jit_divi_u_ = Code (svec_setvecword (Word.fromInt (jit_code "divi_u")))
   val jit_remi_u_ = Code (svec_setvecword (Word.fromInt (jit_code "remi_u")))
   val jit_andi_ = Code (svec_setvecword (Word.fromInt (jit_code "andi")))
   val jit_ori_ = Code (svec_setvecword (Word.fromInt (jit_code "ori")))
   val jit_xori_ = Code (svec_setvecword (Word.fromInt (jit_code "xori")))
   val jit_lshi_ = Code (svec_setvecword (Word.fromInt (jit_code "lshi")))
   val jit_rshi_ = Code (svec_setvecword (Word.fromInt (jit_code "rshi")))
   val jit_rshi_u_ = Code (svec_setvecword (Word.fromInt (jit_code "rshi_u")))
   val jit_subr_ = Code (svec_setvecword (Word.fromInt (jit_code "subr")))
   val jit_addr_ = Code (svec_setvecword (Word.fromInt (jit_code "addr")))
   val jit_subcr_ = Code (svec_setvecword (Word.fromInt (jit_code "subcr")))
   val jit_addcr_ = Code (svec_setvecword (Word.fromInt (jit_code "addcr")))
   val jit_subxr_ = Code (svec_setvecword (Word.fromInt (jit_code "subxr")))
   val jit_addxr_ = Code (svec_setvecword (Word.fromInt (jit_code "addxr")))
   val jit_mulr_ = Code (svec_setvecword (Word.fromInt (jit_code "mulr")))
   val jit_divr_ = Code (svec_setvecword (Word.fromInt (jit_code "divr")))
   val jit_remr_ = Code (svec_setvecword (Word.fromInt (jit_code "remr")))
   val jit_divr_u_ = Code (svec_setvecword (Word.fromInt (jit_code "divr_u")))
   val jit_remr_u_ = Code (svec_setvecword (Word.fromInt (jit_code "remr_u")))
   val jit_andr_ = Code (svec_setvecword (Word.fromInt (jit_code "andr")))
   val jit_orr_ = Code (svec_setvecword (Word.fromInt (jit_code "orr")))
   val jit_xorr_ = Code (svec_setvecword (Word.fromInt (jit_code "xorr")))
   val jit_lshr_ = Code (svec_setvecword (Word.fromInt (jit_code "lshr")))
   val jit_rshr_ = Code (svec_setvecword (Word.fromInt (jit_code "rshr")))
   val jit_rshr_u_ = Code (svec_setvecword (Word.fromInt (jit_code "rshr_u")))
in
   fun jit_subi (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_subi_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_addi (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_addi_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_subci (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_subci_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_addci (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_addci_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_subxi (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_subxi_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_addxi (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_addxi_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_muli (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_muli_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_divi (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_divi_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_remi (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_remi_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_divi_u (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_divi_u_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_remi_u (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_remi_u_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_andi (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_andi_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_ori (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_ori_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_xori (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_xori_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_lshi (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_lshi_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_rshi (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_rshi_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_rshi_u (jit, gpr1, gpr2, n) = 
          jit_new_node_www (jit, jit_rshi_u_, jit_gpr gpr1, jit_gpr gpr2, Word.fromInt n);
   fun jit_subr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_subr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
   fun jit_addr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_addr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
   fun jit_subcr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_subcr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
   fun jit_addcr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_addcr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
   fun jit_subxr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_subxr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
   fun jit_addxr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_addxr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
   fun jit_mulr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_mulr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
   fun jit_divr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_divr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
   fun jit_remr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_remr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
   fun jit_divr_u (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_divr_u_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
   fun jit_remr_u (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_remr_u_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
   fun jit_andr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_andr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
   fun jit_orr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_orr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
   fun jit_xorr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_xorr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
   fun jit_lshr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_lshr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
   fun jit_rshr (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_rshr_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
   fun jit_rshr_u (jit, gpr1, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_rshr_u_, jit_gpr gpr1, jit_gpr gpr2, jit_gpr gpr3);
end;

local open Ffi
   val jit_new_node_ww_type = 
              Function(Structure([Pointer(Structure[]),
                                  Integer(Unsigned,Int),
                                  Integer(Unsigned,Int),
                                  Integer(Unsigned,Int)]),
                       SOME FFI_TYPE_POINTER)
   fun mkargs (State jit_, Code code_, word1_, word2_) =
          let val (argsv,svec) = mkargssvec [jit_ , code_,
                                                    svec_setvecword word1_,
                                                    svec_setvecword word2_]
          in argsv
          end
     |  mkargs _ = raise Fail ("Jit:jit_new_node_ww: argument type mismatch: expected"^
                                  " (State,Code,word,word).")
   fun mkretval v = Node v
   val jit_new_node_ww_sym = "_jit_new_node_ww"
   val jit_new_node_ww_ =  Dynlib.cptr (Dynlib.dlsym liblightning jit_new_node_ww_sym)
in
   val jit_new_node_ww = ffi_trampoline "jit_new_node_ww" jit_new_node_ww_
                         jit_new_node_ww_type mkargs mkretval
end;

local 
   open Ffi
   val jit_movi_ = Code (svec_setvecword (Word.fromInt (jit_code "movi")))
   val jit_movr_ = Code (svec_setvecword (Word.fromInt (jit_code "movr")))
   val jit_negr_ = Code (svec_setvecword (Word.fromInt (jit_code "negr")))
   val jit_comr_ = Code (svec_setvecword (Word.fromInt (jit_code "comr")))
   val jit_htonr_ = Code (svec_setvecword (Word.fromInt (jit_code "htonr")))
   val jit_ntohr_ = Code (svec_setvecword (Word.fromInt (jit_code "htonr")))
in
   fun jit_movi (jit, gpr, n) = 
          jit_new_node_ww (jit, jit_movi_, jit_gpr gpr, Word.fromInt n)
   fun jit_movi_p (jit, gpr, p) = 
          jit_new_node_wp (jit, jit_movi_, jit_gpr gpr, p)
   fun jit_movr (jit, gpr, gpr2) = 
          jit_new_node_ww (jit, jit_movr_, jit_gpr gpr, jit_gpr gpr2)
   fun jit_negr (jit, gpr, gpr2) = 
          jit_new_node_ww (jit, jit_negr_, jit_gpr gpr, jit_gpr gpr2)
   fun jit_comr (jit, gpr, gpr2) = 
          jit_new_node_ww (jit, jit_comr_, jit_gpr gpr, jit_gpr gpr2)
   fun jit_ntohr (jit, gpr, gpr2) = 
          jit_new_node_ww (jit, jit_ntohr_, jit_gpr gpr, jit_gpr gpr2)
   fun jit_htonr (jit, gpr, gpr2) = 
          jit_new_node_ww (jit, jit_htonr_, jit_gpr gpr, jit_gpr gpr2)
end;

local 
   open Ffi
   val jit_ld_type = (if WORDSIZE = 32 then "_i" else "_l")
   val jit_ldr_ = Code (svec_setvecword (Word.fromInt (jit_code ("ldr"^jit_ld_type))))
   val jit_ldi_ = Code (svec_setvecword (Word.fromInt (jit_code ("ldi"^jit_ld_type))))
   val jit_ldr_c_ = Code (svec_setvecword (Word.fromInt (jit_code "ldr_c")))
   val jit_ldi_c_ = Code (svec_setvecword (Word.fromInt (jit_code "ldi_c")))
   val jit_ldr_uc_ = Code (svec_setvecword (Word.fromInt (jit_code "ldr_uc")))
   val jit_ldi_uc_ = Code (svec_setvecword (Word.fromInt (jit_code "ldi_uc")))
   val jit_ldr_s_ = Code (svec_setvecword (Word.fromInt (jit_code "ldr_s")))
   val jit_ldi_s_ = Code (svec_setvecword (Word.fromInt (jit_code "ldi_s")))
   val jit_ldr_us_ = Code (svec_setvecword (Word.fromInt (jit_code "ldr_us")))
   val jit_ldi_us_ = Code (svec_setvecword (Word.fromInt (jit_code "ldi_us")))
   val jit_ldr_i_ = Code (svec_setvecword (Word.fromInt (jit_code "ldr_i")))
   val jit_ldi_i_ = Code (svec_setvecword (Word.fromInt (jit_code "ldi_i")))
   val jit_ldr_ui_ = Code (svec_setvecword (Word.fromInt (jit_code "ldr_ui")))
   val jit_ldi_ui_ = Code (svec_setvecword (Word.fromInt (jit_code "ldi_ui")))
   val jit_ldr_l_ = Code (svec_setvecword (Word.fromInt (jit_code "ldr_l")))
   val jit_ldi_l_ = Code (svec_setvecword (Word.fromInt (jit_code "ldi_l")))
   val jit_ldxr_ = Code (svec_setvecword (Word.fromInt (jit_code ("ldxr"^jit_ld_type))))
   val jit_ldxi_ = Code (svec_setvecword (Word.fromInt (jit_code ("ldxi"^jit_ld_type))))
   val jit_ldxr_c_ = Code (svec_setvecword (Word.fromInt (jit_code "ldxr_c")))
   val jit_ldxi_c_ = Code (svec_setvecword (Word.fromInt (jit_code "ldxi_c")))
   val jit_ldxr_uc_ = Code (svec_setvecword (Word.fromInt (jit_code "ldxr_uc")))
   val jit_ldxi_uc_ = Code (svec_setvecword (Word.fromInt (jit_code "ldxi_uc")))
   val jit_ldxr_s_ = Code (svec_setvecword (Word.fromInt (jit_code "ldxr_s")))
   val jit_ldxi_s_ = Code (svec_setvecword (Word.fromInt (jit_code "ldxi_s")))
   val jit_ldxr_us_ = Code (svec_setvecword (Word.fromInt (jit_code "ldxr_us")))
   val jit_ldxi_us_ = Code (svec_setvecword (Word.fromInt (jit_code "ldxi_us")))
   val jit_ldxr_i_ = Code (svec_setvecword (Word.fromInt (jit_code "ldxr_i")))
   val jit_ldxi_i_ = Code (svec_setvecword (Word.fromInt (jit_code "ldxi_i")))
   val jit_ldxr_ui_ = Code (svec_setvecword (Word.fromInt (jit_code "ldxr_ui")))
   val jit_ldxi_ui_ = Code (svec_setvecword (Word.fromInt (jit_code "ldxi_ui")))
   val jit_ldxr_l_ = Code (svec_setvecword (Word.fromInt (jit_code "ldxr_l")))
   val jit_ldxi_l_ = Code (svec_setvecword (Word.fromInt (jit_code "ldxi_l")))
   val jit_st_type = (if WORDSIZE = 32 then "_i" else "_l")
   val jit_str_ = Code (svec_setvecword (Word.fromInt (jit_code ("str"^jit_st_type))))
   val jit_sti_ = Code (svec_setvecword (Word.fromInt (jit_code ("sti"^jit_st_type))))
   val jit_str_c_ = Code (svec_setvecword (Word.fromInt (jit_code "str_c")))
   val jit_sti_c_ = Code (svec_setvecword (Word.fromInt (jit_code "sti_c")))
   val jit_str_s_ = Code (svec_setvecword (Word.fromInt (jit_code "str_s")))
   val jit_sti_s_ = Code (svec_setvecword (Word.fromInt (jit_code "sti_s")))
   val jit_str_i_ = Code (svec_setvecword (Word.fromInt (jit_code "str_i")))
   val jit_sti_i_ = Code (svec_setvecword (Word.fromInt (jit_code "sti_i")))
   val jit_str_l_ = Code (svec_setvecword (Word.fromInt (jit_code "str_l")))
   val jit_sti_l_ = Code (svec_setvecword (Word.fromInt (jit_code "sti_l")))
   val jit_stxr_ = Code (svec_setvecword (Word.fromInt (jit_code ("stxr"^jit_st_type))))
   val jit_stxi_ = Code (svec_setvecword (Word.fromInt (jit_code ("stxi"^jit_st_type))))
   val jit_stxr_c_ = Code (svec_setvecword (Word.fromInt (jit_code "stxr_c")))
   val jit_stxi_c_ = Code (svec_setvecword (Word.fromInt (jit_code "stxi_c")))
   val jit_stxr_s_ = Code (svec_setvecword (Word.fromInt (jit_code "stxr_s")))
   val jit_stxi_s_ = Code (svec_setvecword (Word.fromInt (jit_code "stxi_s")))
   val jit_stxr_i_ = Code (svec_setvecword (Word.fromInt (jit_code "stxr_i")))
   val jit_stxi_i_ = Code (svec_setvecword (Word.fromInt (jit_code "stxi_i")))
   val jit_stxr_l_ = Code (svec_setvecword (Word.fromInt (jit_code "stxr_l")))
   val jit_stxi_l_ = Code (svec_setvecword (Word.fromInt (jit_code "stxi_l")))
in
   fun jit_ldr (jit, gpr, gpr2) = 
          jit_new_node_ww (jit, jit_ldr_, jit_gpr gpr, jit_gpr gpr2)
   fun jit_ldi (jit, gpr, p) = 
          jit_new_node_wp (jit, jit_ldi_, jit_gpr gpr, p)
   fun jit_ldr_c (jit, gpr, gpr2) = 
          jit_new_node_ww (jit, jit_ldr_c_, jit_gpr gpr, jit_gpr gpr2)
   fun jit_ldi_c (jit, gpr, p) = 
          jit_new_node_wp (jit, jit_ldi_c_, jit_gpr gpr, p)
   fun jit_ldr_uc (jit, gpr, gpr2) = 
          jit_new_node_ww (jit, jit_ldr_uc_, jit_gpr gpr, jit_gpr gpr2)
   fun jit_ldi_uc (jit, gpr, p) = 
          jit_new_node_wp (jit, jit_ldi_uc_, jit_gpr gpr, p)
   fun jit_ldr_s (jit, gpr, gpr2) = 
          jit_new_node_ww (jit, jit_ldr_s_, jit_gpr gpr, jit_gpr gpr2)
   fun jit_ldi_s (jit, gpr, p) = 
          jit_new_node_wp (jit, jit_ldi_s_, jit_gpr gpr, p)
   fun jit_ldr_us (jit, gpr, gpr2) = 
          jit_new_node_ww (jit, jit_ldr_us_, jit_gpr gpr, jit_gpr gpr2)
   fun jit_ldi_us (jit, gpr, p) = 
          jit_new_node_wp (jit, jit_ldi_us_, jit_gpr gpr, p)
   fun jit_ldr_i (jit, gpr, gpr2) = 
          jit_new_node_ww (jit, jit_ldr_i_, jit_gpr gpr, jit_gpr gpr2)
   fun jit_ldi_i (jit, gpr, p) = 
          jit_new_node_wp (jit, jit_ldi_i_, jit_gpr gpr, p)
   fun jit_ldr_ui (jit, gpr, gpr2) =
          only64 "jit_ldr_ui" (fn () => jit_new_node_ww (jit, jit_ldr_ui_, jit_gpr gpr, jit_gpr gpr2))
   fun jit_ldi_ui (jit, gpr, p) =
          only64 "jit_ldi_ui" (fn () => jit_new_node_wp (jit, jit_ldi_ui_, jit_gpr gpr, p))
   fun jit_ldr_l (jit, gpr, gpr2) =
          only64 "jit_ldr_l" (fn () => jit_new_node_ww (jit, jit_ldr_l_, jit_gpr gpr, jit_gpr gpr2))
   fun jit_ldi_l (jit, gpr, p) =
          only64 "jit_ldi_l" (fn () => jit_new_node_wp (jit, jit_ldi_l_, jit_gpr gpr, p))
   fun jit_ldxr (jit, gpr, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_ldxr_, jit_gpr gpr, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_ldxi (jit, gpr, gpr2, n) = 
          jit_new_node_www (jit, jit_ldxi_, jit_gpr gpr, jit_gpr gpr2, Word.fromInt n)
   fun jit_ldxr_c (jit, gpr, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_ldxr_c_, jit_gpr gpr, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_ldxi_c (jit, gpr, gpr2, n) = 
          jit_new_node_www (jit, jit_ldxi_c_, jit_gpr gpr, jit_gpr gpr2, Word.fromInt n)
   fun jit_ldxr_uc (jit, gpr, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_ldxr_uc_, jit_gpr gpr, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_ldxi_uc (jit, gpr, gpr2, n) = 
          jit_new_node_www (jit, jit_ldxi_uc_, jit_gpr gpr, jit_gpr gpr2, Word.fromInt n)
   fun jit_ldxr_s (jit, gpr, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_ldxr_s_, jit_gpr gpr, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_ldxi_s (jit, gpr, gpr2, n) = 
          jit_new_node_www (jit, jit_ldxi_s_, jit_gpr gpr, jit_gpr gpr2, Word.fromInt n)
   fun jit_ldxr_us (jit, gpr, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_ldxr_us_, jit_gpr gpr, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_ldxi_us (jit, gpr, gpr2, n) = 
          jit_new_node_www (jit, jit_ldxi_us_, jit_gpr gpr, jit_gpr gpr2, Word.fromInt n)
   fun jit_ldxr_i (jit, gpr, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_ldxr_i_, jit_gpr gpr, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_ldxi_i (jit, gpr, gpr2, n) = 
          jit_new_node_www (jit, jit_ldxi_i_, jit_gpr gpr, jit_gpr gpr2, Word.fromInt n)
   fun jit_ldxr_ui (jit, gpr, gpr2, gpr3) =
          only64 "jit_ldxr_ui" (fn () => 
          jit_new_node_www (jit, jit_ldxr_ui_, jit_gpr gpr, jit_gpr gpr2, jit_gpr gpr3))
   fun jit_ldxi_ui (jit, gpr, gpr2, n) =
          only64 "jit_ldxi_ui" (fn () => 
          jit_new_node_www (jit, jit_ldxi_ui_, jit_gpr gpr, jit_gpr gpr2, Word.fromInt n))
   fun jit_ldxr_l (jit, gpr, gpr2, gpr3) =
          only64 "jit_ldxr_l" (fn () => 
          jit_new_node_www (jit, jit_ldxr_l_, jit_gpr gpr, jit_gpr gpr2, jit_gpr gpr3))
   fun jit_ldxi_l (jit, gpr, gpr2, n) =
          only64 "jit_ldxi_l" (fn () => 
          jit_new_node_www (jit, jit_ldxi_l_, jit_gpr gpr, jit_gpr gpr2, Word.fromInt n))
   fun jit_str (jit, gpr, gpr2) = 
          jit_new_node_ww (jit, jit_str_, jit_gpr gpr, jit_gpr gpr2)
   fun jit_sti (jit, p, gpr) = 
          jit_new_node_pw (jit, jit_sti_, p, jit_gpr gpr)
   fun jit_str_c (jit, gpr, gpr2) = 
          jit_new_node_ww (jit, jit_str_c_, jit_gpr gpr, jit_gpr gpr2)
   fun jit_sti_c (jit, p, gpr) = 
          jit_new_node_pw (jit, jit_sti_c_, p, jit_gpr gpr)
   fun jit_str_s (jit, gpr, gpr2) = 
          jit_new_node_ww (jit, jit_str_s_, jit_gpr gpr, jit_gpr gpr2)
   fun jit_sti_s (jit, p, gpr) = 
          jit_new_node_pw (jit, jit_sti_s_, p, jit_gpr gpr)
   fun jit_str_i (jit, gpr, gpr2) = 
          jit_new_node_ww (jit, jit_str_i_, jit_gpr gpr, jit_gpr gpr2)
   fun jit_sti_i (jit, p, gpr) = 
          jit_new_node_pw (jit, jit_sti_i_, p, jit_gpr gpr)
   fun jit_str_l (jit, gpr, gpr2) =
          only64 "jit_str_l" (fn () => jit_new_node_ww (jit, jit_str_l_, jit_gpr gpr, jit_gpr gpr2))
   fun jit_sti_l (jit, p, gpr) =
          only64 "jit_sti_l" (fn () => jit_new_node_pw (jit, jit_sti_l_, p, jit_gpr gpr))
   fun jit_stxr (jit, gpr, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_stxr_, jit_gpr gpr, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_stxi (jit, n, gpr, gpr2) = 
          jit_new_node_www (jit, jit_stxi_, Word.fromInt n, jit_gpr gpr, jit_gpr gpr2)
   fun jit_stxr_c (jit, gpr, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_stxr_c_, jit_gpr gpr, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_stxi_c (jit, n, gpr, gpr2) = 
          jit_new_node_www (jit, jit_stxi_c_, Word.fromInt n, jit_gpr gpr, jit_gpr gpr2)
   fun jit_stxr_s (jit, gpr, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_stxr_s_, jit_gpr gpr, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_stxi_s (jit, n, gpr, gpr2) = 
          jit_new_node_www (jit, jit_stxi_s_, Word.fromInt n, jit_gpr gpr, jit_gpr gpr2)
   fun jit_stxr_i (jit, gpr, gpr2, gpr3) = 
          jit_new_node_www (jit, jit_stxr_i_, jit_gpr gpr, jit_gpr gpr2, jit_gpr gpr3)
   fun jit_stxi_i (jit, n, gpr, gpr2) = 
          jit_new_node_www (jit, jit_stxi_i_, Word.fromInt n, jit_gpr gpr, jit_gpr gpr2)
   fun jit_stxr_l (jit, gpr, gpr2, gpr3) =
          only64 "jit_stxr_l" (fn () =>
          jit_new_node_www (jit, jit_stxr_l_, jit_gpr gpr, jit_gpr gpr2, jit_gpr gpr3))
   fun jit_stxi_l (jit, n, gpr, gpr2) =
          only64 "jit_stxi_l" (fn () => 
          jit_new_node_www (jit, jit_stxi_l_, Word.fromInt n, jit_gpr gpr, jit_gpr gpr2))
end;

local
   val jit_emit_type = Ffi.Function(Ffi.Pointer(Ffi.Structure([])), SOME Ffi.FFI_TYPE_POINTER)
   fun mkargs (State jit_) = jit_
     | mkargs _ = raise Fail "Jit.mkargs: argument type mismatch: expected (State)."
   fun mkretval v = Ffi.svec_getcptr v
   val jit_emit = Dynlib.cptr (Dynlib.dlsym liblightning "_jit_emit")
in
   val jit_emit = Ffi.ffi_trampoline "jit_emit" jit_emit jit_emit_type mkargs mkretval
end;
