prim_eqtype state;
prim_eqtype node;
prim_eqtype code;

val liblgtng =
       Dynlib.dlopen {lib = "liblgtng.so",
                      flag = Dynlib.RTLD_LAZY,
                      global = false };

fun lgtsym s = Dynlib.dlsym liblgtng s;
fun lgtsymp s = Dynlib.cptr (lgtsym s);

fun lgtapp1 s = Dynlib.app1 (lgtsym s);
fun lgtapp2 s = Dynlib.app2 (lgtsym s);
fun lgtapp3 s = Dynlib.app3 (lgtsym s);
fun lgtapp4 s = Dynlib.app4 (lgtsym s);
fun lgtapp5 s = Dynlib.app5 (lgtsym s);

prim_val var  : Dynlib.cptr -> 'b
   = 1 "c_var"
prim_val app1 : Dynlib.cptr -> 'a1 -> 'b
   = 2 "cfun_app1"
prim_val app2 : Dynlib.cptr -> 'a1 -> 'a2 -> 'b
   = 3 "cfun_app2"
prim_val app3 : Dynlib.cptr -> 'a1 -> 'a2 -> 'a3 -> 'b
   = 4 "cfun_app3"
prim_val app4 : Dynlib.cptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'b
   = 5 "cfun_app4"
prim_val app5 : Dynlib.cptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b
   = 6 "cfun_app5"

val jit_get_NULL : unit -> Dynlib.cptr
   = lgtapp1 "jit_get_NULL";

val jit_initialise_constants : unit -> unit
    = lgtapp1 "jit_initialise_constants";

val jit_get_constantslength : unit -> Int.int
    = lgtapp1 "jit_get_constantslength";

val jit_get_constantsentry : Int.int -> Word.word * String.string
    = lgtapp1 "jit_get_constantsentry";

val jit_get_sizeslength : unit -> Int.int
    = lgtapp1 "jit_get_sizeslength";

val jit_get_sizesentry : Int.int -> Int.int * String.string
    = lgtapp1 "jit_get_sizesentry";

val jit_get_wordsize : unit -> int
    = lgtapp1 "jit_get_wordsize";

val jit_get_arch : unit -> string
    = lgtapp1 "jit_get_arch";

val jit_get_little_endian : unit -> int
    = lgtapp1 "jit_get_little_endian";

val jit_get_big_endian : unit -> int
    = lgtapp1 "jit_get_big_endian";

val jit_get_byteorder : unit -> int
    = lgtapp1 "jit_get_byteorder";

val jit_r_num : unit -> int
    = lgtapp1 "jit_r_num_";

val jit_v_num : unit -> int
    = lgtapp1 "jit_v_num_";

val jit_f_num : unit -> int
    = lgtapp1 "jit_f_num_";

val jit_r : int -> int
    = lgtapp1 "jit_r_";

val jit_v : int -> int
    = lgtapp1 "jit_v_";

val jit_f : int -> int
    = lgtapp1 "jit_f_";

val init_jit : unit -> unit
    = lgtapp1 "sml_init_jit";

val finish_jit : unit -> unit
    = lgtapp1 "sml_finish_jit";

val jit_new_state : unit -> state
    = lgtapp1 "sml_jit_new_state";

datatype jit_gpr_t = 
   R0 | R1 | R2 | V0 | V1 | V2 | FP | R of int | V of int

datatype jit_fpr_t = 
   F0 | F1 | F2 | F3 | F4 | F5 | F of int

fun assq n l =
  let fun lookup k =
     let fun iter [] = raise Fail ("Lightning.assq: internal error: no value in "^
                                               n^" table for key "^k)
           | iter ((k',v)::ps) = if k' = k then v else iter ps
     in iter l
     end
  in lookup
  end;

fun assqi n l =
  let fun lookup k =
     let fun iter [] = raise Fail ("Lightning.assq: internal error: no value in "^
                                      n^" table for key "^(Int.toString k))
           | iter ((k',v)::ps) = if k' = k then v else iter ps
     in iter l
     end
  in lookup
  end;

local
   val _ = jit_initialise_constants();
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
   val WORDSIZE = jit_get_wordsize()
   val BYTE_ORDER = jit_get_byteorder()
   val LITTLE_ENDIAN = jit_get_little_endian()
   val BIG_ENDIAN = jit_get_big_endian()
   val jit_get_constant = ent
   val jit_constants = ctbl
end;

val R_NUM = jit_r_num();
val V_NUM = jit_v_num();
val F_NUM = jit_f_num();

val MAX_R = R_NUM - 1;
val MAX_V = V_NUM - 1;
val MAX_F = F_NUM - 1;

val NULL = jit_get_NULL();

val R3 = R(3)
val V3 = V(3)

val F6 = F(6);
val F7 = F(7);

val JIT_FP = Word.toInt (jit_get_constant "FP");

fun jit_gpr (R n) = 
       if n > MAX_R
          then raise Fail
                 ("Invalid R(n) register. (n>"
                     ^(Int.toString (MAX_R))^")")
          else jit_r(n)
  | jit_gpr R0 = jit_r(0)
  | jit_gpr R1 = jit_r(1)
  | jit_gpr R2 = jit_r(2)
  | jit_gpr FP = jit_r(JIT_FP)
  | jit_gpr (V n) = 
       if n > MAX_V
          then raise Fail
                 ("Invalid V(n) register. (n>"
                     ^(Int.toString (MAX_V))^")")
          else jit_v(n)
  | jit_gpr V0 = jit_v(0)
  | jit_gpr V1 = jit_v(1)
  | jit_gpr V2 = jit_v(2);

fun jit_fpr (F n) = 
       if n > MAX_F
          then raise Fail
                 ("Invalid F(n) register. (n>"
                     ^(Int.toString (MAX_F))^")")
          else jit_f(n)
  | jit_fpr F0 = jit_f(0)
  | jit_fpr F1 = jit_f(1)
  | jit_fpr F2 = jit_f(2)
  | jit_fpr F3 = jit_f(3)
  | jit_fpr F4 = jit_f(4)
  | jit_fpr F5 = jit_f(5);

val jit_address : state * node -> Dynlib.cptr
       = fn (s, node) => lgtapp1 "jit_sml_address" (s, node);

val jit_allocai : state * word -> int
       = fn (s, u) => lgtapp1 "jit_sml_allocai" (s, u);

val jit_arg : state -> node
       = fn (s) => lgtapp1 "jit_sml_arg" (s);

val jit_arg_d : state -> node
       = fn (s) => lgtapp1 "jit_sml_arg_d" (s);

val jit_arg_f : state -> node
       = fn (s) => lgtapp1 "jit_sml_arg_f" (s);

val jit_clear_state : state -> unit
       = fn (s) => lgtapp1 "jit_sml_clear_state" (s);

val jit_destroy_state : state -> unit
       = fn (s) => lgtapp1 "jit_sml_destroy_state" (s);

val jit_disassemble : state -> unit
       = fn (s) => lgtapp1 "jit_sml_disassemble" (s);

val jit_ellipsis : state -> unit
       = fn (s) => lgtapp1 "jit_sml_ellipsis" (s);

val jit_emit : state -> Dynlib.cptr
       = fn (s) => lgtapp1 "jit_sml_emit" (s);

val jit_epilog : state -> unit
       = fn (s) => lgtapp1 "jit_sml_epilog" (s);

val jit_finishi : state * Dynlib.cptr -> node
       = fn (s, u) => lgtapp1 "jit_sml_finishi" (s, u);

val jit_finishr : state * jit_gpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_finishr" (s, jit_gpr u);

val jit_forward : state -> unit
       = fn (s) => lgtapp1 "jit_sml_forward" (s);

val jit_getarg_c : state * jit_gpr_t * node -> unit
       = fn (s, u, v) => lgtapp1 "jit_sml_getarg_c" (s, jit_gpr u, v);

val jit_getarg_d : state * jit_fpr_t * node -> unit
       = fn (s, u, v) => lgtapp1 "jit_sml_getarg_d" (s, jit_fpr u, v);

val jit_getarg_f : state * jit_fpr_t * node -> unit
       = fn (s, u, v) => lgtapp1 "jit_sml_getarg_f" (s, jit_fpr u, v);

val jit_getarg_i : state * jit_gpr_t * node -> unit
       = fn (s, u, v) => lgtapp1 "jit_sml_getarg_i" (s, jit_gpr u, v);

val jit_getarg_l : state * jit_gpr_t * node -> unit
       = fn (s, u, v) => lgtapp1 "jit_sml_getarg_l" (s, jit_gpr u, v);

val jit_getarg : state * jit_gpr_t * node -> unit
       = fn (s, u, v) => lgtapp1 "jit_sml_getarg" (s, jit_gpr u, v);

val jit_getarg_s : state * jit_gpr_t * node -> unit
       = fn (s, u, v) => lgtapp1 "jit_sml_getarg_s" (s, jit_gpr u, v);

val jit_getarg_uc : state * jit_gpr_t * node -> unit
       = fn (s, u, v) => lgtapp1 "jit_sml_getarg_uc" (s, jit_gpr u, v);

val jit_getarg_ui : state * jit_gpr_t * node -> unit
       = fn (s, u, v) => lgtapp1 "jit_sml_getarg_ui" (s, jit_gpr u, v);

val jit_getarg_us : state * jit_gpr_t * node -> unit
       = fn (s, u, v) => lgtapp1 "jit_sml_getarg_us" (s, jit_gpr u, v);

val jit_indirect : state -> node
       = fn (s) => lgtapp1 "jit_sml_indirect" (s);

val jit_label : state -> unit
       = fn (s) => lgtapp1 "jit_sml_label" (s);

val jit_new_node : state * code -> node
       = fn (s, c) => lgtapp1 "jit_sml_new_node" (s, c);

val jit_note : state * Dynlib.cptr * word -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_note" (s, u, v);

val jit_patch : state * node -> unit
       = fn (s, u) => lgtapp1 "jit_sml_patch" (s, u);

val jit_patch_abs : state * node * Dynlib.cptr -> unit
       = fn (s, u, v) => lgtapp1 "jit_sml_patch_abs" (s, u, v);

val jit_patch_at : state * node * node -> unit
       = fn (s, u, v) => lgtapp1 "jit_sml_patch_at" (s, u, v);

val jit_prepare : state -> unit
       = fn (s) => lgtapp1 "jit_sml_prepare" (s);

val jit_print : state -> unit
       = fn (s) => lgtapp1 "jit_sml_print" (s);

val jit_prolog : state -> unit
       = fn (s) => lgtapp1 "jit_sml_prolog" (s);

val jit_pushargi : state * word -> unit
       = fn (s, u) => lgtapp1 "jit_sml_pushargi" (s, u);

val jit_pushargi_d : state * real -> unit
       = fn (s, u) => lgtapp1 "jit_sml_pushargi_d" (s, u);

val jit_pushargi_f : state * real -> unit
       = fn (s, u) => lgtapp1 "jit_sml_pushargi_f" (s, u);

val jit_pushargr : state * jit_gpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_pushargr" (s, jit_gpr u);

val jit_pushargr_d : state * jit_fpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_pushargr_d" (s, jit_fpr u);

val jit_pushargr_f : state * jit_fpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_pushargr_f" (s, jit_fpr u);

val jit_realize : state -> unit
       = fn (s) => lgtapp1 "jit_sml_realize" (s);

val jit_ret : state -> unit
       = fn (s) => lgtapp1 "jit_sml_ret" (s);

val jit_reti : state * word -> unit
       = fn (s, u) => lgtapp1 "jit_sml_reti" (s, u);

val jit_reti_d : state * real -> unit
       = fn (s, u) => lgtapp1 "jit_sml_reti_d" (s, u);

val jit_reti_f : state * real -> unit
       = fn (s, u) => lgtapp1 "jit_sml_reti_f" (s, u);

val jit_retr : state * jit_gpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_retr" (s, jit_gpr u);

val jit_retr_d : state * jit_fpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_retr_d" (s, jit_fpr u);

val jit_retr_f : state * jit_fpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_retr_f" (s, jit_fpr u);

val jit_retval_c : state * jit_gpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_retval_c" (s, jit_gpr u);

val jit_retval_d : state * jit_fpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_retval_d" (s, jit_fpr u);

val jit_retval_f : state * jit_fpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_retval_f" (s, jit_fpr u);

val jit_retval_i : state * jit_gpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_retval_i" (s, jit_gpr u);

val jit_retval_l : state * jit_gpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_retval_l" (s, jit_gpr u);

val jit_retval : state * jit_gpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_retval" (s, jit_gpr u);

val jit_retval_s : state * jit_gpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_retval_s" (s, jit_gpr u);

val jit_retval_uc : state * jit_gpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_retval_uc" (s, jit_gpr u);

val jit_retval_ui : state * jit_gpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_retval_ui" (s, jit_gpr u);

val jit_retval_us : state * jit_gpr_t -> unit
       = fn (s, u) => lgtapp1 "jit_sml_retval_us" (s, jit_gpr u);

val jit_absr_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_absr_d" (s, jit_fpr u, jit_fpr v);

val jit_absr_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_absr_f" (s, jit_fpr u, jit_fpr v);

val jit_addci : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_addci" (s, jit_gpr u, jit_gpr v, w);

val jit_addcr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_addcr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_addi : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_addi" (s, jit_gpr u, jit_gpr v, w);

val jit_addi_d : state * jit_fpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_addi_d" (s, jit_fpr u, jit_fpr v, w);

val jit_addi_f : state * jit_fpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_addi_f" (s, jit_fpr u, jit_fpr v, w);

val jit_addr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_addr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_addr_d : state * jit_fpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_addr_d" (s, jit_fpr u, jit_fpr v, jit_fpr w);

val jit_addr_f : state * jit_fpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_addr_f" (s, jit_fpr u, jit_fpr v, jit_fpr w);

val jit_addxi : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_addxi" (s, jit_gpr u, jit_gpr v, w);

val jit_addxr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_addxr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_andi : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_andi" (s, jit_gpr u, jit_gpr v, w);

val jit_andr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_andr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_beqi : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_beqi" (s, jit_gpr v, w);

val jit_beqi_d : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_beqi_d" (s, jit_fpr v, w);

val jit_beqi_f : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_beqi_f" (s, jit_fpr v, w);

val jit_beqr : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_beqr" (s, jit_gpr v, jit_gpr w);

val jit_beqr_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_beqr_d" (s, jit_fpr v, jit_fpr w);

val jit_beqr_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_beqr_f" (s, jit_fpr v, jit_fpr w);

val jit_bgei : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bgei" (s, jit_gpr v, w);

val jit_bgei_d : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bgei_d" (s, jit_fpr v, w);

val jit_bgei_f : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bgei_f" (s, jit_fpr v, w);

val jit_bgei_u : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bgei_u" (s, jit_gpr v, w);

val jit_bger : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bger" (s, jit_gpr v, jit_gpr w);

val jit_bger_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bger_d" (s, jit_fpr v, jit_fpr w);

val jit_bger_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bger_f" (s, jit_fpr v, jit_fpr w);

val jit_bger_u : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bger_u" (s, jit_gpr v, jit_gpr w);

val jit_bgti : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bgti" (s, jit_gpr v, w);

val jit_bgti_d : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bgti_d" (s, jit_fpr v, w);

val jit_bgti_f : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bgti_f" (s, jit_fpr v, w);

val jit_bgti_u : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bgti_u" (s, jit_gpr v, w);

val jit_bgtr : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bgtr" (s, jit_gpr v, jit_gpr w);

val jit_bgtr_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bgtr_d" (s, jit_fpr v, jit_fpr w);

val jit_bgtr_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bgtr_f" (s, jit_fpr v, jit_fpr w);

val jit_bgtr_u : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bgtr_u" (s, jit_gpr v, jit_gpr w);

val jit_blei : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_blei" (s, jit_gpr v, w);

val jit_blei_d : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_blei_d" (s, jit_fpr v, w);

val jit_blei_f : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_blei_f" (s, jit_fpr v, w);

val jit_blei_u : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_blei_u" (s, jit_gpr v, w);

val jit_bler : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bler" (s, jit_gpr v, jit_gpr w);

val jit_bler_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bler_d" (s, jit_fpr v, jit_fpr w);

val jit_bler_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bler_f" (s, jit_fpr v, jit_fpr w);

val jit_bler_u : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bler_u" (s, jit_gpr v, jit_gpr w);

val jit_bltgti_d : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bltgti_d" (s, jit_fpr v, w);

val jit_bltgti_f : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bltgti_f" (s, jit_fpr v, w);

val jit_bltgtr_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bltgtr_d" (s, jit_fpr v, jit_fpr w);

val jit_bltgtr_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bltgtr_f" (s, jit_fpr v, jit_fpr w);

val jit_blti : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_blti" (s, jit_gpr v, w);

val jit_blti_d : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_blti_d" (s, jit_fpr v, w);

val jit_blti_f : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_blti_f" (s, jit_fpr v, w);

val jit_blti_u : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_blti_u" (s, jit_gpr v, w);

val jit_bltr : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bltr" (s, jit_gpr v, jit_gpr w);

val jit_bltr_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bltr_d" (s, jit_fpr v, jit_fpr w);

val jit_bltr_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bltr_f" (s, jit_fpr v, jit_fpr w);

val jit_bltr_u : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bltr_u" (s, jit_gpr v, jit_gpr w);

val jit_bmci : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bmci" (s, jit_gpr v, w);

val jit_bmcr : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bmcr" (s, jit_gpr v, jit_gpr w);

val jit_bmsi : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bmsi" (s, jit_gpr v, w);

val jit_bmsr : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bmsr" (s, jit_gpr v, jit_gpr w);

val jit_bnei : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bnei" (s, jit_gpr v, w);

val jit_bnei_d : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bnei_d" (s, jit_fpr v, w);

val jit_bnei_f : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bnei_f" (s, jit_fpr v, w);

val jit_bner : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bner" (s, jit_gpr v, jit_gpr w);

val jit_bner_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bner_d" (s, jit_fpr v, jit_fpr w);

val jit_bner_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bner_f" (s, jit_fpr v, jit_fpr w);

val jit_boaddi : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_boaddi" (s, jit_gpr v, w);

val jit_boaddi_u : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_boaddi_u" (s, jit_gpr v, w);

val jit_boaddr : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_boaddr" (s, jit_gpr v, jit_gpr w);

val jit_boaddr_u : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_boaddr_u" (s, jit_gpr v, jit_gpr w);

val jit_bordi_d : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bordi_d" (s, jit_fpr v, w);

val jit_bordi_f : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bordi_f" (s, jit_fpr v, w);

val jit_bordr_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bordr_d" (s, jit_fpr v, jit_fpr w);

val jit_bordr_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bordr_f" (s, jit_fpr v, jit_fpr w);

val jit_bosubi : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bosubi" (s, jit_gpr v, w);

val jit_bosubi_u : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bosubi_u" (s, jit_gpr v, w);

val jit_bosubr : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bosubr" (s, jit_gpr v, jit_gpr w);

val jit_bosubr_u : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bosubr_u" (s, jit_gpr v, jit_gpr w);

val jit_buneqi_d : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_buneqi_d" (s, jit_fpr v, w);

val jit_buneqi_f : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_buneqi_f" (s, jit_fpr v, w);

val jit_buneqr_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_buneqr_d" (s, jit_fpr v, jit_fpr w);

val jit_buneqr_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_buneqr_f" (s, jit_fpr v, jit_fpr w);

val jit_bungei_d : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bungei_d" (s, jit_fpr v, w);

val jit_bungei_f : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bungei_f" (s, jit_fpr v, w);

val jit_bunger_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bunger_d" (s, jit_fpr v, jit_fpr w);

val jit_bunger_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bunger_f" (s, jit_fpr v, jit_fpr w);

val jit_bungti_d : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bungti_d" (s, jit_fpr v, w);

val jit_bungti_f : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bungti_f" (s, jit_fpr v, w);

val jit_bungtr_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bungtr_d" (s, jit_fpr v, jit_fpr w);

val jit_bungtr_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bungtr_f" (s, jit_fpr v, jit_fpr w);

val jit_bunlei_d : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bunlei_d" (s, jit_fpr v, w);

val jit_bunlei_f : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bunlei_f" (s, jit_fpr v, w);

val jit_bunler_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bunler_d" (s, jit_fpr v, jit_fpr w);

val jit_bunler_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bunler_f" (s, jit_fpr v, jit_fpr w);

val jit_bunlti_d : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bunlti_d" (s, jit_fpr v, w);

val jit_bunlti_f : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bunlti_f" (s, jit_fpr v, w);

val jit_bunltr_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bunltr_d" (s, jit_fpr v, jit_fpr w);

val jit_bunltr_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bunltr_f" (s, jit_fpr v, jit_fpr w);

val jit_bunordi_d : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bunordi_d" (s, jit_fpr v, w);

val jit_bunordi_f : state * jit_fpr_t * real -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bunordi_f" (s, jit_fpr v, w);

val jit_bunordr_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bunordr_d" (s, jit_fpr v, jit_fpr w);

val jit_bunordr_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bunordr_f" (s, jit_fpr v, jit_fpr w);

val jit_bxaddi : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bxaddi" (s, jit_gpr v, w);

val jit_bxaddi_u : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bxaddi_u" (s, jit_gpr v, w);

val jit_bxaddr : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bxaddr" (s, jit_gpr v, jit_gpr w);

val jit_bxaddr_u : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bxaddr_u" (s, jit_gpr v, jit_gpr w);

val jit_bxsubi : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bxsubi" (s, jit_gpr v, w);

val jit_bxsubi_u : state * jit_gpr_t * word -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bxsubi_u" (s, jit_gpr v, w);

val jit_bxsubr : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bxsubr" (s, jit_gpr v, jit_gpr w);

val jit_bxsubr_u : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, v, w) => lgtapp1 "jit_sml_bxsubr_u" (s, jit_gpr v, jit_gpr w);

val jit_calli : state * Dynlib.cptr -> node
       = fn (s, u) => lgtapp1 "jit_sml_calli" (s, u);

val jit_callr : state * jit_gpr_t -> node
       = fn (s, u) => lgtapp1 "jit_sml_callr" (s, jit_gpr u);

val jit_comr : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_comr" (s, jit_gpr u, jit_gpr v);

val jit_divi : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_divi" (s, jit_gpr u, jit_gpr v, w);

val jit_divi_d : state * jit_fpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_divi_d" (s, jit_fpr u, jit_fpr v, w);

val jit_divi_f : state * jit_fpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_divi_f" (s, jit_fpr u, jit_fpr v, w);

val jit_divi_u : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_divi_u" (s, jit_gpr u, jit_gpr v, w);

val jit_divr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_divr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_divr_d : state * jit_fpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_divr_d" (s, jit_fpr u, jit_fpr v, jit_fpr w);

val jit_divr_f : state * jit_fpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_divr_f" (s, jit_fpr u, jit_fpr v, jit_fpr w);

val jit_divr_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_divr_u" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_eqi : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_eqi" (s, jit_gpr u, jit_gpr v, w);

val jit_eqi_d : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_eqi_d" (s, jit_gpr u, jit_fpr v, w);

val jit_eqi_f : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_eqi_f" (s, jit_gpr u, jit_fpr v, w);

val jit_eqr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_eqr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_eqr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_eqr_d" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_eqr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_eqr_f" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_extr_c : state * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_extr_c" (s, jit_gpr u, jit_fpr v);

val jit_extr_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_extr_d" (s, jit_fpr u, jit_fpr v);

val jit_extr_d_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_extr_d_f" (s, jit_fpr u, jit_fpr v);

val jit_extr_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_extr_f" (s, jit_fpr u, jit_fpr v);

val jit_extr_f_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_extr_f_d" (s, jit_fpr u, jit_fpr v);

val jit_extr_i : state * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_extr_i" (s, jit_gpr u, jit_fpr v);

val jit_extr_s : state * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_extr_s" (s, jit_gpr u, jit_fpr v);

val jit_extr_uc : state * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_extr_uc" (s, jit_gpr u, jit_fpr v);

val jit_extr_ui : state * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_extr_ui" (s, jit_gpr u, jit_fpr v);

val jit_extr_us : state * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_extr_us" (s, jit_gpr u, jit_fpr v);

val jit_gei : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_gei" (s, jit_gpr u, jit_gpr v, w);

val jit_gei_d : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_gei_d" (s, jit_gpr u, jit_fpr v, w);

val jit_gei_f : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_gei_f" (s, jit_gpr u, jit_fpr v, w);

val jit_gei_u : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_gei_u" (s, jit_gpr u, jit_gpr v, w);

val jit_ger : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ger" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_ger_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ger_d" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_ger_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ger_f" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_ger_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ger_u" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_gti : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_gti" (s, jit_gpr u, jit_gpr v, w);

val jit_gti_d : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_gti_d" (s, jit_gpr u, jit_fpr v, w);

val jit_gti_f : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_gti_f" (s, jit_gpr u, jit_fpr v, w);

val jit_gti_u : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_gti_u" (s, jit_gpr u, jit_gpr v, w);

val jit_gtr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_gtr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_gtr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_gtr_d" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_gtr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_gtr_f" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_gtr_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_gtr_u" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_htonr : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_htonr" (s, jit_gpr u, jit_gpr v);

val jit_jmpi : state -> node
       = fn (s) => lgtapp1 "jit_sml_jmpi" (s);

val jit_jmpr : state * jit_gpr_t -> node
       = fn (s, u) => lgtapp1 "jit_sml_jmpr" (s, jit_gpr u);

val jit_ldi_c : state * jit_gpr_t * Dynlib.cptr -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldi_c" (s, jit_gpr u, v);

val jit_ldi_d : state * jit_fpr_t * Dynlib.cptr -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldi_d" (s, jit_fpr u, v);

val jit_ldi_f : state * jit_fpr_t * Dynlib.cptr -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldi_f" (s, jit_fpr u, v);

val jit_ldi_i : state * jit_gpr_t * Dynlib.cptr -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldi_i" (s, jit_gpr u, v);

val jit_ldi_l : state * jit_gpr_t * Dynlib.cptr -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldi_l" (s, jit_gpr u, v);

val jit_ldi : state * jit_gpr_t * Dynlib.cptr -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldi" (s, jit_gpr u, v);

val jit_ldi_s : state * jit_gpr_t * Dynlib.cptr -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldi_s" (s, jit_gpr u, v);

val jit_ldi_uc : state * jit_gpr_t * Dynlib.cptr -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldi_uc" (s, jit_gpr u, v);

val jit_ldi_ui : state * jit_gpr_t * Dynlib.cptr -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldi_ui" (s, jit_gpr u, v);

val jit_ldi_us : state * jit_gpr_t * Dynlib.cptr -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldi_us" (s, jit_gpr u, v);

val jit_ldr_c : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldr_c" (s, jit_gpr u, jit_gpr v);

val jit_ldr_d : state * jit_fpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldr_d" (s, jit_fpr u, jit_gpr v);

val jit_ldr_f : state * jit_fpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldr_f" (s, jit_fpr u, jit_gpr v);

val jit_ldr_i : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldr_i" (s, jit_gpr u, jit_gpr v);

val jit_ldr_l : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldr_l" (s, jit_gpr u, jit_gpr v);

val jit_ldr : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldr" (s, jit_gpr u, jit_gpr v);

val jit_ldr_s : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldr_s" (s, jit_gpr u, jit_gpr v);

val jit_ldr_uc : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldr_uc" (s, jit_gpr u, jit_gpr v);

val jit_ldr_ui : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldr_ui" (s, jit_gpr u, jit_gpr v);

val jit_ldr_us : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ldr_us" (s, jit_gpr u, jit_gpr v);

val jit_ldxi_c : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxi_c" (s, jit_gpr u, jit_gpr v, w);

val jit_ldxi_d : state * jit_fpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxi_d" (s, jit_fpr u, jit_gpr v, w);

val jit_ldxi_f : state * jit_fpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxi_f" (s, jit_fpr u, jit_gpr v, w);

val jit_ldxi_i : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxi_i" (s, jit_gpr u, jit_gpr v, w);

val jit_ldxi_l : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxi_l" (s, jit_gpr u, jit_gpr v, w);

val jit_ldxi : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxi" (s, jit_gpr u, jit_gpr v, w);

val jit_ldxi_s : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxi_s" (s, jit_gpr u, jit_gpr v, w);

val jit_ldxi_uc : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxi_uc" (s, jit_gpr u, jit_gpr v, w);

val jit_ldxi_ui : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxi_ui" (s, jit_gpr u, jit_gpr v, w);

val jit_ldxi_us : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxi_us" (s, jit_gpr u, jit_gpr v, w);

val jit_ldxr_c : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxr_c" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_ldxr_d : state * jit_fpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxr_d" (s, jit_fpr u, jit_gpr v, jit_gpr w);

val jit_ldxr_f : state * jit_fpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxr_f" (s, jit_fpr u, jit_gpr v, jit_gpr w);

val jit_ldxr_i : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxr_i" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_ldxr_l : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxr_l" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_ldxr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_ldxr_s : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxr_s" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_ldxr_uc : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxr_uc" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_ldxr_ui : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxr_ui" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_ldxr_us : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ldxr_us" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_lei : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_lei" (s, jit_gpr u, jit_gpr v, w);

val jit_lei_d : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_lei_d" (s, jit_gpr u, jit_fpr v, w);

val jit_lei_f : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_lei_f" (s, jit_gpr u, jit_fpr v, w);

val jit_lei_u : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_lei_u" (s, jit_gpr u, jit_gpr v, w);

val jit_ler : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ler" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_ler_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ler_d" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_ler_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ler_f" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_ler_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ler_u" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_live : state * jit_gpr_t -> node
       = fn (s, u) => lgtapp1 "jit_sml_live" (s, jit_gpr u);

val jit_lshi : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_lshi" (s, jit_gpr u, jit_gpr v, w);

val jit_lshr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_lshr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_ltgti_d : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ltgti_d" (s, jit_gpr u, jit_fpr v, w);

val jit_ltgti_f : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ltgti_f" (s, jit_gpr u, jit_fpr v, w);

val jit_ltgtr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ltgtr_d" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_ltgtr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ltgtr_f" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_lti : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_lti" (s, jit_gpr u, jit_gpr v, w);

val jit_lti_d : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_lti_d" (s, jit_gpr u, jit_fpr v, w);

val jit_lti_f : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_lti_f" (s, jit_gpr u, jit_fpr v, w);

val jit_lti_u : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_lti_u" (s, jit_gpr u, jit_gpr v, w);

val jit_ltr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ltr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_ltr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ltr_d" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_ltr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ltr_f" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_ltr_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ltr_u" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_movi : state * jit_gpr_t * word -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_movi" (s, jit_gpr u, v);

val jit_movi_d : state * jit_fpr_t * real -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_movi_d" (s, jit_fpr u, v);

val jit_movi_d_w : state * jit_gpr_t * real -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_movi_d_w" (s, jit_gpr u, v);

val jit_movi_d_ww : state * jit_gpr_t * jit_gpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_movi_d_ww" (s, jit_gpr u, jit_gpr v, w);

val jit_movi_f : state * jit_fpr_t * real -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_movi_f" (s, jit_fpr u, v);

val jit_movi_f_w : state * jit_gpr_t * real -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_movi_f_w" (s, jit_gpr u, v);

val jit_movr : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_movr" (s, jit_gpr u, jit_gpr v);

val jit_movr_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_movr_d" (s, jit_fpr u, jit_fpr v);

val jit_movr_d_w : state * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_movr_d_w" (s, jit_gpr u, jit_fpr v);

val jit_movr_d_ww : state * jit_gpr_t * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_movr_d_ww" (s, jit_gpr u, jit_gpr v, jit_fpr w);

val jit_movr_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_movr_f" (s, jit_fpr u, jit_fpr v);

val jit_movr_f_w : state * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_movr_f_w" (s, jit_gpr u, jit_fpr v);

val jit_movr_w_d : state * jit_fpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_movr_w_d" (s, jit_fpr u, jit_gpr v);

val jit_movr_w_f : state * jit_fpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_movr_w_f" (s, jit_fpr u, jit_gpr v);

val jit_movr_ww_d : state * jit_fpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_movr_ww_d" (s, jit_fpr u, jit_gpr v, jit_gpr w);

val jit_muli : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_muli" (s, jit_gpr u, jit_gpr v, w);

val jit_muli_d : state * jit_fpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_muli_d" (s, jit_fpr u, jit_fpr v, w);

val jit_muli_f : state * jit_fpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_muli_f" (s, jit_fpr u, jit_fpr v, w);

val jit_mulr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_mulr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_mulr_d : state * jit_fpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_mulr_d" (s, jit_fpr u, jit_fpr v, jit_fpr w);

val jit_mulr_f : state * jit_fpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_mulr_f" (s, jit_fpr u, jit_fpr v, jit_fpr w);

val jit_negr : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_negr" (s, jit_gpr u, jit_gpr v);

val jit_negr_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_negr_d" (s, jit_fpr u, jit_fpr v);

val jit_negr_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_negr_f" (s, jit_fpr u, jit_fpr v);

val jit_nei : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_nei" (s, jit_gpr u, jit_gpr v, w);

val jit_nei_d : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_nei_d" (s, jit_gpr u, jit_fpr v, w);

val jit_nei_f : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_nei_f" (s, jit_gpr u, jit_fpr v, w);

val jit_ner : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ner" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_ner_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ner_d" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_ner_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ner_f" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_ntohr : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_ntohr" (s, jit_gpr u, jit_gpr v);

val jit_ordi_d : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ordi_d" (s, jit_gpr u, jit_fpr v, w);

val jit_ordi_f : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ordi_f" (s, jit_gpr u, jit_fpr v, w);

val jit_ordr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ordr_d" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_ordr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ordr_f" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_ori : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ori" (s, jit_gpr u, jit_gpr v, w);

val jit_orr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_orr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_qdivi : state * jit_gpr_t * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, l, h, v, w) => lgtapp1 "jit_sml_qdivi" (s, jit_gpr l, jit_gpr h, jit_gpr v, w);

val jit_qdivi_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, l, h, v, w) => lgtapp1 "jit_sml_qdivi_u" (s, jit_gpr l, jit_gpr h, jit_gpr v, w);

val jit_qdivr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, l, h, v, w) => lgtapp1 "jit_sml_qdivr" (s, jit_gpr l, jit_gpr h, jit_gpr v, jit_gpr w);

val jit_qdivr_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, l, h, v, w) => lgtapp1 "jit_sml_qdivr_u" (s, jit_gpr l, jit_gpr h, jit_gpr v, jit_gpr w);

val jit_qmuli : state * jit_gpr_t * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, l, h, v, w) => lgtapp1 "jit_sml_qmuli" (s, jit_gpr l, jit_gpr h, jit_gpr v, w);

val jit_qmuli_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, l, h, v, w) => lgtapp1 "jit_sml_qmuli_u" (s, jit_gpr l, jit_gpr h, jit_gpr v, w);

val jit_qmulr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, l, h, v, w) => lgtapp1 "jit_sml_qmulr" (s, jit_gpr l, jit_gpr h, jit_gpr v, jit_gpr w);

val jit_qmulr_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, l, h, v, w) => lgtapp1 "jit_sml_qmulr_u" (s, jit_gpr l, jit_gpr h, jit_gpr v, jit_gpr w);

val jit_remi : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_remi" (s, jit_gpr u, jit_gpr v, w);

val jit_remi_u : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_remi_u" (s, jit_gpr u, jit_gpr v, w);

val jit_remr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_remr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_remr_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_remr_u" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_rshi : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_rshi" (s, jit_gpr u, jit_gpr v, w);

val jit_rshi_u : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_rshi_u" (s, jit_gpr u, jit_gpr v, w);

val jit_rshr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_rshr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_rshr_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_rshr_u" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_sqrtr_d : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_sqrtr_d" (s, jit_fpr u, jit_fpr v);

val jit_sqrtr_f : state * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_sqrtr_f" (s, jit_fpr u, jit_fpr v);

val jit_sti_c : state * Dynlib.cptr * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_sti_c" (s, u, jit_gpr v);

val jit_sti_d : state * Dynlib.cptr * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_sti_d" (s, u, jit_fpr v);

val jit_sti_f : state * Dynlib.cptr * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_sti_f" (s, u, jit_fpr v);

val jit_sti_i : state * Dynlib.cptr * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_sti_i" (s, u, jit_gpr v);

val jit_sti_l : state * Dynlib.cptr * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_sti_l" (s, u, jit_gpr v);

val jit_sti : state * Dynlib.cptr * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_sti" (s, u, jit_gpr v);

val jit_sti_s : state * Dynlib.cptr * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_sti_s" (s, u, jit_gpr v);

val jit_str_c : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_str_c" (s, jit_gpr u, jit_gpr v);

val jit_str_d : state * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_str_d" (s, jit_gpr u, jit_fpr v);

val jit_str_f : state * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_str_f" (s, jit_gpr u, jit_fpr v);

val jit_str_i : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_str_i" (s, jit_gpr u, jit_gpr v);

val jit_str_l : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_str_l" (s, jit_gpr u, jit_gpr v);

val jit_str : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_str" (s, jit_gpr u, jit_gpr v);

val jit_str_s : state * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_str_s" (s, jit_gpr u, jit_gpr v);

val jit_stxi_c : state * word * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_stxi_c" (s, u, jit_gpr v, jit_gpr w);

val jit_stxi_d : state * word * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_stxi_d" (s, u, jit_gpr v, jit_fpr w);

val jit_stxi_f : state * word * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_stxi_f" (s, u, jit_gpr v, jit_fpr w);

val jit_stxi_i : state * word * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_stxi_i" (s, u, jit_gpr v, jit_gpr w);

val jit_stxi_l : state * word * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_stxi_l" (s, u, jit_gpr v, jit_gpr w);

val jit_stxi : state * word * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_stxi" (s, u, jit_gpr v, jit_gpr w);

val jit_stxi_s : state * word * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_stxi_s" (s, u, jit_gpr v, jit_gpr w);

val jit_stxr_c : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_stxr_c" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_stxr_d : state * jit_gpr_t * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_stxr_d" (s, jit_gpr u, jit_gpr v, jit_fpr w);

val jit_stxr_f : state * jit_gpr_t * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_stxr_f" (s, jit_gpr u, jit_gpr v, jit_fpr w);

val jit_stxr_i : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_stxr_i" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_stxr_l : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_stxr_l" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_stxr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_stxr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_stxr_s : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_stxr_s" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_subci : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_subci" (s, jit_gpr u, jit_gpr v, w);

val jit_subcr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_subcr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_subi : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_subi" (s, jit_gpr u, jit_gpr v, w);

val jit_subi_d : state * jit_fpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_subi_d" (s, jit_fpr u, jit_fpr v, w);

val jit_subi_f : state * jit_fpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_subi_f" (s, jit_fpr u, jit_fpr v, w);

val jit_subr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_subr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_subr_d : state * jit_fpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_subr_d" (s, jit_fpr u, jit_fpr v, jit_fpr w);

val jit_subr_f : state * jit_fpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_subr_f" (s, jit_fpr u, jit_fpr v, jit_fpr w);

val jit_subxi : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_subxi" (s, jit_gpr u, jit_gpr v, w);

val jit_subxr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_subxr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

val jit_truncr_d_i : state * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_truncr_d_i" (s, jit_gpr u, jit_fpr v);

val jit_truncr_d_l : state * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_truncr_d_l" (s, jit_gpr u, jit_fpr v);

val jit_truncr_d : state * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_truncr_d" (s, jit_gpr u, jit_fpr v);

val jit_truncr_f_i : state * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_truncr_f_i" (s, jit_gpr u, jit_fpr v);

val jit_truncr_f_l : state * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_truncr_f_l" (s, jit_gpr u, jit_fpr v);

val jit_truncr_f : state * jit_gpr_t * jit_fpr_t -> node
       = fn (s, u, v) => lgtapp1 "jit_sml_truncr_f" (s, jit_gpr u, jit_fpr v);

val jit_uneqi_d : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_uneqi_d" (s, jit_gpr u, jit_fpr v, w);

val jit_uneqi_f : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_uneqi_f" (s, jit_gpr u, jit_fpr v, w);

val jit_uneqr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_uneqr_d" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_uneqr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_uneqr_f" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_ungei_d : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ungei_d" (s, jit_gpr u, jit_fpr v, w);

val jit_ungei_f : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ungei_f" (s, jit_gpr u, jit_fpr v, w);

val jit_unger_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_unger_d" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_unger_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_unger_f" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_ungti_d : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ungti_d" (s, jit_gpr u, jit_fpr v, w);

val jit_ungti_f : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ungti_f" (s, jit_gpr u, jit_fpr v, w);

val jit_ungtr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ungtr_d" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_ungtr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_ungtr_f" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_unlei_d : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_unlei_d" (s, jit_gpr u, jit_fpr v, w);

val jit_unlei_f : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_unlei_f" (s, jit_gpr u, jit_fpr v, w);

val jit_unler_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_unler_d" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_unler_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_unler_f" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_unlti_d : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_unlti_d" (s, jit_gpr u, jit_fpr v, w);

val jit_unlti_f : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_unlti_f" (s, jit_gpr u, jit_fpr v, w);

val jit_unltr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_unltr_d" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_unltr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_unltr_f" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_unordi_d : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_unordi_d" (s, jit_gpr u, jit_fpr v, w);

val jit_unordi_f : state * jit_gpr_t * jit_fpr_t * real -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_unordi_f" (s, jit_gpr u, jit_fpr v, w);

val jit_unordr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_unordr_d" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_unordr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_unordr_f" (s, jit_gpr u, jit_fpr v, jit_fpr w);

val jit_xori : state * jit_gpr_t * jit_gpr_t * word -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_xori" (s, jit_gpr u, jit_gpr v, w);

val jit_xorr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
       = fn (s, u, v, w) => lgtapp1 "jit_sml_xorr" (s, jit_gpr u, jit_gpr v, jit_gpr w);

