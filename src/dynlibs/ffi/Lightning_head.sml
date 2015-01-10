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

