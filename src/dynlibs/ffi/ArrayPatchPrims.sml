signature ArrayPatchPrims =
sig
   type jit
   type slice
   type exptn
   type array
   type elem
   type address
   type label
   type ptype
   type phint
   type pval
   type param

   type oper

   val add : oper
   val adc : oper
   val adx : oper
   val sub : oper
   val rsh : oper
   val asr : oper
   val lsh : oper
   val cnj : oper
   val xor : oper
   val dsj : oper
   val com : oper

   val boole_t : ptype
   val octet_t : ptype
   val int8_t : ptype
   val int16_t : ptype
   val int32_t : ptype
   val int64_t : ptype
   val int16u_t : ptype
   val int32u_t : ptype
   val int64u_t : ptype
   val float_t : ptype
   val double_t  : ptype
   val address_t : ptype
   val elem_t    : ptype

   val mkBoole : bool -> pval
   val mkElem : elem -> pval
   val mkOctet : word -> pval (* unsigned int8 *)
   val mkInt8 : int -> pval

   val mkInt16 : int -> pval
   val mkIntML : int -> pval (* makes int32 on 32 bit, int64 on 64 bit *)
   val mkInt32 : int * int -> pval (* 2 of int16 *)
   val mkInt64 : int * int * int * int -> pval (* 4 of int16 *)

   val mkInt16u : word -> pval
   val mkIntMLu : word -> pval (* makes int32u on 32 bit, int64u on 64 bit *)
   val mkInt32u : word * word -> pval (* 2 of int16u *)
   val mkInt64u : word * word * word * word -> pval (* 4 of int16u *)

   val mkFloat : real -> pval
   val mkDouble : real -> pval

   val mkAddress : word * word -> pval (* 2 machine half-words:
                                          i.e. 2 of int16u(int32u) on 32(64) bit *)

   val loopctr : phint
   val loopcnd : phint
   val accum   : phint
   val basesrc : phint
   val basedst : phint
   val idxoffs : phint

   val raiseExcn1 : jit * exptn * param -> jit
   val raiseExcn0 : jit * exptn -> jit

   val allocl : jit * ptype * int -> jit * address
   val addoffs : address * ptype * int -> address

   val static : jit * ptype * int * (int -> pval) -> jit * address

   val newparam   : jit * ptype -> param
   val loadparam  : jit * address * param -> jit
   val loadparami : jit * pval * param -> jit
   val storeparam : jit * address * param -> jit

   val getelt : jit * param * int * param -> jit
   val putelt : jit * param * int * param -> jit

   val newlabel : jit -> jit * label
   val target : jit * label -> jit

   val jump :  jit * label -> jit
   val jnz :  jit * param * label * bool -> jit
   val jz : jit * param * label * bool -> jit

   val binop : jit * oper * param * param * param -> jit
   val unop : jit * oper * param * param -> jit
   val nop : jit * oper * param Vector.vector -> jit

   val ifthen : jit * (jit * param -> unit) * bool * jit -> jit
   val ifelse : jit * (jit * param -> unit) * bool * jit * jit -> jit
   val dowhile : jit * (jit * param -> unit) * int * jit -> jit
   val whiledo : jit * (jit * param -> unit) * int * jit -> jit
end

structure ArrayPatchPrims =
struct
   type slice
   type exptn
   type array
   type elem
   type address
   type label
   type phint
   type param = int * ptype

   datatype pvalt = 
      BooleVal of bool
    | ElemVal of elem
    | OctetVal of Word8.word
    | Int8Val of Word8.word
    | IntTwoOfVal of pvalt * pvalt
    | DoubleVal of real
    | PointerVal of pvalt

   datatype ptypet = 
      Boole
    | Elem
    | Octet
    | Int8
    | IntTwoOf
    | Double of pvaldt
    | Pointer of pvaldt

   type pval = pvalt
   type ptype = ptypet

   val mkInt16u : word -> pval
   val mkIntMLu : word -> pval (* makes int32u on 32 bit, int64u on 64 bit *)
   val mkInt32u : word * word -> pval (* 2 of int16u *)
   val mkInt64u : word * word * word * word -> pval (* 4 of int16u *)

   val mkFloat : real -> pval
   val mkDouble : real -> pval

   val mkAddress : word * word -> pval (* 2 machine half-words:
                                          i.e. 2 of int16u(int32u) on 32(64) bit *)


   type jit = { jit_ : Lightning.state,
                labels : label list ref, 
                targets : (label * address) list ref,
                params : (param * ptype)

   type oper

   val add : oper
   val adc : oper
   val adx : oper
   val sub : oper
   val rsh : oper
   val asr : oper
   val lsh : oper
   val cnj : oper
   val xor : oper
   val dsj : oper
   val com : oper

   val boole_t : ptype
   val octet_t : ptype
   val int8_t : ptype
   val int16_t : ptype
   val int32_t : ptype
   val int64_t : ptype
   val int16u_t : ptype
   val int32u_t : ptype
   val int64u_t : ptype
   val float_t : ptype
   val double_t  : ptype
   val address_t : ptype
   val elem_t    : ptype

end
