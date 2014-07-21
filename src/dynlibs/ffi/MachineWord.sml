structure MachineWord :> WordPrim =
struct
   local
       prim_eqtype array_;
       prim_val array_   : int -> array_                 = 1 "create_string";
       prim_val fill_    : array_ -> int -> int -> int -> unit 
                                                      = 4 "fill_string";
       type array = array_ ref;
   in
      type word = (array * int * int)
      type largeword = word
      type excn = Word.word
      val wordSize = Jit.WORDSIZE;
      local
         val WORDSIZE = Jit.WORDSIZE
         val wsz = WORDSIZE div 8
         prim_val fromWord8ArraySlice_ : Word8ArraySlice.slice -> word = 1 "identity";
         prim_val toWord8ArraySlice_ : word -> Word8ArraySlice.slice = 1 "identity";
         fun mkVal () = 
             let val a = array_ wsz 
             in fill_ a 0 wsz 0;
                (ref a, 0, wsz)
              end;
         fun dnz jit_binop (jit_, r3, r1, r2) =
            let open Jit
                val raiseprimitive0 = Jit.Pointer (Ffi.svec_getcptrvalue Ffi.raiseprimitive0)
                val ref' = jit_bnei (jit_, r2, 0);
                val _ = jit_prepare (jit_);
                val _ = jit_pushargi (jit_, 0w28); (* excn Div defined in globals.h *)
                val _ = jit_finishi (jit_,raiseprimitive0);
         
                val () = jit_patch (jit_, ref');
                val _ = jit_binop (jit_, r3, r1, r2);
            in ()
            end
         fun binop jit_binop = 
         let open Jit
            val jit_ = jit_new_state ();
            val slccall = jit_label (jit_);
            val () = jit_prolog (jit_);
            val v = jit_arg (jit_);
         
            val () = jit_getarg (jit_, V0, v);
            val _  = jit_ldxi (jit_, V1, V0, wsz * 1); (* V1 = Field(v,1) *)
            val _  = jit_ldxi (jit_, R1, V1, 0); (* R1 = Field(V1,0) *)
            val _  = jit_ldxi (jit_, V2, R1, 0); (* V2 = Field(R1,0) *)
            val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
            val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
            val _  = jit_ldxr (jit_, R1, V2, R0); (* R1 = *(Field(V1,0)+Field(V1,1)) *)
         
            val _  = jit_ldxi (jit_, V1, V0, wsz * 2); (* V1 = Field(v,2) *)
            val _  = jit_ldxi (jit_, R2, V1, 0); (* R2 = Field(V1,0) *)
            val _  = jit_ldxi (jit_, V2, R2, 0); (* V2 = Field(R2,0) *)
            val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
            val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
            val _  = jit_ldxr (jit_, R2, V2, R0); (* R2 = *(Field(V1,0)+Field(V1,1)) *)
         
            val _ = jit_binop (jit_, R2, R1, R2);
         
            val _  = jit_ldxi (jit_, V1, V0, 0); (* V1 = Field(v,0) *)
            val _  = jit_ldxi (jit_, R1, V1, 0); (* R1 = Field(V1,0) *)
            val _  = jit_ldxi (jit_, V2, R1, 0); (* V2 = Field(R1,0) *)
            val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
            val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
            val _  = jit_stxr (jit_, R0, V2, R2); (* *(Field(V1,0)+Field(V1,1)) = R2 *)
         
            val _ = jit_ret(jit_);
         
            val slccallptr = jit_emit (jit_);
         
            val slccall : word * word * word -> unit = Ffi.app1 slccallptr
         in
            slccall
         end
         fun unop jit_unop = 
         let open Jit
            val jit_ = jit_new_state ();
            val slccall = jit_label (jit_);
            val () = jit_prolog (jit_);
            val v = jit_arg (jit_);
         
            val () = jit_getarg (jit_, V0, v);
            val _  = jit_ldxi (jit_, V1, V0, wsz * 1); (* V1 = Field(v,1) *)
            val _  = jit_ldxi (jit_, R1, V1, 0); (* R1 = Field(V1,0) *)
            val _  = jit_ldxi (jit_, V2, R1, 0); (* V2 = Field(R1,0) *)
            val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
            val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
            val _  = jit_ldxr (jit_, R1, V2, R0); (* R1 = *(Field(V1,0)+Field(V1,1)) *)
         
            val _ = jit_unop (jit_, R2, R1);
         
            val _  = jit_ldxi (jit_, V1, V0, 0); (* V1 = Field(v,0) *)
            val _  = jit_ldxi (jit_, R1, V1, 0); (* R1 = Field(V1,0) *)
            val _  = jit_ldxi (jit_, V2, R1, 0); (* V2 = Field(R1,0) *)
            val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
            val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
            val _  = jit_stxr (jit_, R0, V2, R2); (* *(Field(V1,0)+Field(V1,1)) = R2 *)
         
            val _ = jit_ret(jit_);
         
            val slccallptr = jit_emit (jit_);
         
            val slccall : word * word -> unit = Ffi.app1 slccallptr
         in
            slccall
         end
         val toInt = 
         let open Jit
            val jit_ = jit_new_state ();
            val slccall = jit_label (jit_);
            val () = jit_prolog (jit_);
            val v = jit_arg (jit_);
         
            val () = jit_getarg (jit_, V0, v);
            val _  = jit_ldxi (jit_, R1, V0, 0); (* R1 = Field(V0,0) *)
            val _  = jit_ldxi (jit_, V2, R1, 0); (* V2 = Field(R1,0) *)
            val _  = jit_ldxi (jit_, R0, V0, wsz * 1); (* R0 = Field(V0,1) *)
            val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
            val _  = jit_ldxr (jit_, R1, V2, R0); (* R1 = *(Field(R1,0)+Field(V0,1)) *)
         
            val _ = jit_lshi (jit_, R1, R1, 1);
            val _ = jit_addi (jit_, R1, R1, 1); (* Val_long(R1) *)
         
            val _ = jit_retr(jit_,R1);
         
            val tointptr = jit_emit (jit_);
         
            val tointcall : word -> int = Ffi.app1 tointptr
         in
            tointcall
         end
         val fromInt_ =
         let open Jit
            val jit_ = jit_new_state ();
            val slccall = jit_label (jit_);
            val () = jit_prolog (jit_);
            val v = jit_arg (jit_);
         
            val () = jit_getarg (jit_, V0, v);
         
            val _  = jit_ldxi (jit_, R2, V0, 1 * wsz); (* R2 = Field(v,1) *)
            val _  = jit_rshi (jit_, R2, R2, 1); (* Long_val(R2) *)
         
            val _  = jit_ldxi (jit_, V1, V0, 0); (* V1 = Field(v,0) *)
            val _  = jit_ldxi (jit_, R1, V1, 0); (* R1 = Field(V1,0) *)
            val _  = jit_ldxi (jit_, V2, R1, 0); (* V2 = Field(R1,0) *)
            val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
            val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
            val _  = jit_stxr (jit_, R0, V2, R2); (* *(Field(V1,0)+Field(V1,1)) = R2 *)
         
            val _ = jit_ret(jit_);
         
            val slccallptr = jit_emit (jit_);
         
            val slccall : word * int -> unit = Ffi.app1 slccallptr
         in
            slccall
         end
         fun jit_atom (jit_, r1, r2) =
            let open Jit
                val first_atoms = Pointer (Ffi.svec_getcptrvalue Ffi.first_atoms_)
                val _ = jit_muli (jit_, r2, r2, wsz)
                val _ = jit_movi_p (jit_, r1, first_atoms)
                val _ = jit_addi (jit_, r1, r1, wsz * 1)
                val _ = jit_addr (jit_, r1, r1, r2)
            in ()
            end
         fun relop jit_relop = 
         let open Jit
            val jit_ = jit_new_state ();
            val slccall = jit_label (jit_);
            val () = jit_prolog (jit_);
            val v = jit_arg (jit_);
         
            val () = jit_getarg (jit_, V0, v);
            val _  = jit_ldxi (jit_, V1, V0, 0); (* V1 = Field(v,0) *)
            val _  = jit_ldxi (jit_, R1, V1, 0); (* R1 = Field(V1,0) *)
            val _  = jit_ldxi (jit_, V2, R1, 0); (* V2 = Field(R1,0) *)
            val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
            val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
            val _  = jit_ldxr (jit_, R1, V2, R0); (* R1 = *(Field(V1,0)+Field(V1,1)) *)
         
            val _  = jit_ldxi (jit_, V1, V0, wsz * 1); (* V1 = Field(v,1) *)
            val _  = jit_ldxi (jit_, R2, V1, 0); (* R2 = Field(V1,0) *)
            val _  = jit_ldxi (jit_, V2, R2, 0); (* V2 = Field(R2,0) *)
            val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
            val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
            val _  = jit_ldxr (jit_, R2, V2, R0); (* R2 = *(Field(V1,0)+Field(V1,1)) *)
         
            val _ = jit_relop (jit_, R2, R1, R2);
            val _ = jit_atom (jit_, R1, R2);
            val _ = jit_retr(jit_, R1);
         
            val slccallptr = jit_emit (jit_);
         
            val slccall : word * word -> bool = Ffi.app1 slccallptr
         in
            slccall
         end
         val slcadd = binop Jit.jit_addr
         val slcsub = binop Jit.jit_subr
         val slcmul = binop Jit.jit_mulr
         val slcdiv = binop (dnz Jit.jit_divr_u)
         val slcmod = binop (dnz Jit.jit_remr_u)
         val slcrsh = binop Jit.jit_rshr
         val slcrsh_u = binop Jit.jit_rshr_u
         val slclsh = binop Jit.jit_lshr
         val slcand = binop Jit.jit_andr
         val slcor = binop Jit.jit_orr
         val slcxor = binop Jit.jit_xorr
         val slcnot = unop Jit.jit_comr
         val slcneg = unop Jit.jit_negr
         val slceq = relop Jit.jit_eqr
         val slclt = relop Jit.jit_ltr_u
         val slcle = relop Jit.jit_ler_u
         val slcgt = relop Jit.jit_gtr_u
         val slcge = relop Jit.jit_ger_u
         
         fun applybin opfn (x,y) =
            let val res = mkVal ()
            in opfn (res,x,y);
              res
            end;
         fun applyun opfn x =
            let val res = mkVal ()
            in opfn (res,x);
              res
            end;
      in
             val toInt = toInt;
             val toIntX = toInt;
             val fromInt = applyun fromInt_
             val toLargeInt = toInt;
             val toLargeIntX = toInt;
             val fromLargeInt = fromInt;

             val fromWord8ArraySlice = fromWord8ArraySlice_; 
             val toWord8ArraySlice = toWord8ArraySlice_; 
         
             prim_val toLargeWord   : word -> word = 1 "identity";
             prim_val toLargeWordX  : word -> word = 1 "identity";
             prim_val fromLargeWord : word -> word = 1 "identity";
         
             prim_val toLarge   : word -> word = 1 "identity";
             prim_val toLargeX  : word -> word = 1 "identity";
             prim_val fromLarge : word -> word = 1 "identity";
         
             fun orb (x, y)  = applybin slcor (x, y)
             fun andb (x, y) = applybin slcand (x, y)
             fun xorb (x, y) = applybin slcxor (x, y)
             fun notb x      = applyun slcnot x
         
             val ~ = fn w => applyun slcneg w
         
             fun << (w, k) = 
               	if toInt k >= WORDSIZE orelse toInt k < 0 then fromInt 0
               	else applybin slclsh (w,k)
               
             fun >> (w, k) = 
               	if toInt k >= WORDSIZE orelse toInt k < 0 then fromInt 0
               	else applybin slcrsh_u (w,k)
               
             fun ~>> (w, k) = 
               	if toInt k >= WORDSIZE orelse toInt k < 0 then 
               	    if slceq (op >>(w,fromInt (WORDSIZE-1)),fromInt 0) then	(* msbit = 0 *)
               		fromInt 0
               	    else			(* msbit = 1 *)
               		fromInt ~1
               	else	
               	     applybin slcrsh (w,k)
         
             val op *    : word * word -> word = applybin slcmul;
             val op +    : word * word -> word = applybin slcadd;
             val op -    : word * word -> word = applybin slcsub;
             val op div  : word * word -> word = applybin slcdiv;
             val op mod  : word * word -> word = applybin slcmod;
         
             val MAXPOS = (<< (fromInt 1,fromInt (op Int.- (WORDSIZE,2)))) - fromInt 1;

             val op >    : word * word -> bool = slcgt;
             val op >=   : word * word -> bool = slcge;
             val op <    : word * word -> bool = slclt;
             val op <=   : word * word -> bool = slcle;
             val eq : word * word -> bool = slceq;
      end
   end
end
