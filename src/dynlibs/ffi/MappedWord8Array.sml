(* Static (not on ML heap, so not GCed) Word8Array --
   from SML Basis Library -- then hacked about some. *)

structure MappedWord8Array :> MappedWord8Array =
struct
   type elem   = Word8.word
   type vector = Word8Vector.vector
   prim_type array_
   prim_eqtype array__
   type array = array__ ref
   local

      val dlih = Dynlib.dlopen {lib = "libcinfo.so",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }

      fun syminf s = Dynlib.dlsym dlih s
      fun syminfp s = Dynlib.cptr (syminf s)

      val slfh =
          Dynlib.dlopen {lib = "",
                         flag = Dynlib.RTLD_LAZY,
                         global = false }

      fun slfsym s = Dynlib.dlsym slfh s
      fun slfsymp s = Dynlib.cptr (slfsym s)

      val app1 : string -> 'a -> 'b
           = fn s => Dynlib.app1 (slfsym s)
      val app2 : string -> 'a -> 'b -> 'c
           = fn s => Dynlib.app2 (slfsym s)
      val app3 : string -> 'a -> 'b -> 'c -> 'd
           = fn s => Dynlib.app3 (slfsym s)
      val app4 : string -> 'a -> 'b -> 'c -> 'd -> 'e
           = fn s => Dynlib.app4 (slfsym s)
      val app5 : string -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
           = fn s => Dynlib.app5 (slfsym s)

      val alloc_finalp = slfsymp "alloc_final"
      val initializep = slfsymp "initialize"
      val mallocp = slfsymp "malloc"
      val freep = slfsymp "free"
      val my_allocp = syminfp "my_alloc"
      val my_freep = syminfp "my_free"
      val memdebug_final1p = syminfp "memdebug_finalize1"
      val memdebug_final2p = syminfp "memdebug_finalize2"

      open Lightning32

      val () = init_jit()

      val jit_ = jit_new_state()

      fun jit_fprolog jit_ =
         jit_note (jit_,NULL,0w0)
         before jit_prolog (jit_)

      val wsz = Word.fromInt (WORDSIZE div 8)

      local
         val minInt = Option.valOf Int.minInt
         val spr = ref (~1)
         val max = ref (minInt)
         val wsz = (Word.toInt wsz)
      in
         fun jit_alloc_local (jit_, n) =
            let val sp = jit_allocai (jit_, Word.fromInt (n * wsz))
                val _ = max := sp + (n *  wsz)
                val _ = spr := sp
            in ()
            end
         fun jit_lvar (jit_, n) =
            let val sp = !spr
                val sp' = sp + (n * wsz)
            in if !max = minInt
                  then raise Fail "jit_lvar: local variables unallocated"
                  else if sp' > (!max)
                          then raise Fail "jit_lvar: local variable allocation exceeded"
                          else sp before spr := sp'
            end
         fun jit_lass (jit_, v, r0) =
            let val _ = jit_stxi (jit_, Word.fromInt v, FP, r0)
            in ()
            end
         fun jit_lref (jit_, r0, v) =
            let val _ = jit_ldxi (jit_, r0, FP, Word.fromInt v)
            in ()
            end
      end
    (* Field(v,0) is the finalize wrapper fn ptr (this fn)
       Field(v,1) is the object pointer reference
                       (possibly pointing to a locn _within_ the allocated block)
       Field(v,2) is the length of the contained object as an ML word value

           ML side
       ---------------------------------------------------------------------------
           C side

       Field(v,3) is the real finalize function pointer (munmap, say)

       Field(v,4) is the container
                              (i.e. the real pointer to the allocated block/page)
       Field(v,5) is the size (as a C long value) of the container
                     (i.e. the size_t size of the actual block allocated/mmapped) *)

      val alloc_final_call = jit_fprolog (jit_)
      val v = jit_arg (jit_)

      val _ = jit_alloc_local(jit_,2)
      val rv = jit_lvar(jit_,1)
      val vp = jit_lvar(jit_,1)

      val () = jit_getarg (jit_, V0, v)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0w0) (* V1 = Field(v,0) *)
      val _  = jit_rshi (jit_, V1, V1, 0w1); (* V1 = Long_val(V1) *)
      val _ = jit_ldxi (jit_, V2, V0, wsz * 0w1) (* V1 = Field(v,1) *)
      val _ = jit_ldxi (jit_, R0, V0, wsz * 0w2) (* R0 = Field(v,2) *)
      val _  = jit_rshi (jit_, R0, R0, 0w1); (* R0 = Long_val(R0) *)
      val _ = jit_ldxi (jit_, R1, V0, wsz * 0w3) (* R0 = Field(v,2) *)
      val _  = jit_rshi (jit_, R1, R1, 0w1); (* R1 = Long_val(R1) *)

      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_pushargr (jit_, V2)
      val _ = jit_pushargr (jit_, R0)
      val _ = jit_pushargr (jit_, R1)
      val _ = jit_finishi (jit_, alloc_finalp)
      val _ = jit_retval (jit_,R0)

      val _ = jit_lass(jit_,rv,R0)

      val () = jit_getarg (jit_, V0, v)
      val _ = jit_ldxi (jit_, V0, V0, wsz * 0w4) (* V0 = Field(v,4) *)

      val _ = jit_lass(jit_,vp,V0)

      val _ = jit_addi (jit_, R1, R0, wsz * 0w1) (* R1 = &Field(R0,1) *)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0w0) (* V1 = Field(V0,0) *)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, R1)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_finishi (jit_, initializep)

      val _ = jit_lref (jit_,R0,rv)
      val _ = jit_lref (jit_,V0,vp)

      val _ = jit_addi (jit_, R1, R0, wsz * 0w2) (* R1 = &Field(R0,2) *)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0w1) (* V1 = Field(V0,1) *)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, R1)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_finishi (jit_, initializep)

      val _ = jit_lref (jit_,R0,rv)
      val _ = jit_lref (jit_,V0,vp)

      val _ = jit_addi (jit_, R1, R0, wsz * 0w3) (* R1 = &Field(R0,3) *)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0w2) (* V1 = Field(V0,2) *)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, R1)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_finishi (jit_, initializep)

      val _ = jit_lref (jit_,R0,rv)
      val _ = jit_lref (jit_,V0,vp)

      val _ = jit_addi (jit_, R1, R0, wsz * 0w4) (* R1 = &Field(R0,3) *)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0w3) (* V1 = Field(V0,2) *)

      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, R1)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_finishi (jit_, initializep)

      val _ = jit_lref (jit_,R0,rv)
      val _ = jit_lref (jit_,V0,vp)

      val _ = jit_addi (jit_, R1, R0, wsz * 0w5) (* R1 = &Field(R0,3) *)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0w4) (* V1 = Field(V0,2) *)
      val _  = jit_rshi (jit_, V1, V1, 0w1); (* V1 = Long_val(V1) *)

      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, R1)
      val _ = jit_pushargr (jit_, V1)
      val _ = jit_finishi (jit_, initializep)

      val _ = jit_lref (jit_,R0,rv)
      val _ = jit_retr (jit_,R0)

      val _ = jit_epilog (jit_)

      val get_array_call = jit_fprolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0w1) (* V1 = Field(v,1) *)
      val _ = jit_retr (jit_,V1)

      val _ = jit_epilog (jit_)

      val get_length_call = jit_fprolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0w2) (* V1 = Field(v,2) *)
      val _ = jit_retr (jit_,V1)

      val _ = jit_epilog (jit_)

      val malloc_call = jit_fprolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _  = jit_rshi (jit_, V0, V0, 0w1); (* Long_val(V0) *)
 
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, V0)
      val _ = jit_finishi (jit_, my_allocp)
      val _ = jit_retval (jit_,V0)
      val _ = jit_retr (jit_,V0)

      val _ = jit_epilog (jit_)

      val free_call = jit_fprolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
 
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, V0)
      val _ = jit_finishi (jit_, my_freep)
      val _ = jit_ret (jit_)

      val _ = jit_epilog (jit_)

      val final2_call = jit_fprolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0w3) (* V1 = Field(v,3) *)
      val _ = jit_ldxi (jit_, R0, V0, wsz * 0w4) (* R0 = Field(v,4) *)
      val _ = jit_ldxi (jit_, R1, V0, wsz * 0w5) (* R1 = Field(v,5) *)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, R0)
      val _ = jit_pushargr (jit_, R1)
      val _ = jit_finishr (jit_, V1)
      val _ = jit_ret (jit_)

      val _ = jit_epilog (jit_)

      val final1_call = jit_fprolog (jit_)
      val v = jit_arg (jit_)
      val () = jit_getarg (jit_, V0, v)
      val _ = jit_ldxi (jit_, V1, V0, wsz * 0w3) (* V1 = Field(v,3) *)
      val _ = jit_ldxi (jit_, R0, V0, wsz * 0w4) (* R0 = Field(v,4) *)
      val _ = jit_prepare (jit_)
      val _ = jit_pushargr (jit_, R0)
      val _ = jit_finishr (jit_, V1)
      val _ = jit_ret (jit_)

      val _ = jit_epilog (jit_)

      val _ = jit_emit (jit_)

      val get_length_add = jit_address (jit_,get_length_call)

      val get_array_add = jit_address (jit_,get_array_call)

      val final2_add = jit_address (jit_,final2_call)

      val final1_add = jit_address (jit_,final1_call)

      val malloc_add = jit_address (jit_,malloc_call)

      val free_add = jit_address (jit_,free_call)

      val alloc_final_add = jit_address (jit_,alloc_final_call)

      val () = jit_clear_state (jit_)

      val get_array_ : array__ ref -> array_ =
          fn (ref v) =>
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in app1 get_array_add v
          end
      val get_length_ : array__ -> int =
          fn v =>
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in app1 get_length_add v
          end
      val final2_ : (cptr * Obj.obj * Obj.obj) -> unit =
          fn v =>
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in app1 final2_add v
          end
      val final1_ : (cptr * Obj.obj) -> unit =
          fn v =>
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in app1 final1_add v
          end
      val malloc_ : int -> Dynlib.cptr =
          fn v =>
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in app1 malloc_add v
          end
      val free_ : Dynlib.cptr -> unit =
          fn v =>
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in (app1 free_add v;())
          end
      val alloc_final_ : (int * Dynlib.cptr * int * int * 
                           (Dynlib.cptr * int * Dynlib.cptr * Dynlib.cptr * int)) -> array__ =
          fn v =>
          let val jitref = jit_  (* to prevent the machine code from being GCed *) 
          in app1 alloc_final_add v
          end
      val to_cptr : array_ -> Dynlib.cptr =
         fn a_ => 
           let val cptr : Dynlib.cptr = Obj.magic a_
         in cptr end

      prim_val vector_  : int -> vector                 = 1 "create_string";
      prim_val lengthv_ : vector -> int                 = 1 "string_length";

      prim_val blitva__  : vector -> int -> Dynlib.cptr -> int -> int -> unit 
                                                        = 5 "blit_string";
      prim_val blitav__  : Dynlib.cptr -> int -> vector -> int -> int -> unit 
                                                        = 5 "blit_string";
      prim_val sub__     : Dynlib.cptr -> int -> elem         = 2 "get_nth_char";

      prim_val update__  : Dynlib.cptr -> int -> elem -> unit = 3 "set_nth_char";

      prim_val fill__    : Dynlib.cptr -> int -> int -> elem -> unit 
                                                            = 4 "fill_string";
      prim_val blitaa__  : Dynlib.cptr -> int -> Dynlib.cptr -> int -> int -> unit 
                                                            = 5 "blit_string";
      val sub_     : array_ -> int -> elem
             = fn a => sub__ (to_cptr a)
      val update_  : array_ -> int -> elem -> unit
             = fn a => update__ (to_cptr a)
      val fill_    : array_ -> int -> int -> elem -> unit 
             = fn a => fill__ (to_cptr a)
      val blitaa_  : array_ -> int -> array_ -> int -> int -> unit 
             = fn a => fn i => fn b => blitaa__ (to_cptr a) i (to_cptr b)
      val blitva_  : vector -> int -> array_ -> int -> int -> unit 
             = fn v => fn i => fn a => blitva__ v i (to_cptr a)
      val blitav_  : array_ -> int -> vector -> int -> int -> unit 
             = fn a => blitav__ (to_cptr a)
      val WORDSIZE : word = Word.fromInt Lightning32.WORDSIZE
   in val malloc : Int.int -> Dynlib.cptr = malloc_
      val free : Dynlib.cptr -> unit = free_
      fun alloc_final ((cptrfn,nargs,cptrblk,len)
            : Dynlib.cptr * Int.int * Dynlib.cptr * int) =
         let val cptrfin = if nargs = 2
                              then  final2_add
                              else  final1_add
             val arr = alloc_final_ (6,cptrfin,6,50000,(cptrblk, len, cptrfn, cptrblk, len))
         in ref arr
         end
      fun array_ n =
         let val cptrblk = malloc_ n
         in if ValRepr.wordFromCptr cptrblk = 0w0
               then raise Fail "Not enough memory"
               else alloc_final (free_add, 1, cptrblk, n)
         end
      fun get_cptr (a : array) = to_cptr (get_array_ a) : Dynlib.cptr
      val BYTESPERWORD = WORDSIZE div 0w8
      val TAGBITS = 0w10
      val maxLen = Word.toInt (((Word.<<(0w1,WORDSIZE - TAGBITS)) - 0w1) * BYTESPERWORD - 0w1)
      fun array (n, v: elem) =
          let val a = if n < 0 orelse n > maxLen then raise Size else array_ n 
          in fill_ (get_array_ a) 0 n v; a end;      
      fun tabulate (n, f : int -> elem) =
        if n < 0 orelse n > maxLen then raise Size else
        let val a = array_ n
            fun init i = if i >= n then () else (update_ (get_array_ a) i (f i); init (i+1))
        in init 0; a end;
      fun fromList (vs : elem list) =
          let val n = List.length vs
              val a = if n > maxLen then raise Size else array_ n 
      	      fun init [] i = ()
      	        | init (v::vs) i = (update_ (get_array_ a) i v; init vs (i+1))
          in init vs 0; a end;
      fun length (ref c: array) = get_length_ c
      fun sub (a: array, i) =
        if i < 0 orelse i >= length a then raise Subscript 
        else sub_ (get_array_ a) i;
      fun update (a: array, i, v) =
        if i < 0 orelse i >= (length a) then raise Subscript 
        else update_ (get_array_ a) i v;
      fun vector (a: array) =
          let val n = length a
              val newvec = vector_ n 
          in blitav_ (get_array_ a) 0 newvec 0 n; newvec end;
      fun copy {src = a1: array, dst = a2: array, di=i2} =
          let val n = length a1
          in if i2 < 0 orelse i2+n > (length a2) then raise Subscript
      	        else blitaa_ (get_array_ a1) 0 (get_array_ a2) i2 n
          end
      fun copyVec {src = a1: vector, dst = a2: array, di=i2} =
          let val n = lengthv_ a1
          in if i2 < 0 orelse i2+n > (length a2) then raise Subscript
                else blitva_ a1 0 (get_array_ a2) i2 n
          end
      fun find (p : elem -> bool) (a: array) : elem option = 
         let val stop = length a
             fun lr j = 
      	        if j < stop then 
                   if p (sub_ (get_array_ a) j) then SOME (sub_ (get_array_ a) j) else lr (j+1)
      	              else NONE
          in lr 0 end
      fun exists (p : elem -> bool) (a : array) : bool = 
          let val stop = length a
              fun lr j = j < stop andalso (p (sub_ (get_array_ a) j) orelse lr (j+1))
          in lr 0 end
      fun all (p : elem -> bool) (a: array) : bool = 
          let val stop = length a
      	fun lr j = j >= stop orelse (p (sub_ (get_array_ a) j) andalso lr (j+1))
          in lr 0 end
      fun foldl f e (a: array) = 
          let val stop = length a
              fun lr j res = if j < stop then lr (j+1) (f(sub_ (get_array_ a) j, res))
      	                         else res
          in lr 0 e end
      fun foldr f e (a: array) =
          let fun rl j res = if j >= 0 then rl (j-1) (f(sub_ (get_array_ a) j, res))
      		       else res
          in rl (length a - 1) e end
      fun modify f (a: array) = 
          let val stop = length a
              fun lr j = if j < stop then (update_ (get_array_ a) j (f(sub_ (get_array_ a) j)); lr (j+1))
      	                    else ()
          in lr 0 end
      fun app f (a: array) = 
          let val stop = length a
              fun lr j = if j < stop then (f(sub_ (get_array_ a) j); lr (j+1))
                            else ()
          in lr 0 end
      fun findi (p : int * elem -> bool) (a: array) : (int * elem) option = 
          let val stop = length a
              fun lr j = if j < stop then 
                            if p (j, sub_ (get_array_ a) j)
                               then SOME (j, sub_ (get_array_ a) j)
                               else lr (j+1)
                            else NONE
          in lr 0 end
      fun foldli f e (a: array) = 
          let val stop = length a
      	fun lr j res = 
      	    if j < stop then lr (j+1) (f(j, sub_ (get_array_ a) j, res))
      	    else res
          in lr 0 e end;
      fun foldri f e (a: array) = 
          let fun rl j res = 
      	    if j >= 0 then rl (j-1) (f(j, sub_ (get_array_ a) j, res))
      	    else res
          in rl (length a - 1) e end;
      fun modifyi f (a: array) = 
          let val stop = length a 
              fun lr j = 
                  if j < stop then (update_ (get_array_ a) j (f(j, sub_ (get_array_ a) j)); lr (j+1))
                     else ()
          in lr 0 end;
      fun appi f (a: array) = 
          let val stop = length a
              fun lr j = 
      	          if j < stop then (f(j, sub_ (get_array_ a) j); lr (j+1)) 
      	          else ()
          in lr 0 end;
      fun collate cmp (a1: array,a2: array) =
          let val n1 = length a1 
              and n2 = length a2
              val stop = if n1 < n2 then n1 else n2
              fun h j = (* At this point a1[0..j-1] = a2[0..j-1] *)
                                  if j = stop then if n1 < n2 then LESS
                                     else if n1 > n2 then GREATER
                                     else                 EQUAL
      	    else
      		case cmp(sub_ (get_array_ a1) j, sub_ (get_array_ a2) j) of
      		    EQUAL => h (j+1)
      		  | res   => res
          in h 0 end;
   end
end
      
