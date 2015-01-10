signature ObjRepr =
sig
   type state
   datatype rep =
       Tuple of int * sym Vector.vector        (* Tuple(tag,elts) *)
     | ByteVector of int * Word8Vector.vector  (* ByteVector(tag,bytes) *)
     | Word of word
   and sym =
       Decl of int * rep                       (* Decl(idx,rep) *)
     | Ref of int                              (* Ref(idx) *)
   val encode : 'a * state -> state
   val decode : state -> 'b
   val valAbsRepr : 'c -> sym
   val absReprVal : sym -> 'd
   val compare : 'a * 'a -> order
   val compare_abs : sym * sym -> order
end

(* Now we have to turn this whole thing inside-out! What we want is a
   pair of co-routines and all the i/o going via callbacks. This will
   allow us to put each end in an event-loop and drive the process by
   IO signalling. Then there will be no waiting around or returning
   error/again codes, we'll only compute when there's data to compute,
   and when that data is needed somewhere.

   The other nice thing about a co-routine arrangement is that it will
   allow us to compose two (or more) translators and the intermediate
   representation(s) will just "disappear in a puff of logic". *)

functor ObjRepr
   (type state
    val readByte : state -> Word8.word
    val writeByte : state * Word8.word -> unit)
  :> ObjRepr where type state = state =
struct
   datatype rep = datatype AbsObjRepr.rep
   datatype sym = datatype AbsObjRepr.sym
   local
      structure AbsObjRepr = AbsObjRepr
      open AbsObjRepr
      open ValRepr
      val ref_type   : word8 = 0w0;
      val word_type  : word8 = 0w1;
      val bytes_type : word8 = 0w2;
      val tuple_type : word8 = 0w3;
      fun decRepr producer =
         let fun loop slc =
             let fun scanner len =
                     fn getc => fn slc =>
                        let val arr = Word8Array.tabulate (len, fn _ => 0w0)
                            fun iter 0 rest = (Word8Array.vector arr,rest)
                              | iter n rest =
                                  let val (w,rest) = getc rest
                                      val _ = Word8Array.update (arr,len - n,w);
                                  in iter (n-1) rest
                                  end
                        in iter len slc
                        end
                 fun decVec len slc = producer (scanner len) slc
                 fun decWord16 slc =
                     let val (slc,rest) = producer (scanner 2) slc
                         val w = Word8Vector.foldr
                                    (fn (b,w) => Word.orb(Word.<<(w,0w8),
                                                          Word8.toLarge b)) 0w0 slc
                     in (w,rest)
                     end
                 fun decWord8 slc =
                     let val (vec,rest) = producer (scanner 1) slc
                     in (Word8Vector.foldr #1 0w0 vec,rest)
                     end
                 fun decInteger slc = (* polymorphic in 'a : int/word *)
                     let val (len,rest) = decWord8 slc
                         val (vec,rest) = decVec (Word8.toInt len) rest
                         val n = Obj.magic (Obj.obj_field (Obj.repr vec) 0) 
                     in (n,rest)
                     end              (*  Above could be for any interpreter: Concretion  *)
      (*----------------------------------------------------------------------------------*)
                 fun decUint8 slc =   (*  Below could be for any language:    Abstraction *)
                     let val (w,rest) = decWord8 slc
                     in (Word8.toInt w,rest)
                     end
                 fun decUint16 slc =
                     let val (w,rest) = decWord16 slc
                     in (Word.toInt w,rest)
                     end
                 fun decInt slc =
                     let val (i:int,rest) = decInteger slc
                     in (i,rest)
                     end
                 fun decWord slc =
                     let val (w:Word.word,rest) = decInteger slc
                     in (Word w,rest)
                     end
                 fun decBytes slc =
                     let val (tag,rest) = decUint16 slc
                         val (len,rest) = decInt rest
                         val (vec,rest) = decVec len rest
                     in (ByteVector (tag, vec),rest)
                     end
                 fun decTuple slc =
                     let val (tag,rest) = decUint16 slc
                         val (len,rest) = decInt rest
                         fun iter (acc,rest) 0 = (List.rev acc,rest)
                           | iter (acc,rest) n = 
                                let val (fld,rest) = loop rest
                                in iter (fld::acc,rest) (n-1)
                                end
                         val (l,rest) = iter ([],rest) len
               in (Tuple (tag,Vector.fromList l),rest)
               end
           fun decType slc =
               let val (t,rest) = decWord8 slc
                   val (idx:Int.int,rest) = decInteger rest
               in case t
                    of 0w0 => (Ref idx,rest)
                     | 0w1 => let val (rep,rest) = decWord rest
                              in (Decl (idx,rep),rest) end
                     | 0w2 => let val (rep,rest) = decBytes rest
                              in (Decl (idx,rep),rest)
                              end
                     | 0w3 => let val (rep,rest) = decTuple rest
                              in (Decl (idx,rep),rest)
                              end
                     | w => raise
                             Fail
                             ("decType: no case 0wx"^
                              (Word8.toString w))
               end
             in decType slc
             end
         in loop
         end
      fun encRepr formatter consumer (repr,acc) =
         let fun encType (t,acc) =
                 consumer (formatter (1,fn _ => t),acc)
             fun encTag (tag,acc) =
                 let fun tabfn i =
                      Word8.fromLarge
                          (Word.andb(0wxff,Word.>>(Word.fromInt tag,
                                                   0w8*(Word.fromInt i))))
                 in consumer (formatter (2,tabfn),acc) end
             fun encWord (w : word,acc) =
                 let val v = wordWord8Vector w
                     val len = Word8Vector.length v
                 in consumer (formatter
                      (1 + len, fn 0 => Word8.fromInt (len)
                                 | i => Word8Vector.sub(v,i-1)),acc)
                 end
             fun encVec (v : Word8Vector.vector ,acc) =
                 let val len = Word8Vector.length v
                 in consumer (formatter (len, fn i => Word8Vector.sub(v,i)),acc)
                 end                                       (* Concretion *)
      (*-----------------------------------------------------------------*)
             fun loop (repr as (Decl(idx,Word w),acc)) =  (* Abstraction *)
                    let val acc = encType (word_type,acc)
                        val acc = encWord (Word.fromInt idx,acc)
                        val acc = encWord (w,acc)
                        in acc
                    end
               | loop (repr as (Decl(idx,ByteVector (tag,v)),acc)) =
                    let val acc = encType (bytes_type,acc)
                        val acc = encWord (Word.fromInt idx,acc)
                        val acc = encTag (tag,acc)
                        val acc = encWord (Word.fromInt (Word8Vector.length v),acc)
                        val acc = encVec (v,acc)
                     in acc
                    end
               | loop (repr as (Decl(idx,Tuple (tag,v)),acc)) =
                    if tag = 255 then raise Fail "Can't encode objects with finalized tags" else 
                    if tag = 249 then raise Fail "Can't encode objects with code tags" else 
                    let val acc = encType (tuple_type,acc)
                        val acc = encWord (Word.fromInt idx,acc)
                        val acc = encTag (tag,acc)
                        val acc = encWord (Word.fromInt (Vector.length v),acc)
                    in Vector.foldl (fn (v',a) => loop (v',a)) acc v
                    end
               | loop (repr as (Ref idx),acc) =
                    let val acc = encType (ref_type,acc)
                        val acc = encWord (Word.fromInt idx,acc)
                    in acc end
         in (loop (repr,acc))
         end

   val producer : ((state -> Word8.word * state) ->
                   state -> Word8Vector.vector * state) ->
                   state -> Word8Vector.vector * state =
      let fun getc buf =
              (readByte buf,buf)
          fun produce sfn =
              sfn getc
      in produce
      end
      val consumer : (Word8Vector.vector * state) -> state
            = (fn (vec,buff) =>
                  let val len = Word8Vector.length vec
                  in Word8Vector.foldl (fn (b,s) => (writeByte (s,b);s)) buff vec
                  end)

      val printem = encRepr Word8Vector.tabulate consumer
      val scanem = decRepr producer
   in
      type state = state

      val valAbsRepr : 'a -> sym = fn v => objAbsRepr (Obj.repr v)
      val absReprVal : sym -> 'd = fn obj => Obj.magic (absReprObj obj)

      fun compare (x : 'a, y : 'a) = compare_abs (valAbsRepr x,valAbsRepr y)

      val compare_abs  = compare_abs

      fun encode (v,s) = printem (valAbsRepr v,s)

      fun decode s =
         let val (r,s') = scanem s
         in absReprVal r
         end
   end
end
