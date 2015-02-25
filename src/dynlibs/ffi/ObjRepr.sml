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
      fun decRepr scanner producer =
         let fun loop slc =
             let fun decVec len slc = producer (scanner len) slc
                 fun decWord16 slc =  (* These three deconstuctors are all instances of foldr,
                                         so we should make the scanner the foldr functional
                                         for the representing type. *)
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

 (* And these intermediate representations could be skipped entirely
    if we passed the foldr arguments (the function and the
    accumulator) directly from the constructors (the absReprObj
    function, in this case) straight down to the deconstructors
    (decVec, decWord16, decInteger etc.)

    The result will be a species of continuation-passing style. See
    interpreter III in the file DefInt.sml for Reynold's really
    beautiful example of it, (and also see note [*] below.)  

    This interpreter uses only tail-calls. As Reynolds points out, it
    is basically a finite state machine. So it could be implemented in
    assembler as a set of 4 states with deterministic transitions
    which are represented by events with arguments.  The states
    basically pass processes to each other, and the processes take the
    form of abstract syntax. Each state switches on the first
    argument, and then deconstructs the event and constructs a new
    event which it passes either to itself, or to another process. So
    the machine doesn't need a stack, it just needs some form of
    registers for the function arguments. The registers only need to
    keep references to the values. It looks a lot like Wadsworth's
    combinator graph reduction. Indeed, the records and case switches
    really are a variable-free combinator calculus. *)

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

       (* A compound representation like this ByteVector could be
          handled similarly. We pass a sequence of three successive
          foldr deconstructors down, each of which takes, as an
          accumulator, the result produced by its predecessor, and the
          producer then calls them in order as the data come in. It's
          only a very simple-minded combinator parser, after all.

          I suspect this might be the solution to what people seem to
          be calling "call-back hell". *)

                 fun decBytes slc =
                     let val (tag,rest) = decUint16 slc
                         val (len,rest) = decInt rest
                         val (vec,rest) = decVec len rest
                     in (ByteVector (tag, vec),rest)
                     end

       (* Here's a nice example of the insanity of concrete
          representation: I construct a list of the elements of the
          tuple, and then immediately tear it apart to make a
          vector. And then I pass that vector to absObjRepr and
          immediately tear it apart again to make ...  guess what
          ... yes! _another_ vector!

          It's true that, "in the end" we have to get the
          representation out of the FiFo and into something presented
          on the ML heap. But that is really only because the
          interpreter and the compiler are all cemented into a
          garbage-collected heap. We know it doesn't have to be like
          this though. We could be using regions (a la MLKit),
          closures (a la v8) or slab allocators (a la, er ...,
          OpenBSD?), for example.

          Some of these things, IO-bound processing like this, in
          particular, could be designed to do all their memory
          management through buffers of one sort or another.  A mark
          and sweep garbage collector, for example, is itself a sort
          of ring buffer. So once we get the infrastructure together
          to start representing data as assembler code, we can make
          self-reproducing assembler representations in ring buffers,
          which, when asked to, will politely re-assemble the next
          incarnation of themselves in the unfragmented space and
          await further instructions. A sort of viral engineering. *)

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
                                    in (Decl (idx,rep),rest)
                                    end
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

     (* The dual of the above comments apply to the following
        functions. Here we are printing the results of the objAbsRepr
        function. And it's a real nuisance when the two get out of
        sync.  with one another. These functions should come from the
        same structure. See tdpeprint.sml for an idea as to how to do
        this. The code there doesn't do the semantic actions right,
        but that shouldn't be too hard to fix. We just need to use
        parser combinators, and invent the dual: printer
        combinators. *)

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
                                                          (* Abstraction *)

      (* In these cases it's very obvious that the arguments we are
         giving the encoding functions are of the form of the
         arguments that the foldr functional applies its caller's
         function to. *)

             fun loop (repr as (Decl(idx,Word w),acc)) =
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
      fun scanner len =
        fn getc =>
          fn slc =>
            let val arr = Word8Array.tabulate (len, fn _ => 0w0)
                fun iter 0 rest = (Word8Array.vector arr,rest)
                  | iter n rest =
                       let val (w,rest) = getc rest
                          val _ = Word8Array.update (arr,len - n,w)
                       in iter (n-1) rest
                       end
            in iter len slc
            end

      val formatter = Word8Vector.tabulate

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

      val printem = encRepr formatter consumer
      val scanem = decRepr scanner producer
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

(*  [*]

    There are only two basic types: products, and sums; or sequences
    and alternations. For example, EXP is an alternation, and APPL,
    LAMBDA, COND etc. are sequences:

    EXP = CONST ∪ VAR ∪ APPL ∪ LAMBDA ∪ COND ∪ LETREC
    APPL = [opr: EXP, opnd: EXP] ...

    Now take a look at the LLVM intermediate representation
    language. It wouldn't take many hours to write an ML unit which
    output LLVM IR text describing the primitive deconstructors and
    constructors for moving represented values between ML and C. LLVM
    will compile these into a dynamic library for you. Then you could
    write another unit which uses those primitives to call the full C
    interface defined in include/llvm-c/*.h, which would give you
    access to the JIT compiler, as well as all the optimisation
    phases, disassemblers etc.

    Then you could interpret Reynold's lambda evaluators into pretty
    fast self-modifying machine code, and it would run on lots of CPU
    architectures. So you could write an ML bytecode compiler in a
    sugared version of Reynold's lambda language, and use that to
    compile bytecode, or ZAM or whatever, into pretty efficient
    assembler. The important thing about this process is that there
    will be relatively little source, and it will mostly be very
    abstract, and therefore re-interpretable, so you won't be locked
    into any one concrete technology.

    For example, you will be able to replace the LLVM code generator
    with a native assembler, generated from a formal description
    written in Reynold's lambda language. So a system built on that
    foundation will be much easier to secure than one based on, say
    5,500,000 lines of C code which needs 600 MB+ of C compiler source
    and heaven only knows how much other junk, to build. And that's
    not even counting all the dwarves! *)
