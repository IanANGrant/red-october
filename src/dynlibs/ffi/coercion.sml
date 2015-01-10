val _ = load "Real";
val _ = load "Values";

datatype tagType =
   INT of int
 | UNIT
 | REAL of real
 | WORD of word
 | STR of string
 | REF of tagType
 | LST of tagType list
 | FUN of tagType -> tagType
 | PR of tagType * tagType
 | TR of tagType * tagType * tagType
 | TUPLE of tagType list

datatype typeExp = 
   tINT
 | tUNIT
 | tSTR
 | tREAL
 | tWORD
 | tREF of typeExp
 | tLST of typeExp
 | tFUN of typeExp * typeExp
 | tPR of typeExp * typeExp  
 | tTR of typeExp * typeExp * typeExp  
 | tTUPLE of typeExp list  


val Unit = (fn () => UNIT,
           fn (UNIT) => ()
            | _ => raise Fail "Int: internal error.",
           tUNIT)

val Int = (fn n => INT n,
           fn (INT n) => n
            | _ => raise Fail "Int: internal error.",
           tINT)

val Real = (fn x => REAL x,
           fn (REAL x) => x
            | _ => raise Fail "Real: internal error.",
           tREAL)

val Word = (fn x => WORD x,
           fn (WORD x) => x
            | _ => raise Fail "Word: internal error.",
           tWORD)

val Str = (fn s => STR s,
           fn (STR s) => s
            | _ => raise Fail "Str: internal error.",
           tSTR)

fun List (T as (emb_T, proj_T, tE_T)) =
      (fn l => LST (List.map emb_T l),
       fn (LST l) => List.map proj_T l
        | _ => raise Fail "List: internal error.",
       tLST tE_T)

fun Ref (T as (emb_T, proj_T, tE_T)) =
      (fn v => REF (emb_T v),
       fn (REF v) => proj_T v
        | _ => raise Fail "Ref: internal error.",
       tREF tE_T)

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
       tTR (tE_T,tE_T',tE_T''))

infixr 5 -->

fun (T as (emb_T, proj_T, tE_T)) --> 
    (T' as (emb_T', proj_T', tE_T')) =
      (fn f => FUN (fn t => emb_T' (f (proj_T t))),
       fn (FUN f) => (fn x => (proj_T' (f (emb_T x))))
        | _ => raise Fail "-->: internal error.",
       tFUN (tE_T,tE_T'))

fun tail x =
   let val len = Values.length x
   in if len = 0
         then raise Fail "tail: unit"
         else let val y = Values.new (Values.Tuple 0,len - 1)
                  fun iter 0 = y
                    | iter n = let val n' = n - 1
                               in Values.update (y,len-n'-2,Values.sub(x,len - n'-1));
                                  iter n'
                               end
              in iter (len - 1)
              end
   end

fun head x =
   let val len = Values.length x
   in if len = 0
         then raise Fail "head: unit"
         else Values.sub(x,0)
   end

fun cons (x,y) =
   let val len = Values.length y
   in let val z = Values.new (Values.Tuple 0,len+1)
          val _ = Values.update (z,0,x)
          fun iter 0 = z
            | iter n = let val n' = n - 1
                       in Values.update (z,len-n',Values.sub(y,len - n'-1));
                          iter n'
                       end
      in iter len
      end
   end

fun Tuple l =
   let fun iter [] = 
            (fn _ => TUPLE [],
             fn (TUPLE []) => Values.value(Values.repr {})
              | _ => raise Fail "Tuple: unit internal error",
             tTUPLE [])
         | iter ((T as (emb_T, proj_T, tE_T))::ts) =
            let val (fe,fp,t) = iter ts
            in (fn v =>
                  let val l =
                     case fe (Values.value (tail v))
                       of TUPLE l => l
                        | _ => raise Fail "Tuple: internal error emb"
                  in TUPLE ((emb_T (Values.value (head v)))::l) end,
                fn (TUPLE (h::t)) => (Values.value (cons (proj_T h,fp (TUPLE t))))
                 | _ => raise Fail "Tuple: internal error proj",
                case t of tTUPLE l => tTUPLE (tE_T::l)
                        | _ => raise Fail "Tuple: internal error type")
            end
   in iter l
   end

exception nonSubtype of typeExp * typeExp

fun lookup_coerce [] tE1 tE2 =
      raise nonSubtype (tE1,tE2)
  | lookup_coerce ((t,t',t2t')::Others) tE1 tE2 =
      if t = tE1 andalso t' = tE2 
         then t2t'
         else lookup_coerce Others tE1 tE2

fun univ_coerce c1 (tFUN(tE1_T1,tE2_T1))
                   (tFUN(tE1_T2,tE2_T2)) (FUN v) =
      FUN(fn x => univ_coerce c1 tE2_T1 tE2_T2
                 (v (univ_coerce c1 tE1_T2 tE1_T1 x)))
  | univ_coerce c1 (tLST(tE_T1)) (tLST(tE_T2)) (LST v) =
      LST(List.map (univ_coerce c1 tE_T1 tE_T2) v)
  | univ_coerce c1 (tREF(tE_T1)) (tREF(tE_T2)) (REF v) =
      REF(univ_coerce c1 tE_T1 tE_T2 v)
  | univ_coerce c1 (tPR(tE1_T1,tE2_T1))
                   (tPR(tE1_T2,tE2_T2)) (PR (v,v')) =
      PR(univ_coerce c1 tE1_T1 tE1_T2 v,
         univ_coerce c1 tE2_T1 tE2_T2 v')
  | univ_coerce c1 (tTR(tE1_T1,tE2_T1,tE3_T1))
                   (tTR(tE1_T2,tE2_T2,tE3_T2)) (TR (v,v',v'')) =
      TR(univ_coerce c1 tE1_T1 tE1_T2 v,
         univ_coerce c1 tE2_T1 tE2_T2 v',
         univ_coerce c1 tE3_T1 tE3_T2 v'')
  | univ_coerce c1 (tTUPLE [])
                   (tTUPLE []) (TUPLE []) = TUPLE []
  | univ_coerce c1 (tTUPLE (t::ts))
                   (tTUPLE (t'::ts')) (TUPLE (v::vs)) =
       let val r = univ_coerce c1 (tTUPLE ts) (tTUPLE ts') (TUPLE vs)
       in TUPLE((univ_coerce c1 t t' v)::(case r
                                            of TUPLE l => l
                                             | _ => raise Fail "uc:int err"))
       end
  | univ_coerce c1 x y v =
      if x = y 
         then v
         else (lookup_coerce c1 x y) v  

fun coerce c1 (T1 as (emb_T1, proj_T1, tE_T1))
              (T2 as (emb_T2, proj_T2, tE_T2)) v =
      proj_T2 (univ_coerce c1 tE_T1 tE_T2 (emb_T1 v))

val C' = [(tINT,
           tREAL,
           fn (INT x) => REAL (Real.fromInt x)
            | _ => raise Fail "coerce: INT->REAL: Internal error"),
          (tWORD,
           tREAL,
           fn (WORD x) => REAL (Real.fromInt (Word.toInt x))
            | _ => raise Fail "coerce: WORD->REAL: Internal error"),
          (tREAL,
           tINT,
           fn (REAL x) => INT (Real.round x)
            | _ => raise Fail "coerce: REAL->INT: Internal error"),
          (tWORD,
           tSTR,
           fn (WORD x) => STR (Word.toString x)
            | _ => raise Fail "coerce: WORD->STR: Internal error"),
          (tINT,
           tSTR,
           fn (INT x) => STR (Int.toString x)
            | _ => raise Fail "coerce: INT->STR: Internal error"),
          (tREAL,
           tSTR,
           fn (REAL x) => STR (Real.toString x)
            | _ => raise Fail "coerce: WORD->STR: Internal error")]

val r2i = coerce C' (Pair(Real,Real) --> Real) (Pair(Int,Real) --> Int)

 (* Doesn't work. Try with curried functions? *)
val r2it = coerce C' (Tuple[Real,Real] --> Real) (Tuple[Real,Real] --> Int)

val iplus = r2i Real.+
val 42 = iplus (12,30.0)
