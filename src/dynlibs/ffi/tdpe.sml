(* Using embedding/projection pairs and type expressions to coerce
   values. 

   From Zhe Yang's, "Encoding Types in ML-like Languages" 
   in proc. ACM SIGPLAN ICFP'98 *)

val _ = Meta.load "Real";
val _ = Meta.load "ListPair";

datatype tagType =
   INT of int
 | REAL of real
 | WORD of word
 | STR of string
 | REF of tagType
 | LST of tagType list
 | FUN of tagType -> tagType
 | PR of tagType * tagType
 | TR of tagType * tagType * tagType

datatype typeExp = 
   tINT
 | tSTR
 | tREAL
 | tWORD
 | tREF of typeExp
 | tLST of typeExp
 | tFUN of typeExp * typeExp
 | tPR of typeExp * typeExp  
 | tTR of typeExp * typeExp * typeExp  

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
  | univ_coerce c1 x y v =
      if x = y 
         then v
         else (lookup_coerce c1 x y) v  

fun coerce c1 (T1 as (emb_T1, proj_T1, tE_T1))
              (T2 as (emb_T2, proj_T2, tE_T2)) v =
      proj_T2 (univ_coerce c1 tE_T1 tE_T2 (emb_T1 v))

infix 5 ++
val I = fn b => b
fun LIT s p = fn b => p (b^s)
fun % toStr_t p = fn b => fn x => p (b^toStr_t x)
val op ++ = op o
fun format fs = fs I ""
val fInt = Int.toString
val fStr = I : string -> string

val fReal = Real.toString
val fWord = Word.toString

fun tPair mkPair mkL mkR =
   fn (x1,x2) => mkPair (mkL x1,mkR x2)

fun fPair l m r =
   tPair (fn (x,x') => l^x^m^x'^r)

fun tList mkCons mkNil mkElt =
   let fun mkTail [] = mkNil
         | mkTail (e::es) = mkCons (mkElt e, mkTail es)
   in mkTail
   end

fun fList l m r toStr =
   let fun mkCons (e,es) = if es = r then e^es else e^m^es
   in fn es => l^(tList mkCons r toStr es)
   end

val fList' = fList "[" "," "]" 
val t = fList' (let val fPair' = fPair  "(" "," ")" 
                in fPair'
                end fStr 
                   (let val fPair' = fPair "(" "," ")" 
                    in fPair'
                    end fReal fInt))
val "[(N,(42.0,1)),(P,(43.5,2))]" = format (%t)
    [("N",(42.0,1)),("P",(43.5,2))]

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
val iplus = r2i Real.+ 
val 42 = iplus (12,30.0)

datatype lexp =
   VAR of string
 | LAM of string * lexp
 | APP of lexp * lexp

datatype 'base tagBaseFunc =
   BASE of 'base
 | FUNC of 'base tagBaseFunc -> 'base tagBaseFunc

datatype typeExpF =
   tBASE
 | tFUNC of typeExpF * typeExpF

val Base = (fn x => BASE x,
            fn (BASE x) => x
             | _ => raise Fail "Base: internal error.",
            tBASE)

infixr 5 ==>

fun (T1 as (I_T1, P_T1, tE1)) ==>
    (T2 as (I_T2, P_T2, tE2)) =
      (fn f => FUNC (fn t => I_T2 (f (P_T1 t))),
       fn (FUNC f) => (fn x => (P_T2 (f (I_T1 x))))
        | _ => raise Fail "==>: internal error.",
       tFUNC (tE1,tE2))

local
   val vn = ref 0
   fun next () = (vn := (!vn) + 1;!vn)
in
   fun resetvar () = vn := 0
   fun newvar s = s^(Int.toString (next ()))
end

val rec reifyTg =
   fn (tBASE, BASE v) => v
    | (tFUNC(tE1,tE2),FUNC v) =>
        let val x1 = newvar "x"
        in LAM(x1, reifyTg (tE2, reflectTg (tE1,VAR x1)))
        end
    | _ => raise Fail "reifyTg: internal error"
and reflectTg =
   fn (tBASE, e) => BASE e
    | (tFUNC(tE1,tE2),e) =>
        FUNC (fn v1 => reflectTg (tE2, APP (e, reifyTg (tE1, v1))))
    | _ => raise Fail "reflectTg: internal error"

fun reify (T as (emb, _, tE)) v = reifyTg (tE, emb v)

val xxx = reify (Base ==> Base) ((fn x => fn y => x y) (fn x => x))
