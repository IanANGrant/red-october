(* Using embedding/projection pairs and type expressions to coerce
   values. This time, presented as consumers: 'a * 'b -> 'b    *)

val _ = Meta.load "Real";
val _ = Meta.load "ListPair";

val _ = Meta.quotation := true;

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

fun quote l =
   let fun iter r [] = r
         | iter r ((QUOTE s)::fs)     = iter (r^s) fs
         | iter r ((ANTIQUOTE s)::fs) = iter (r^s) fs
   in iter "" l
   end;

val fPair''  = quote `(fPair "(" "," ")")`;
val fPair''' = quote `(fPair "{" ":" "}")`;
val fList''  = quote `(fList "[" "," "]")`;
val fStr''   = quote `fStr`;
val fReal''  = quote `fReal`;
val fInt''   = quote `fInt`;

local 
   val fList' = fn fname => fn s =>
                 quote ` let val fList' = ^(fname)
                         in fList'
                         end^(s)`
   val fPair' = fn fname => fn s => fn s' => 
                 quote ` (let val fPair' = ^(fname)
                          in fPair'
                          end^(s)^(s'))`
   val fStr'  = fn fname => " "^fname
   val fReal' = fn fname => " "^fname
   val fInt'  = fn fname => " "^fname
in
   fun mkformat fstr =
       fn v =>
          let val decl = quote `val ^(v) =^(fstr)`
              val ok = Meta.exec decl
          in if not ok
                then raise Fail ("Internal error compiling: "^fstr)
                else ()
          end
      val fListPlain = fList' fList''
      val fPairPlain = fPair' fPair''
      val fPairCurly = fPair' fPair'''
      val fStrPlain  = fStr'  fStr''
      val fRealPlain = fReal' fReal''
      val fIntPlain  = fInt'  fInt''
end

val mt' = mkformat (fListPlain (fPairPlain
                                   (fStrPlain)
                                   (fPairPlain
                                      (fRealPlain)
                                      (fIntPlain))));

val mt'' = mkformat (fListPlain (fPairCurly
                                   (fStrPlain)
                                   (fPairPlain
                                      (fRealPlain)
                                      (fIntPlain))));

val _ = mt' "testme";

val _ = mt'' "testme'";

val "[(N,(42.0,1)),(P,(43.5,2))]" = format (%testme)
    [("N",(42.0,1)),("P",(43.5,2))];

val "[{N:(42.0,1)},{P:(43.5,2)}]" = format (%testme')
    [("N",(42.0,1)),("P",(43.5,2))];
