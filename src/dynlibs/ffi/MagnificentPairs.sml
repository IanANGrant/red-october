val _ = List.map Meta.load ["Real", "Math", "IntInf"];

val _ = Meta.orthodox();

signature Pair =
sig
   type int
   val pair : int * int -> int
   val part : int -> int * int
   val eq : int * int -> bool
   val fromInt : Int.int -> int
   val toInt : int -> Int.int
end

signature PairPrims =
sig
   type int
   val op + : int * int -> int
   val op - : int * int -> int
   val sqadd : int * int -> int
   val sqrtrem : int -> int * int
   val op < : int * int -> bool
   val eq : int * int -> bool
   val fromInt : Int.int -> int
   val toInt : int -> Int.int
end

functor Pair(structure Prims : PairPrims)
   :> Pair
      where type int = Prims.int =
struct
   type int = Prims.int
   local open Prims
   in fun pair (x, y) =
         let val z =
            if x < y 
               then sqadd (y, x)
               else sqadd (x, y + x)
         in z end
      fun part z =
         let val (y, x) = sqrtrem z
         in if x < y
               then (x, y)
               else (y, x - y)
         end
      val eq = eq
      val fromInt = fromInt
      val toInt = toInt
   end
end

structure IntInfPrims
  : PairPrims =
struct
   open IntInf
   val sqadd = fn (x,y) => x * x + y
end

structure IntInfPair =
   Pair(structure Prims = IntInfPrims)

(* With n(n+1) combinators numbered 1, if n=0, and n, n+1, n+2, ...,
   n(n+2), otherwise, we have an injection from the set of combinatory
   logic expressions into the integers {n,n+1,...}. So we can have
   sets of 1, 6, 12, ...  combinators. The case n=0 corresponds to
   finite binary trees, which can encode a single point basis such as
   the A combinator mentioned in Harrison's lecture notes. This case
   is interesting because there is (almost!) a one-one correspondence
   between combinator expressions and the non-negative integers. With
   n=1 we can encode SK combinatory logic expressions, and n=2 is
   enough to add I and Turner's B and C combinators, with one more, Y,
   say. But the numbers quickly become enormous. Even for the case
   n=1, the SK expression for, say, Tromp's fixedpoint combinator S S
   K (S (K (S S (S (S S K)))) K) is encoded by a number with 74
   decimal digits:
 
         2651284714322749091140226947223489961
         9997949440222738226790693614723236878

   So with that in mind, we use recursion schemae to define a generic
   interface to various implementations of pairing, so that we can
   change the underlying implementation to use, e.g. hash consing,
   if/when it is necessary. Recursion schemae are explained the paper
   "Programming with Recursion Schemes" by Wang & Murphy
   http://www.cs.cmu.edu/~tom7/papers/wang-murphy-recursion.pdf
   which is well worth reading.

*)

signature Comb =
sig
   type t
   datatype 'a F = S | K | Comb of 'a * 'a
   val inj : t F -> t
   val prj : t -> t F
end

signature CombRec =
sig
   structure C : Comb
   val fold : ('a C.F -> 'a) -> C.t -> 'a
   val unfold : ('a -> 'a C.F) -> 'a -> C.t
end

signature CombRep =
sig
    type comb
    val mkS : comb
    val mkK : comb
    val mkComb : comb * comb -> comb
    val deconstruct : (unit -> 'a) ->
                      (unit -> 'a) ->
                      (comb * comb -> 'a) -> comb -> 'a
end

functor Convert(structure Rec : CombRec
                structure CombRep : CombRep) : sig
   structure C : Comb
   val toComb : C.t -> CombRep.comb
   val fromComb : CombRep.comb -> C.t
end =
struct
   open Rec CombRep
   fun toComb c =
       fold (fn C.S => mkS
              | C.K => mkK
              | C.Comb (a,b) => mkComb (a,b)) c
   val fromComb =
       unfold (deconstruct
                 (fn _ => C.S)
                 (fn _ => C.K)
                  C.Comb)
end

signature RecType =
sig
   type t
   type 'a F 
   val FMap : ('a -> 'b) -> 'a F -> 'b F
   val inj : t F -> t
   val prj : t -> t F
end

signature Rec =
sig
   structure T : RecType
   val fold : ('a T.F -> 'a) -> T.t -> 'a
   val unfold : ('a -> 'a T.F) -> 'a -> T.t
end

functor Rec(structure T : RecType) : Rec =
struct
   structure T = T
   open T
   fun wrapF f g h x = f (FMap h (g x))
   fun fold step x = wrapF step prj (fold step) x
   fun unfold gen y = wrapF inj gen (unfold gen) y
end

functor CombRec(structure C : Comb) : CombRec =
struct
   structure C = C
   structure T =
   struct
      type t = C.t
      type 'a F = 'a C.F
      val inj = C.inj
      val prj = C.prj
      fun FMap f C.S = C.S
        | FMap f C.K = C.K
        | FMap f (C.Comb(a,b)) = C.Comb(f a, f b)
   end
   structure Rec = Rec(structure T = T)
   open Rec
end

functor NatComb(structure IntPair : Pair) : Comb =
struct
   datatype 'a F = S | K | Comb of 'a * 'a
   type t = IntPair.int
   open IntPair
   val S' = fromInt 1
   val K' = fromInt 2
   fun inj S = S'
     | inj K = K'
     | inj (Comb c) = pair c
   fun prj z =
      if eq (z, S')
         then S
         else if eq (z, K')
                 then K
                 else Comb (part z)
end

datatype comb = Comb of comb * comb | S | K

functor RDTCombRep(datatype
                      comb = S
                           | K
                           | Comb of comb * comb)
  : CombRep =
struct
   type comb = comb
   val mkS = S
   val mkK = K
   val mkComb = Comb
   fun deconstruct S' K' Comb' =
      fn S => S'()
       | K => K'()
       | Comb p => Comb' p
end

structure IntInfComb =
   Convert(structure Rec = CombRec(structure C =
                                      NatComb(structure IntPair = IntInfPair))
           structure CombRep = RDTCombRep(datatype comb = datatype comb))


val k =  Comb(Comb(Comb(S, S), K),
                   Comb(Comb(S,
                             Comb(K, Comb(Comb(S, S),
                                          Comb(S, Comb(Comb(S, S),
                                                       K))))),
                        K))

fun ppcomb e =
   let fun iter prec K = "K"
         | iter prec S = "S"
         | iter prec (Comb(e1,e2)) = 
             let fun maybebracket s =
                  if prec <= 20
                     then s
                     else "("^s^")"
             in
                maybebracket ((iter 20 e1)^" "^(iter 21 e2))
             end
   in
       iter 10 e
   end

val k' = IntInfComb.fromComb k
val k's = IntInf.toString k'
val k'' = IntInfComb.toComb k'
val true = k'' = k

val r = ppcomb k''

structure Poly =
struct
   datatype 'a comb = S
                    | K
                    | Comb of 'a * 'a
   val e1 = S
   val e2 = Comb(S,K)
   val e3 = Comb(e1,e2)
end

structure Recursive =
struct
   datatype comb = S
                 | K
                 | Comb of comb * comb
   val e1 = S
   val e2 = Comb(S,K)
   val e3 = Comb(e1,e2)
end

signature Comb =
sig
   type t
   datatype 'a F = S | K | Comb of 'a * 'a
   val inj : t F -> t
   val prj : t -> t F
end

signature B =
sig
   type 'a F
   val f : 'a F -> Recursive.comb
   val g : Recursive.comb -> 'a F
end

functor S(P : B) :> B
   where type 'a F = ('a P.F) Poly.comb =
struct
   type 'a F = ('a P.F) Poly.comb
   val f = fn Poly.Comb(e,e') =>
                   Recursive.Comb(P.f e,P.f e')
            | Poly.S => Recursive.S
            | Poly.K => Recursive.K
   val g = fn Recursive.Comb(e,e') =>
                   Poly.Comb(P.g e,P.g e')
            | Recursive.S => Poly.S
            | Recursive.K => Poly.K
end

structure O : B =
struct
   type 'a F = 'a Poly.comb
   fun f Poly.S = Recursive.S
     | f Poly.K = Recursive.K
     | f _ = raise Fail "O.f: no case"
   fun g Recursive.S = Poly.S
     | g Recursive.K = Poly.K
     | g _ = raise Fail "O.g: no case"
end

structure It = O

fun loopexec s =
    let fun iter m 0 = m
          | iter m n =
              if m then iter (Meta.exec s) (n-1)
                   else m
    in iter true
    end;

val _ = Meta.quietdec := true;
val true = loopexec "structure It = S(It)" 10;
Meta.quietdec := false;

abstype 'a point = POINT
     of {getx : 'a vector,
         diff : 'a point -> 'a point,
         move : 'a point -> 'a point,
         scale : 'a -> 'a point,
         proj : 'a point -> 'a}
with
   fun new i (op +) (op -) (op * ) dot =
      let fun self x =
             POINT {getx = x,
                    move = fn (POINT pr) => (self (x + (#getx pr))),
                    diff = fn (POINT pr) =>
                             self (x - (#getx pr)),
                    scale = fn i => (self (x * i)),
                    proj = fn (POINT pr) =>
                                dot(x, (#getx pr))}
       in self i
       end
   fun getx (POINT pr) = #getx pr
   fun diff (POINT pr) = #diff pr
   fun move (POINT pr) = #move pr
   fun scale (POINT pr) = #scale pr
   fun proj (POINT pr) = #proj pr
end

val op >> = fn (x,y) => move x y
infix 5 >>

fun realpoint (origin : real) (f : real vector) =
  new f (fn (v,v') => (* + *)
           let val (n,n',n'') = (Vector.length v,Vector.length v',Vector.length f)
           in if n <> n' orelse n <> n''
                 then raise Size
                 else Vector.tabulate
                        (n, fn i => Vector.sub (v,i) + (Vector.sub (v',i)))
           end)
        (fn (v,v') => (* - *)
           let val (n,n',n'') =
                     (Vector.length v,
                      Vector.length v',
                      Vector.length f)
           in if n <> n' orelse n <> n''
                 then raise Size
                 else Vector.tabulate
                        (n, fn i => Vector.sub (v,i) - (Vector.sub (v',i)))
           end)
        (fn (v,s) => (* x *)
           let val (n,n') = (Vector.length v,Vector.length f)
           in if n <> n' then raise Fail "Internal error"
                         else Vector.tabulate (n, fn i => Vector.sub (v,i) * s)
           end)
        (fn (v,v') => Vector.foldri (fn (i,s,r) => r + (Vector.sub (v',i) * s)) origin v) (* dot *)

fun real1point (f : real) =
   realpoint 0.0  #[f]

fun real2point (x : real, y : real) =
   realpoint 0.0 #[x,y]

val p = real1point 1.0
val q = real1point 0.0
val 1.0 = proj p (diff p q)

val o2 = real2point (0.0,0.0)
val q2 = real2point (0.0,1.0)
val p2 = real2point (1.0,0.0)
val q3 = move (scale q2 5.0) (scale p2 5.0)
val true = (proj q2 (diff p2 q2)) = (proj p2 (diff q2 p2))
val r = (getx q3,getx q,diff q2 q3)

fun realspace (origin : 'a point) (f : 'a point vector) =
  new f (fn (v,v') => 
           let val (n,n',n'') =
                       (Vector.length v,
                        Vector.length v',
                        Vector.length f)
           in if n <> n' orelse n <> n''
                 then raise Size
                 else Vector.tabulate
                        (n, fn i => move (Vector.sub (v,i))
                                         (Vector.sub (v',i)))
           end)
        (fn (v,v') => 
           let val (n,n',n'') =
                     (Vector.length v,
                      Vector.length v',
                      Vector.length f)
           in if n <> n' orelse n <> n''
                 then raise Size
                 else Vector.tabulate
                        (n, fn i => diff (Vector.sub (v,i))
                                         (Vector.sub (v',i)))
           end)
        (fn (v,s) => 
           let val (n,n') =
                     (Vector.length v,
                      Vector.length f)
               val v' = getx s
           in if n <> n' then raise Fail "Internal error"
                         else Vector.tabulate 
                                (n, fn i => scale (Vector.sub (v,i))
                                                  (Vector.sub (v',i)))
           end)
        (fn (v,v') => 
           let val (n,n',n'') =
                     (Vector.length v,
                      Vector.length v',
                      Vector.length f)
           in if n <> n' orelse n <> n''
                 then raise Size
                 else Vector.foldri
                        (fn (i,x,a) => move a (scale x (proj x (Vector.sub (v',i))))) origin v
           end)

fun matrix p = Vector.map getx (getx p)

val sp2 = real2point (1.0,1.0)
val v2 = realspace o2 #[p2, q2]
val u2 = scale v2 (scale sp2 5.0)
 
fun real3point (x : real, y : real, z : real) =
   realpoint 0.0 #[x,y,z]

val o3 = real3point (0.0,0.0,0.0)
val p3 = real3point (1.0,0.0,0.0)
val q3 = real3point (0.0,1.0,0.0)
val r3 = real3point (0.0,0.0,1.0)
val sp3 = move (move p3 q3) r3
val sp3' = scale sp3 (1.0 / (Math.sqrt(proj sp3 sp3)))

val s3 = realspace o3 #[p3, q3, r3]
val i = scale s3 p3
val j = scale s3 q3
val k = scale s3 r3

fun matrix' p = Vector.map matrix (getx p)

val v3 = realspace (scale s3 o3) #[i,j,k]

fun vec3 (x,y,z) =
      scale (i >> j >> k)
            ((scale p3 x) >> (scale q3 y) >> (scale r3 z))

val x1 = vec3(12.0,   5.0,  2.0)
val x2 = vec3(~5.0,  12.0, ~2.0)
val x3 = vec3(~2.0, ~12.0,  5.0)

val pv3 = (scale v3 x1) >> (scale v3 x2) >> (scale v3 x3)
val pv3m = matrix' pv3

abstype 'a Mu = In of {out : 'a Mu -> 'a} with
   fun init f = In {out = f}
   fun getf (In{out}) = out
end

abstype 'a Tree = collect of {f : 'a Tree -> 'a} with
   fun init f = collect {f = f}
   fun getf (collect {f}) = f
end
