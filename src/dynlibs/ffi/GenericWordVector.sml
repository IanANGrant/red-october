(* GenericWordVector -- SML Basis Library *)

signature GenericWordVector =
sig
   type vector
   type elem

   val maxLen   : int

   val fromList : elem list -> vector
   val tabulate : int * (int -> elem) -> vector

   val length   : vector -> int
   val sub      : vector * int -> elem
   val update   : vector * int * elem -> vector
   val concat   : vector list -> vector

   val find     : (elem -> bool) -> vector -> elem option
   val exists   : (elem -> bool) -> vector -> bool
   val all      : (elem -> bool) -> vector -> bool

   val app      : (elem -> unit) -> vector -> unit
   val map      : (elem -> elem) -> vector -> vector
   val foldl    : (elem * 'b -> 'b) -> 'b -> vector -> 'b
   val foldr    : (elem * 'b -> 'b) -> 'b -> vector -> 'b

   val findi    : (int * elem -> bool) -> vector -> (int * elem) option
   val appi     : (int * elem -> unit) -> vector -> unit
   val mapi     : (int * elem -> elem) -> vector -> vector
   val foldli   : (int * elem * 'b -> 'b) -> 'b -> vector -> 'b
   val foldri   : (int * elem * 'b -> 'b) -> 'b -> vector -> 'b

   val collate  : (elem * elem -> order) -> vector * vector -> order
end

(* [vector] is the type of one-dimensional, immutable, zero-based
   constant-time-access Generic vectors with elements of type elem,
   that is, exact integers or characters of some kind.  Type vector
   does not admit equality --- not even under duress.

   All operations are as for Vector.vector. *)

signature GenericWordVectorRepr =
sig
   include GenericWordVector
   type repr
   val fromVector : repr -> vector
end

functor GenericWordVector
  (structure AbstractVector : AbstractVector
   structure WordVector : GenericWordVector
      where type elem = AbstractVector.elem)
    :> GenericWordVectorRepr
          where type vector = AbstractVector.vector
            and type repr = WordVector.vector
            and type elem = AbstractVector.elem =
struct
   type vector = AbstractVector.vector
   type repr = WordVector.vector
   type elem = AbstractVector.elem
   val maxLen = WordVector.maxLen
   local
      fun init a = AbstractVector.new WordVector.sub WordVector.length a
   in
      val tabulate = init o WordVector.tabulate
      val fromList = init o WordVector.fromList
      val fromVector = init
      val length = AbstractVector.length
      val sub = AbstractVector.sub
      val map = fn f => fn vec => tabulate(length vec,fn i => f(sub (vec,i)))
      val mapi = fn f => fn vec => tabulate(length vec,fn i => f(i,sub (vec,i)))
      val update = fn (vec,n,e) => tabulate(length vec,fn i => if i = n then e else sub (vec,i))
      fun concat l = (* Let's hope nobody expects this to be particularly swift! *)
         let fun len [] n = n
               | len (v::vs) n = len vs (n + (length v))
             fun subl [] n = raise Fail "GenericWordVector: internal error"
               | subl (v::vs) n =
                     if n < length v
                        then sub (v,n)
                        else subl vs (n - length v)
             val len = len l 0
         in if len > WordVector.maxLen
            then raise Size
            else tabulate (len,fn i => subl l i)
         end
      fun find (p : elem -> bool) a : elem option = 
          let val stop = length a
              fun lr j = 
                  if j < stop
                      then if p (sub (a, j))
                              then SOME (sub (a, j))
                              else lr (j + 1)
                      else NONE
          in lr 0
          end
      fun exists (p : elem -> bool) a : bool = 
          let val stop = length a
              fun lr j = j < stop andalso (p (sub (a, j)) orelse lr (j + 1))
          in lr 0
          end
      fun all (p : elem -> bool) a : bool = 
          let val stop = length a
              fun lr j = j >= stop orelse (p (sub (a, j)) andalso lr (j + 1))
          in lr 0
          end
      fun foldl f e a = 
          let val stop = length a
              fun lr j res =
                     if j < stop
                        then lr (j + 1) (f (sub (a, j), res))
                        else res
          in lr 0 e
          end
      fun foldr f e a =
          let fun rl j res =
                  if j >= 0
                     then rl (j - 1) (f (sub (a, j), res))
                     else res
          in rl (length a - 1) e
          end
      fun app f a = 
          let val stop = length a
              fun lr j = if j < stop
                            then (f (sub (a, j));
                                  lr (j + 1))
                            else ()
          in lr 0
          end
      fun findi (p : int * elem -> bool) a : (int * elem) option = 
          let val stop = length a
              fun lr j = 
                  if j < stop
                      then if p (j, sub (a, j))
                              then SOME (j, sub (a, j))
                              else lr (j + 1)
                      else NONE
          in lr 0
          end
      fun foldli f e a = 
          let val stop = length a 
              fun lr j res = 
                  if j < stop
                     then lr (j + 1) (f (j, sub (a, j), res))
                     else res
          in lr 0 e
          end
      fun foldri f e a = 
          let fun rl j res = 
                  if j >= 0
                     then rl (j - 1) (f (j, sub (a, j), res))
                     else res
          in rl (length a - 1) e
          end
      fun appi f a = 
          let val stop = length a
              fun lr j = 
                  if j < stop
                     then (f (j, sub (a, j));
                           lr (j + 1))
                     else ()
          in lr 0
          end
      fun collate cmp (a1, a2) =
          let val n1 = length a1 
              and n2 = length a2
              val stop =
                     if n1 < n2
                        then n1
                        else n2
              fun h j =
                  if j = stop
                     then if n1 < n2
                             then LESS
                             else if n1 > n2
                                  then GREATER
                                  else EQUAL
                      else case cmp(sub (a1, j), sub (a2, j))
                             of EQUAL => h (j + 1)
                              | res   => res
          in h 0
          end
   end
end
