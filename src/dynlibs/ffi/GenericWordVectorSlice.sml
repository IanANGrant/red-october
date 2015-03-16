(* GenericWordVectorSlice -- SML Basis Library *)

signature GenericWordVectorSlice =
sig
   type elem
   type vector
   
   type slice
   
   val length   : slice -> int
   val sub      : slice * int -> elem
   val slice    : vector * int * int option -> slice
   val full     : vector -> slice
   val subslice : slice * int * int option -> slice
   val base     : slice -> vector * int * int
   val vector   : slice -> vector
   val concat   : slice list -> vector
   val isEmpty  : slice -> bool
   val getItem  : slice -> (elem * slice) option

   val find     : (elem -> bool) -> slice -> elem option
   val exists   : (elem -> bool) -> slice -> bool
   val all      : (elem -> bool) -> slice -> bool

   val app      : (elem -> unit) -> slice -> unit
   val map      : (elem -> elem) -> slice -> vector
   val foldl    : (elem * 'b -> 'b) -> 'b -> slice -> 'b
   val foldr    : (elem * 'b -> 'b) -> 'b -> slice -> 'b

   val findi    : (int * elem -> bool) -> slice -> (int * elem) option
   val appi     : (int * elem -> unit) -> slice -> unit
   val mapi     : (int * elem -> elem) -> slice -> vector
   val foldli   : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
   val foldri   : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b

   val collate  : (elem * elem -> order) -> slice * slice -> order
end

(* 
   [slice] is the type of GenericWordVector slices, that is, sub-vectors of
   GenericWordVector.vector values.
   The slice (a,i,n) is valid if 0 <= i <= i+n <= size s, 
                or equivalently, 0 <= i and 0 <= n and i+n <= size s.  
   A valid slice sli = (a,i,n) represents the sub-vector a[i...i+n-1],
   so the elements of sli are a[i], a[i+1], ..., a[i+n-1], and n is
   the length of the slice.  Only valid slices can be constructed by
   these functions.

   All operations are as for VectorSlice.slice.
*)

functor GenericWordVectorSlice
  (structure WordVector : GenericWordVectorRepr
   structure AbstractVectorSlice : AbstractVectorSlice
      where type vector = WordVector.vector)
 :> GenericWordVectorSlice
      where type elem = WordVector.elem
        and type vector = WordVector.vector
        and type slice = AbstractVectorSlice.slice =
struct
   type elem = WordVector.elem
   type vector = WordVector.vector
   type slice = AbstractVectorSlice.slice
   local val base = AbstractVectorSlice.base
         val new = AbstractVectorSlice.new
   in
      fun slice (v, i, len) =
          let val vlen = WordVector.length v
          in 
              case len of 
                  NONE   => if 0 <= i andalso i <= vlen
                               then new (v, i, vlen - i)
                               else raise Subscript
                | SOME n => if 0 <= i andalso 0 <= n andalso n <= vlen - i
                               then new (v, i, n)
                               else raise Subscript
          end
      fun full v = new (v, 0, WordVector.length v)
      fun subslice (slc, i', NONE) =
          let val (v, i, n) = base slc
          in if 0 <= i' andalso i' <= n
                then new (v, i + i', n - i')
                else raise Subscript
          end
        | subslice (slc, i', SOME n') =
          let val (v, i, n) = base slc
          in if 0 <= i' andalso 0 <= n' andalso n' <= n - i'
                then new (v, i + i', n')
                else raise Subscript
          end
      val base = base
      fun vector slc =
         let val (vec : vector, i, len) = base slc
         in WordVector.tabulate (len, fn n => WordVector.sub (vec, i + n))
         end
      fun length slc =
          let val (_, _, n) = base slc
          in n end
      fun sub (slc, i) = 
          let val (v', i', n') = base slc
          in if i < 0 orelse i >= n'
                then raise Subscript
                else WordVector.sub (v', (i' + i))
          end
      fun update (slc, i, v) =
         let val (v', i', n') = base slc
         in if i < 0 orelse i >= n'
               then raise Subscript
               else WordVector.update (v', i' + i, v)
         end
      fun map f slc =
         let val (vec,i,len) = base slc
         in  WordVector.tabulate (len, fn i' => f (WordVector.sub (vec, i + i')))
         end
      fun mapi f slc =
         let val (vec, i, len) = base slc
         in WordVector.tabulate (len, fn i' => f (i + i', WordVector.sub (vec, i + i')))
         end
      fun isEmpty slc =
         let val (_, _, n) = base slc
         in n = 0
         end
      fun concat l = (* Again, this is going to be slow. *)
         let val len = 
                let fun iter [] n = n
                      | iter (slc::vs) n =
                         let val (_,_,len) = base slc
                         in iter vs (n + len)
                         end
                in iter l 0
                end
             fun subl [] n = raise Fail "GenericWordVectorSlice: internal error"
               | subl (slc::ss) n =
                  let val (v,i,len) = base slc
                  in if n < len
                        then WordVector.sub (v, n + i)
                        else subl ss (n - len)
                  end
         in if len > WordVector.maxLen
               then raise Size
               else WordVector.tabulate (len, fn i => subl l i)
         end
      fun getItem slc =
           case base slc
             of (_, _, 0) => NONE
              | (v, i, n) => SOME(WordVector.sub (v, i), new (v, i + 1, n - 1))
      fun find (p : elem -> bool) (slc : slice) : elem option = 
          let val (v, i, n) = base slc
              val stop = i + n
              fun lr j = 
                  if j < stop then 
                      if p (WordVector.sub (v, j))
                         then SOME (WordVector.sub (v, j))
                         else lr (j + 1)
                  else NONE
          in lr i end
      fun exists (p : elem -> bool) (slc : slice) : bool = 
          let val (a, i, n) = base slc
              val stop = i + n
              fun lr j = j < stop andalso (p (WordVector.sub (a, j)) orelse lr (j + 1))
          in lr i end
      fun all (p : elem -> bool) (slc : slice) : bool =
          let val (a,i,n) = base slc
              val stop = i + n
              fun lr j = j >= stop orelse (p (WordVector.sub (a, j)) andalso lr (j + 1))
          in lr i end
      fun app f slc = 
          let val (a, i, n) = base slc
              val stop = i + n
              fun lr j = if j < stop
                            then (f(WordVector.sub (a, j));
                                  lr (j + 1))
                            else ()
          in lr i end
      fun foldl f e slc = 
          let val (a, i, n) = base slc
              val stop = i + n
              fun lr j res = if j < stop
                                then lr (j + 1) (f(WordVector.sub (a, j), res))
                                else res
          in lr i e
          end
      fun foldr f e slc =
          let val (a, i, n) = base slc
              fun rl j res = if j >= i
                                then rl (j - 1) (f(WordVector.sub (a, j), res))
                                else res
          in rl (i + n - 1) e
          end
      fun findi (p : int * elem -> bool) (slc : slice) : (int * elem) option = 
          let val (a,i,n) = base slc
              val stop = i + n
              fun lr j = 
                  if j < stop
                     then if p (j, WordVector.sub (a, j))
                             then SOME (j, WordVector.sub (a, j))
                             else lr (j + 1)
                     else NONE
          in lr i
          end
      fun appi f slc = 
          let val (a, i, n) = base slc
              val stop = i + n
              fun lr j = 
                  if j < stop 
                     then (f(j, WordVector.sub (a, j));
                           lr (j + 1)) 
                     else ()
          in lr i
          end
      fun foldli f e slc = 
          let val (a, i, n) = base slc
              val stop = i + n
              fun lr j res = 
                  if j < stop
                     then lr (j+1) (f(j, WordVector.sub (a, j), res))
                     else res
          in lr i e
          end
      fun foldri f e slc = 
          let val (a, i, n) = base slc
              fun rl j res = 
                  if j >= i
                     then rl (j - 1) (f (j, WordVector.sub (a, j), res))
                     else res
          in rl (i + n - 1) e
          end
      fun collate cmp (slc1, slc2) =
          let val (v1, i1, n1) = base slc1
              val (v2, i2, n2) = base slc2
              val stop = if n1 < n2 then n1 else n2
              fun h j =
                  if j = stop then if      n1 < n2 then LESS
                                   else if n1 > n2 then GREATER
                                   else                 EQUAL
                  else
                      case cmp(WordVector.sub (v1, i1 + j),
                               WordVector.sub (v2, i2 + j)) of
                          EQUAL => h (j + 1)
                        | res   => res
          in h 0
          end
   end
end
