(* GenericWordArraySlice -- SML Basis Library *)

signature GenericWordArraySlice =
sig
   type elem
   type array
   type vector
   type vector_slice

   type slice

   val length   : slice -> int
   val sub      : slice * int -> elem
   val update   : slice * int * elem  -> unit
   val slice    : array * int * int option -> slice
   val full     : array -> slice
   val subslice : slice * int * int option -> slice
   val base     : slice -> array * int * int
   val vector   : slice -> vector
   val copy     : {src: slice, dst: array, di: int} -> unit
   val copyVec  : {src: vector_slice, dst: array, di: int} -> unit 
   val isEmpty  : slice -> bool
   val getItem  : slice -> (elem * slice) option

   val find     : (elem -> bool) -> slice -> elem option
   val exists   : (elem -> bool) -> slice -> bool
   val all      : (elem -> bool) -> slice -> bool

   val app      : (elem -> unit) -> slice -> unit
   val foldl    : (elem * 'b -> 'b) -> 'b -> slice -> 'b
   val foldr    : (elem * 'b -> 'b) -> 'b -> slice -> 'b
   val modify   : (elem -> elem) -> slice -> unit

   val findi    : (int * elem -> bool) -> slice -> (int * elem) option
   val appi     : (int * elem -> unit) -> slice -> unit
   val foldli   : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
   val foldri   : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
   val modifyi  : (int * elem -> elem) -> slice -> unit

   val collate  : (elem * elem -> order) -> slice * slice -> order
end

(* 
   [slice] is the type of GenericWordArray slices, that is, sub-arrays of
   GenericWordArray.array values.
   The slice (a,i,n) is valid if 0 <= i <= i+n <= size s, 
                or equivalently, 0 <= i and 0 <= n and i+n <= size s.  
   A valid slice sli = (a,i,n) represents the sub-array a[i...i+n-1],
   so the elements of sli are a[i], a[i+1], ..., a[i+n-1], and n is
   the length of the slice.  Only valid slices can be constructed by
   the functions below.

   All operations are as for ArraySlice.slice.
*)

functor GenericWordArraySlice
  (structure WordArray : GenericWordArrayRepr
   structure AbstractArraySlice : AbstractArraySlice
       where type array = WordArray.array
   structure WordVector : GenericWordVectorRepr
       where type elem = WordArray.elem
         and type vector = WordArray.vector
   structure WordVectorSlice : GenericWordVectorSlice
       where type elem = WordArray.elem
         and type vector = WordArray.vector)
 :> GenericWordArraySlice
      where type elem = WordArray.elem
        and type array = WordArray.array
        and type vector = WordArray.vector
        and type vector_slice = WordVectorSlice.slice
        and type slice = AbstractArraySlice.slice =
struct
   type elem = WordArray.elem
   type array = WordArray.array
   type vector = WordArray.vector
   type vector_slice = WordVectorSlice.slice
   type slice = AbstractArraySlice.slice
   local
      val base = AbstractArraySlice.base
      val new = AbstractArraySlice.new
   in
      fun slice (a, i, len) =
          let val alen = WordArray.length a
          in 
              case len of 
                  NONE   => if 0 <= i andalso i <= alen
                               then new (a, i, alen - i)
                               else raise Subscript
                | SOME n => if 0 <= i andalso 0 <= n andalso n <= alen - i
                               then new (a, i, n)
                               else raise Subscript
          end
      fun full a = new (a, 0, WordArray.length a)
      fun subslice (slc, i', NONE) =
          let val (a, i, n) = base slc
          in if 0 <= i' andalso i' <= n
                then new (a, i + i', n - i')
                else raise Subscript
          end
        | subslice (slc, i', SOME n') =
          let val (a, i, n) = base slc
          in if 0 <= i' andalso 0 <= n' andalso n' <= n - i'
                then new (a, i + i', n')
                else raise Subscript
          end
      val base = base
      fun vector slc = 
         let val (arr : array, i, len) = base slc
         in WordVector.tabulate (len, fn n => WordArray.sub (arr, i + n))
         end
      fun length slc =
          let val (_, _, n) = base slc
          in n end
      fun sub (slc, i) = 
          let val (a', i', n') = base slc
          in if i < 0 orelse i >= n'
                then raise Subscript
                else WordArray.sub (a', (i' + i))
          end
      fun update (slc, i, v) =
         let val (a', i', n') = base slc
         in if i < 0 orelse i >= n'
               then raise Subscript
               else WordArray.update (a', i' + i, v)
         end
      fun map f slc =
         let val (arr,i,len) = base slc
         in  WordArray.tabulate (len, fn i' => f (WordArray.sub (arr, i + i')))
         end
      fun mapi f slc =
         let val (arr, i, len) = base slc
         in WordArray.tabulate (len, fn i' => f (i + i', WordArray.sub (arr, i + i')))
         end
      fun isEmpty slc =
         let val (_, _, n) = base slc
         in n = 0
         end
      fun copy {src=slc : slice, dst=a2: array, di=i2} =
         let val (a1,i1,n) = base slc
         in if i2<0 orelse i2+n > WordArray.length a2
               then raise Subscript
               else if i1 < i2
                       then (* copy from high to low *)
                          let fun hi2lo j = 
                                   if j >= 0
                                      then (WordArray.update (a2, i2 + j,
                                            WordArray.sub (a1, i1 + j));
                                            hi2lo (j - 1))
                                      else ()
                          in hi2lo (n - 1)
                          end
                       else (* i1 >= i2, copy from low to high *)
                          let fun lo2hi j = 
                                   if j < n
                                      then (WordArray.update (a2, i2 + j,
                                            WordArray.sub (a1, i1 + j));
                                            lo2hi (j + 1))
                                      else ()
                          in lo2hi 0
                          end
         end
      fun copyVec {src : vector_slice, dst = a2: array, di = i2} =
         let val n = WordVectorSlice.length src
         in if i2 < 0 orelse i2 + n > WordArray.length a2
               then raise Subscript
               else let fun lo2hi j =
                           if j < n
                              then (WordArray.update (a2, i2 + j,
                                    WordVectorSlice.sub (src, j));
                                    lo2hi (j + 1))
                              else ()
                    in lo2hi 0
                    end
         end
      fun getItem slc =
           case base slc
             of (a, i, 0) => NONE
              | (a, i, n) => SOME(WordArray.sub (a, i), new (a, i + 1, n - 1))
      fun find (p : elem -> bool) (slc : slice) : elem option = 
          let val (a, i, n) = base slc
              val stop = i + n
              fun lr j = 
                  if j < stop then 
                      if p (WordArray.sub (a, j))
                         then SOME (WordArray.sub (a, j))
                         else lr (j + 1)
                  else NONE
          in lr i end
      fun exists (p : elem -> bool) (slc : slice) : bool = 
          let val (a, i, n) = base slc
              val stop = i + n
              fun lr j = j < stop andalso (p (WordArray.sub (a, j)) orelse lr (j + 1))
          in lr i end
      fun all (p : elem -> bool) (slc : slice) : bool =
          let val (a,i,n) = base slc
              val stop = i + n
              fun lr j = j >= stop orelse (p (WordArray.sub (a, j)) andalso lr (j + 1))
          in lr i end
      fun app f slc = 
          let val (a, i, n) = base slc
              val stop = i + n
              fun lr j = if j < stop
                            then (f (WordArray.sub (a, j));
                                  lr (j + 1))
                            else ()
          in lr i end
      fun foldl f e slc = 
          let val (a, i, n) = base slc
              val stop = i + n
              fun lr j res = if j < stop
                                then lr (j + 1) (f (WordArray.sub (a, j), res))
                                else res
          in lr i e
          end
      fun foldr f e slc =
          let val (a, i, n) = base slc
              fun rl j res = if j >= i
                                then rl (j - 1) (f (WordArray.sub (a, j), res))
                                else res
          in rl (i + n - 1) e
          end
      fun modify f slc = 
         let val (a, i, n) = base slc
             val stop = i + n
             fun lr j = if j < stop
                           then (WordArray.update (a, j, f (WordArray.sub (a, j)));
                                 lr (j + 1))
                           else ()
         in lr i
         end
      fun findi (p : int * elem -> bool) (slc : slice) : (int * elem) option = 
          let val (a,i,n) = base slc
              val stop = i + n
              fun lr j = 
                  if j < stop
                     then if p (j, WordArray.sub (a, j))
                             then SOME (j, WordArray.sub (a, j))
                             else lr (j + 1)
                     else NONE
          in lr i
          end
      fun appi f slc = 
          let val (a, i, n) = base slc
              val stop = i + n
              fun lr j = 
                  if j < stop
                     then (f (j, WordArray.sub (a, j));
                           lr (j + 1))
                     else ()
          in lr i
          end
      fun foldli f e slc = 
          let val (a, i, n) = base slc
              val stop = i + n
              fun lr j res = 
                  if j < stop
                     then lr (j+1) (f (j, WordArray.sub (a, j), res))
                     else res
          in lr i e
          end
      fun foldri f e slc = 
          let val (a, i, n) = base slc
              fun rl j res = 
                  if j >= i
                     then rl (j - 1) (f (j, WordArray.sub (a, j), res))
                     else res
          in rl (i + n - 1) e
          end
      fun modifyi f slc = 
          let val (a, i, n) = base slc
              val stop = i + n
              fun lr j = if j < stop
                            then (WordArray.update (a, j, f (j,WordArray.sub (a, j)));
                                  lr (j + 1))
                            else ()
          in lr i
          end
      fun collate cmp (slc1, slc2) =
          let val (a1, i1, n1) = base slc1
              val (a2, i2, n2) = base slc2
              val stop = if n1 < n2 then n1 else n2
              fun h j =
                  if j = stop then if      n1 < n2 then LESS
                                   else if n1 > n2 then GREATER
                                   else                 EQUAL
                  else case cmp(WordArray.sub (a1, i1 + j),
                                WordArray.sub (a2, i2 + j))
                         of EQUAL => h (j + 1)
                          | res   => res
          in h 0
          end
   end
end
