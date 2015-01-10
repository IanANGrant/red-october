(* Ken's written a really nice red-black tree implementation, so let's
   just use that. We'll keep the index of the first element, and the
   length of each slice in the patch in the rbtree.

   We can't use an ordering relation on the index of the first element
   alone, because we need to be able to find slices when we don't know
   how long they are. So let's use this relation, under the condition
   that the array is always represented in the tree as a partition of
   the total space (empty slices will be represented in the tree by a
   placeholder slice) and that we will only ever insert slices which
   exactly fill some gap made by deleting another slice. Then when we
   search for a slice, we search for one of length one. Under these
   conditions, the following pre-order is a total order:

       (s,l) < (s',l') iff s + l < s'
       (s,l) > (s',l') iff s > s' + l'
       (s,l) = (s',l') otherwise

    To see this, just think of the partitions of elements as
    equivalence classes of the elements, then quotienting the above
    pre-order by the equivalence classes yields a total order. This
    works because the conditions are such that whenever two slices
    overlap, one of them is a one-element slice, and therefore that
    slice can be identified as belonging to one and only one
    equivalence class, which is the one it just so happens to coincide
    with. Euclid's 4th axiom: things coincident with one another are
    equal!

    But the neater thing to do is add a function to the red-black tree
    unit, and so we now have [partition m k] which returns a triple
    (bs,kvpo,as) where the bs are a (descending) list of key-value
    pairs of elements before k, the as are a list (in ascending order)
    of those after it, and kvpo is a key-value pair option. And there
    is also another variant on find called bounds, which returns the
    key-value pair, if any, as well as those on either side. Then
    there are the related functions gle, glt, lge and lgt which are
    the greatest element less than or equal to, the greatest element
    less than, the least element greater than or equal to, and the
    least element greater than the one sought-for.  *)

signature SparseArray =
sig
   type elem
   type array
   type vector

   val maxLen   : int

   val array    : int * elem -> array
   val tabulate : int * (int -> elem) -> array
   val fromList : elem list -> array

   val length   : array -> int
   val sub      : array * int -> elem 

   val update   : array * int * elem -> unit
   (* val vector   : array -> vector *)

end

functor SparseArray
 (structure Array : GenericArray
  structure ArraySlice : GenericArraySlice 
     where type array = Array.array
     and type elem = Array.elem)
   :> SparseArray 
     where type elem = Array.elem
       and type vector = Array.vector =
struct

   type vector = Array.vector
   type elem = Array.elem

   datatype array_ =
      Slice of Array.array * int * int
    | Tab of int * (int -> elem)
    | Elem of elem

   type array = (int, array_) Redblackmap.dict ref;

   local open Redblackmap
      val WORDSIZE = 0w32 (* XXX : for now *)
      val BYTESPERWORD = WORDSIZE div 0w8
      val TAGBITS = 0w10

      val maxLen = Word.toInt 
                    (((Word.<<(0w1,WORDSIZE - TAGBITS)) - 0w1)
                     * BYTESPERWORD - 0w1)

      fun array (len,e) =
          let val dict = mkDict(Int.compare)
          in ref (insert (dict,0,Tab(len,fn _ => e)))
          end

      fun tabulate (len,f) =
          let val dict = mkDict(Int.compare)
          in ref (insert (dict,0,(Tab (len,f))))
          end

      fun fromList l =
          let val dict = mkDict(Int.compare)
              val arr = Array.fromList l
              val len = Array.length arr
          in ref (insert (dict,0,(Slice (arr,0,len))))
          end

      fun length (ref a) =
          let fun foldfn (_,(Tab (l,_)),n) = n + l 
                | foldfn (_,(Slice (_,_,l)),n) = n + l 
                | foldfn (_,(Elem _),n) = n + 1
          in foldl foldfn 0 a
          end

      fun sub ((ref a),i) =
         case gle (a,i)
           of NONE => raise Subscript
            | SOME (offs,Elem e) => e
            | SOME (offs,Slice (a,n,l)) => Array.sub (a,n+i-offs)
            | SOME (offs,Tab (l,f)) =>
                  let val j = i-offs
                  in if j < l
                        then f j
                        else raise Subscript
                  end

      fun update (a_ as (ref a),i,v) =
         case gle (a,i)
           of NONE => raise Subscript
            | SOME (offs,Elem e) => 
                 if offs = i
                    then a_ := (insert (a,i,Elem v))
                    else raise Subscript
            | SOME (offs,Slice (a,n,l)) =>
                    Array.update (a,n+i-offs,v)
            | SOME (offs,Tab (l,f)) =>
                  let val j = i-offs
                      fun split k =
                          let val _ = remove (a,offs)
                              val a' = if k > 0
                                          then insert (a,offs,Tab(k,f))
                                          else a
                              val offs' = offs+k+1
                              val a'' = if k < l-1
                                           then insert (a',offs',Tab (l-k-1,fn i => f (i+offs')))
                                           else a'
                          in a_ := (insert (a'',offs+k,Elem v))
                          end
                  in if j < l
                        then split j
                        else raise Subscript
                  end
   in
      val maxLen   : int
            = maxLen
      val array    : int * elem -> array
            = array
      val tabulate : int * (int -> elem) -> array
            = tabulate
      val fromList : elem list -> array
            = fromList
      val length   : array -> int
            = length
      val sub      : array * int -> elem 
            = sub
      val update   : array * int * elem -> unit
            = update
   end
end
