(* GenericWordArray -- SML Basis Library *)

signature GenericWordArray =
sig
   type array
   type elem
   type vector
   
   val maxLen   : int
   
   val array    : int * elem -> array
   val tabulate : int * (int -> elem) -> array
   val fromList : elem list -> array
   
   val length   : array -> int
   val sub      : array * int -> elem
   val update   : array * int * elem -> unit
   val vector   : array -> vector
   
   val copy     : {src: array,  dst: array, di: int} -> unit
   val copyVec  : {src: vector, dst: array, di: int} -> unit
   
   val find     : (elem -> bool) -> array -> elem option
   val exists   : (elem -> bool) -> array -> bool
   val all      : (elem -> bool) -> array -> bool
   
   val app      : (elem -> unit) -> array -> unit
   val foldl    : (elem * 'b -> 'b) -> 'b -> array -> 'b
   val foldr    : (elem * 'b -> 'b) -> 'b -> array -> 'b
   val modify   : (elem -> elem) -> array -> unit
   
   val findi    : (int * elem -> bool) -> array -> (int * elem) option
   val appi     : (int * elem -> unit) -> array -> unit
   val foldli   : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
   val foldri   : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
   val modifyi  : (int * elem -> elem) -> array -> unit
   
   val collate  : (elem * elem -> order) -> array * array -> order
end

(* GenericWordArrayRepr *)

signature GenericWordArrayRepr =
sig
   include GenericWordArray
   type repr
   val fromArray : repr -> array
end

functor GenericWordArray
  (structure AbstractArray : AbstractArray
   structure WordVector : GenericWordVectorRepr
      where type elem = AbstractArray.elem
        and type vector = AbstractArray.vector
   structure WordArray : GenericWordArray
      where type elem = AbstractArray.elem
        and type vector = WordVector.repr)
    :> GenericWordArrayRepr
          where type array = AbstractArray.array
            and type vector = AbstractArray.vector
            and type repr = WordArray.array
            and type elem = AbstractArray.elem =
struct
   type repr = WordArray.array
   type array = AbstractArray.array
   type vector = AbstractArray.vector
   type elem = AbstractArray.elem
   val maxLen = WordArray.maxLen
   local
      val vector = WordVector.fromVector o WordArray.vector
      fun init a = AbstractArray.new WordArray.sub WordArray.update WordArray.length vector a
   in
      val array = init o WordArray.array
      val tabulate = init o WordArray.tabulate
      val fromList = init o WordArray.fromList
      val fromArray = init
      val length = AbstractArray.length
      val sub = AbstractArray.sub
      val update = AbstractArray.update
      val vector = AbstractArray.vector
      fun copy {src = a1: array, dst = a2: array, di=i2} =
          let val stop = length a1
              fun lr j = if j < stop
                            then (update (a2, i2+j,sub (a1, j));
                                  lr (j + 1))
                            else ()
          in if i2 < 0 orelse i2+stop > length a2
                then raise Subscript
                else lr 0
          end
      fun copyVec {src = v1: vector, dst = a2: array, di=i2} =
          let val stop = WordVector.length v1
              fun lr j = if j < stop
                            then (update (a2, i2 + j,WordVector.sub (v1, j));
                                  lr (j + 1))
                            else ()
          in if i2 < 0 orelse i2+stop > length a2
                then raise Subscript
                else lr 0
          end
      fun find (p : elem -> bool) a : elem option = 
          let val stop = length a
              fun lr j = 
                  if j < stop then 
                      if p (sub (a, j)) then SOME (sub (a,j)) else lr (j + 1)
                  else NONE
          in lr 0 end
      fun exists (p : elem -> bool) a : bool = 
          let val stop = length a
              fun lr j = j < stop andalso (p (sub (a, j)) orelse lr (j + 1))
          in lr 0 end
      fun all (p : elem -> bool) a : bool = 
          let val stop = length a
              fun lr j = j >= stop orelse (p (sub (a,j)) andalso lr (j + 1))
          in lr 0 end
      fun foldl f e a = 
          let val stop = length a
              fun lr j res = if j < stop then lr (j+1) (f(sub (a, j), res))
                             else res
          in lr 0 e end
      fun foldr f e a =
          let fun rl j res = if j >= 0 then rl (j - 1) (f(sub (a, j), res))
                             else res
          in rl (length a - 1) e end
      fun modify f a = 
          let val stop = length a
              fun lr j = if j < stop
                            then (update (a, j, f(sub (a, j)));
                                  lr (j + 1))
                            else ()
          in lr 0 end
      fun app f a = 
          let val stop = length a
              fun lr j = if j < stop then (f(sub (a, j)); lr (j + 1))
                         else ()
          in lr 0 end
      fun findi (p : int * elem -> bool) a : (int * elem) option = 
          let val stop = length a
              fun lr j = 
                  if j < stop then 
                      if p (j, sub (a, j)) then SOME (j, sub (a, j)) else lr (j + 1)
                  else NONE
          in lr 0 end
      fun foldli f e a = 
          let val stop = length a 
              fun lr j res = 
                  if j < stop then lr (j + 1) (f(j, sub (a, j), res))
                  else res
          in lr 0 e end
      fun foldri f e a = 
          let fun rl j res = 
                  if j >= 0 then rl (j - 1) (f(j, sub (a, j), res))
                  else res
          in rl (length a - 1) e end
      fun modifyi f a = 
          let val stop = length a
              fun lr j = 
                  if j < stop
                     then (update (a, j, f(j, sub (a, j)));
                           lr (j + 1))
                     else ()
          in lr 0 end
      fun appi f a = 
          let val stop = length a
              fun lr j = 
                  if j < stop
                     then (f(j, sub (a, j));
                           lr (j + 1))
                     else ()
          in lr 0
          end
      fun collate cmp (a1, a2) =
          let val n1 = length a1 
              and n2 = length a2
              val stop = if n1 < n2 then n1 else n2
              fun h j =
                  if j = stop then if      n1 < n2 then LESS
                                   else if n1 > n2 then GREATER
                                   else                 EQUAL
                  else
                      case cmp(sub (a1, j), sub (a2, j)) of
                          EQUAL => h (j+1)
                        | res   => res
          in h 0 end
   end
end
