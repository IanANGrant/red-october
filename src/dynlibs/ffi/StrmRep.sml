signature StrmRep =
sig
   type S
   type T
   val getItem : S -> (T * S) option
   val setItem : S * T -> S
end

functor ConcStrmRep
   (structure ConcRep : ConcRep    
    type T
    val length : int
    val conv : ConcRep.X -> T
    val iconv : T -> ConcRep.X)
 :> StrmRep
       where type S = ConcRep.X
         and type T = T
= struct
   type S = ConcRep.X
   type T = T
   fun getItem slc =
         SOME (conv (ConcRep.subslice (slc,0,SOME length)),
                     ConcRep.subslice (slc,length,NONE))
                  handle Size => NONE
   fun setItem (slc,item) =
      let val object = iconv item
          val l = ConcRep.length (object)
      in  ConcRep.concat [slc,object]
      end
end

functor ConcConcStrmRep
   (structure ConcRep : ConcRep
    val length : int) :> StrmRep
       where type S = ConcRep.X
         and type T = ConcRep.X =
 ConcStrmRep
   (structure ConcRep = ConcRep : ConcRep    
    type T = ConcRep.X
    val length = length
    val conv : (ConcRep.X -> T) = fn x => x
    val iconv : (T -> ConcRep.X) = fn x => x)
 :> StrmRep
       where type S = ConcRep.X
         and type T = ConcRep.X

functor TextStrm
  (type T
   structure ConcRep : ConcRep
      where type S = CharArraySlice.slice
   val scanner : ConcRep.X -> (T * ConcRep.X) option
   val printer : ConcRep.X * T -> ConcRep.X)
 :> StrmRep where type S = ConcRep.X
              and type T = T =
struct
   type S = ConcRep.X
   type T = T
   val getItem : S -> (T * S) option = scanner
   val setItem : S * T -> S = printer
end
