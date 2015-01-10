signature ConcRep =
sig
   type X
   type Y
   type S
   val array : int * Y -> X
   val length : X -> int
   val sub : X * int -> Y
   val update : X * int * Y -> unit
   val tabulate : int * (int -> Y) -> X
   val concat : X list -> X
   val foldr : (Y * 'a -> 'a) -> 'a -> X -> 'a
   val foldl : (Y * 'a -> 'a) -> 'a -> X -> 'a
   val app : (Y -> unit) -> X -> unit
   val appi : (int * Y -> unit) -> X -> unit
   val vector : X -> Y Vector.vector
   val fromList : Y list -> X
   val subslice : X * int * int option -> X
   val slice : X * int * int option -> S
   val fromSlice : S -> X
   val zero : Y
end

functor ConcRep
  (structure PatchStruct : ArrayPatch
   structure ArrayStruct : GenericArray
        where type elem = PatchStruct.elem
          and type array = PatchStruct.array
   structure ArraySliceStruct : GenericArraySlice
        where type array = PatchStruct.array
          and type slice = PatchStruct.slice
   val zero : PatchStruct.elem)
 :> ConcRep 
      where type X = PatchStruct.patch
        and type Y = PatchStruct.elem
        and type S = ArraySliceStruct.slice =
struct
   local
   in
      type X = PatchStruct.patch
      type Y = PatchStruct.elem
      type S = ArraySliceStruct.slice
      open PatchStruct
      val subslice = subpatch
      val array = PatchStruct.fromArray o ArrayStruct.array
      fun vector p =
           Vector.tabulate
              (PatchStruct.length p,
               fn i => PatchStruct.sub(p,i))
      fun fromList l =
         let val arr = ArrayStruct.fromList l
         in PatchStruct.fromArray arr
         end
      val zero = zero
   end
end

structure Patch = ArrayPatch
    (structure ArrayStruct = StaticWord8Array : GenericArray
                  where type elem = Word8.word
     structure ArraySliceStruct = StaticWord8ArraySlice : GenericArraySlice
                  where type elem = Word8.word
     val zero = 0w0) :> ArrayPatch
      where type elem = Word8.word
        and type array = StaticWord8Array.array
        and type slice = StaticWord8ArraySlice.slice
        and type vector = Word8Array.vector

structure CharPatch = ArrayPatch
    (structure ArrayStruct = CharArray : GenericArray
                  where type elem = Char.char
     structure ArraySliceStruct = CharArraySlice : GenericArraySlice
                  where type elem = Char.char
     val zero = #" ") :> ArrayPatch
      where type elem = Char.char
        and type array = CharArray.array
        and type slice = CharArraySlice.slice
        and type vector = CharArray.vector

structure ConcRep = 
 ConcRep
  (structure PatchStruct = Patch
   structure ArrayStruct = StaticWord8Array
   structure ArraySliceStruct = StaticWord8ArraySlice
   val zero = 0w0)

structure StringRep = 
 ConcRep
  (structure PatchStruct = CharPatch
   structure ArrayStruct = CharArray
   structure ArraySliceStruct = CharArraySlice
   val zero = #" ")
