functor ArrayPatch
   (structure ArrayStruct : GenericArray
    structure ArraySliceStruct : GenericArraySlice
           where type array = ArrayStruct.array)
      :> GenericArrayPatch 
           where type slice = ArraySliceStruct.slice
             and type vector = ArraySliceStruct.vector =
struct
   datatype patch_ =
        Slice of slice
      | Branch of patch_ * patch_
      | Patch of patch
   withtype patch = patch_ ref
