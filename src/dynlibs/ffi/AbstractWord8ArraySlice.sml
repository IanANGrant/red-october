structure AbstractWord8ArraySlice =
 AbstractArraySlice
  (type array = AbstractWord8Array.array)
    :> AbstractArraySlice
      where type array = AbstractWord8Array.array
