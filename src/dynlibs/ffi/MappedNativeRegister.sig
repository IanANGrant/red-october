type register
val ! : register -> Word8Vector.vector
val := : register * Word8Vector.vector -> unit
val new : MappedWord8ArraySlice.slice -> register
