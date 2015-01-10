type register
val ! : register -> word
val := : register * word -> unit
val new : MappedWord8ArraySlice.slice -> register
