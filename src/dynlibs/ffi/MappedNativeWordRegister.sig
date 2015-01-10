type register = MappedNativeRegister.register
val ! : register -> word
val := : register * word -> unit
val new : MappedWord8ArraySlice.slice -> register
