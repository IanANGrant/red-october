type register = MappedNativeRegister.register
val ! : register -> Dynlib.cptr
val := : register * Dynlib.cptr -> unit
val new : MappedWord8ArraySlice.slice -> register
