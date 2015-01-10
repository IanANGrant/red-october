val _ = load "MappedRegister";
val _ = load "MappedWord8ArraySlice";

val arr = MappedWord8Array.array (8,0w0);
val reg1 = MappedRegister.new (MappedWord8ArraySlice.slice (arr,0,SOME 4));
val reg2 = MappedRegister.new (MappedWord8ArraySlice.slice (arr,4,SOME 4));

local open MappedRegister
      infix 2 :=
in
   val res1 = !reg1
   val res2 = !reg2
   val _ = reg1 := 0wx7fffffff
   val _ = reg2 := 0wxfe
   val res1' = !reg1
   val res2' = !reg2
   val _ = reg1 := !reg1 + 0w1
   val _ = reg2 := !reg2 + 0w1
   val res1'' = !reg1
   val res2'' = !reg2
end
