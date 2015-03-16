
signature AbstractArraySlice =
sig
   type array
   type slice
   val new : array * int * int -> slice
   val base : slice -> array * int * int
end

functor AbstractArraySlice
  (type array)
    :> AbstractArraySlice
         where type array = array =
struct
   type array = array
   abstype slice = Repr of {base : array * int * int}
   with
      val new =
         fn s => 
            let fun self s = Repr {base=s}
            in self s
            end
      fun base (Repr s) = #base s
   end
end
