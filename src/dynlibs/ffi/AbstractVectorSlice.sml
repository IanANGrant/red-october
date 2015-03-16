signature AbstractVectorSlice =
sig
   type vector
   type slice
   val new : vector * int * int -> slice
   val base : slice -> vector * int * int
end

functor AbstractVectorSlice
  (type vector)
    :> AbstractVectorSlice
         where type vector = vector =
struct
   type vector = vector
   abstype slice = Repr of {base : vector * int * int}
   with
      val new =
         fn s => 
            let fun self s = Repr {base=s}
            in self s
            end
      fun base (Repr s) = #base s
   end
end
