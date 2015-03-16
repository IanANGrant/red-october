signature AbstractVector =
sig
   type vector
   type elem
   val new : ('a * int -> elem) ->
             ('a -> int) ->
               'a -> vector
   val sub : vector * int -> elem
   val length : vector -> int
end

functor AbstractVector
  (type elem)
   :> AbstractVector
    where type elem = elem =
struct
   type elem = elem
   abstype vector =
      Repr of {sub : int -> elem,
               length : int}
   with
      fun new subf lengthf =
         fn s => 
            let fun self s =
                 Repr {sub = fn i => subf (s,i),
                       length = lengthf s}
            in self s
            end
      fun sub (Repr s,i) = #sub s i
      fun length (Repr s) = #length s
   end
end
