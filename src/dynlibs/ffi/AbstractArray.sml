
signature AbstractArray =
sig
   type array
   type vector
   type elem
   val new : ('a * int -> elem) ->
             ('a * int * elem -> unit) ->
             ('a -> int) ->
             ('a -> vector) ->
               'a -> array
   val sub : array * int -> elem
   val update : array * int * elem -> unit
   val length : array -> int
   val vector : array -> vector
end

functor AbstractArray
  (type elem
   type vector)
   :> AbstractArray
    where type elem = elem
      and type vector = vector =
struct
   type elem = elem
   type vector = vector
   abstype array =
      Repr of {sub : int -> elem,
               update : int * elem -> unit,
               length : int,
               vector : vector}
   with
      fun new subf updatef lengthf vectorf =
         fn s => 
            let fun self s =
                 Repr {sub = fn i => subf (s,i),
                       update = fn (i,v) => updatef (s,i,v),
                       length = lengthf s,
                       vector = vectorf s}
            in self s
            end
      fun sub (Repr s,i) = #sub s i
      fun update (Repr s,i,e) = #update s (i,e)
      fun length (Repr s) = #length s
      fun vector (Repr s) = #vector s
   end
end
