structure AbstractRepr =
struct
   abstype 'a repr =
      Repr of {get : 'a,
               set : 'a -> unit,
               copy : 'a repr -> 'a repr}
   with
      fun new getf setf copyf =
         fn s => 
            let fun self s =
                 Repr {get = getf s,
                       set = fn v => setf (s,v),
                       copy = fn (Repr r) => self (copyf (#get r))}
            in self (copyf s)
            end
      fun get (Repr s) = #get s
      fun set (Repr s) = #set s
      fun copy (Repr s) = #copy s
   end
end

