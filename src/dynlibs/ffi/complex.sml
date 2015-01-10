(* Translating the code in Gibbons "Unfolding ADTs" from Haskell. It's far
   easier for me to understand the ML. *)

val _ = load "Math";

abstype Complex = C of {add : Complex -> Complex,
                        mdl : real,
                        arg : real,
                        rea : real,
                        ima : real}
with
   fun new (op +) rfn ifn mfn afn =
      fn z => 
         let fun self z =
             C {add = fn c => self (z + c),
                mdl = mfn z,
                arg = afn z,
                rea = rfn z,
                ima = ifn z}
         in self z
         end
   fun add (C c) = #add c
   fun rea (C c) = #rea c
   fun ima (C c) = #ima c
   fun mdl (C c) = #mdl c
   fun arg (C c) = #arg c
end

(* We can use pattern matching in the implementation, and we don't
   need any recusrion: all the recursion is in the co-datatype. Also,
   all the polymorphism is in the implementation, and there's none in
   the co-datatype. *)

fun newC (c1,c2) =
   new (fn ((x',y'),c) => (x'+ (rea c),y'+ (ima c)))
       (fn (x,_) => x)
       (fn (_,y) => y) 
       (fn (x,y) => Math.sqrt (x*x+y*y))
       (fn (x,y) => Math.atan2 (x,y))
       (c1,c2)
and newP (c1,c2) =
   new (fn ((x,y),c) =>
          let val (x,y) = ((x * (Math.cos y)) + (rea c),
                           (x * (Math.sin y)) + (ima c))
              val m = Math.sqrt (x*x+y*y)
              val a = Math.atan2 (x,y)
          in (m,a) end)
       (fn (x,y) => x * (Math.cos y))
       (fn (x,y) => x * (Math.sin y))
       (fn (x,_) => x)
       (fn (_,y) => y) 
       (c1,c2);

val orig = newC (0.0,0.0);
val (r0,i0) = (rea orig,ima orig);
val one = newC (1.0,0.0);
val i = newC (0.0,1.0);
val two = add one one;
val (r2,i2) = (rea two,ima two);
val oneplusi = add one i;
val (m1i,a1i) = (mdl oneplusi,arg oneplusi);

val one' = newP (1.0,0.0);
val i' = newP (1.0,Math.pi / 2.0);
val oneplusi' = add one' i';
val (r0',i0') = (rea oneplusi',ima oneplusi');

val two' = add one one';
val (r2',i2') = (rea two',ima two');

