(* This makes instances of (key,'a) Redblackmap.dict which are
   serialisable because the serialised objects don't need embedded
   code blocks for the compare function: it is added at the other
   side. *)
 
signature RedBlackMap =
sig
   type state
   type key
   type 'a dict
   val mkDict : unit -> 'a dict
   val encode : 'a dict * state -> state
   val decode : state -> 'a dict
end

functor RedBlackMap
   (type key
    val compare : key * key -> order
    structure ObjRepr : ObjRepr) :> RedBlackMap 
       where type state = ObjRepr.state
         and type key = key
         and type 'a dict = (key,'a) Redblackmap.dict =
struct
   type state = ObjRepr.state
   type key = key
   type 'a dict = (key,'a) Redblackmap.dict
   local
      fun mkDict () = Redblackmap.mkDict(compare)
      fun encode (map,buff) =
         let val maprepr = Redblackmap.toRepr map
             val _ = ObjRepr.encode (maprepr,buff)
         in buff
         end
     fun decode buff =
        let val maprepr : (key,'a) Redblackmap.dictRepr = ObjRepr.decode buff
        in Redblackmap.fromRepr compare maprepr
        end
   in
      val mkDict : unit -> 'a dict
          = mkDict
      val encode : 'a dict * state -> state
          = encode
      val decode : state -> 'a dict
          = decode
   end
end
