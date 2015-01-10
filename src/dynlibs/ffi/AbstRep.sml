signature AbstRep =
sig
   type X
   type Y
   val set : Y -> X
   val get : X -> Y
end

functor AbstConcStrmRep
   (structure ConcRep : ConcRep    
    structure AbstRep : AbstRep
       where type X = ConcRep.X
    val length : int) :> StrmRep
       where type S = ConcRep.X
         and type T = AbstRep.Y =
 ConcStrmRep
   (structure ConcRep = ConcRep : ConcRep    
    type T = AbstRep.Y
    val length = length
    val conv : (ConcRep.X -> T) = AbstRep.get
    val iconv : (T -> ConcRep.X) = AbstRep.set)
 :> StrmRep
       where type S = ConcRep.X
         and type T = AbstRep.Y

functor AbstConcConcStrmRep
   (structure ConcRep : ConcRep
    structure AbstRep : AbstRep
          where type X = ConcRep.X
    val length : int)
  :> sig structure Conc : StrmRep
         structure Abst : StrmRep
     end where type Conc.T = ConcRep.X
           and type Conc.S = ConcRep.X
           and type Abst.T = AbstRep.Y
           and type Abst.S = ConcRep.X =
struct
   structure Conc =
    ConcConcStrmRep
      (structure ConcRep = ConcRep : ConcRep    
       val length = length) :> StrmRep
          where type S = ConcRep.X
            and type T = ConcRep.X
   structure Abst =
     AbstConcStrmRep
      (structure ConcRep = ConcRep : ConcRep    
       structure AbstRep = AbstRep : AbstRep
          where type X = ConcRep.X
       val length = length) :> StrmRep
          where type S = ConcRep.X
            and type T = AbstRep.Y
end

(* The idea here is that we can change the concrete foundation of an
   abstract representation by using an abstract stream "connected
   up-side down" if you see what I mean. It doesn't matter what the
   AbstRep's concrete representation is, we can use the stream to
   translate it to a different one.

   For example, if we had an abstract representation of an IPv4 packet
   as certain ML datatype, and concretely stored in a static buffer on
   a little-endian machine, then we could generate a new abstract
   representation of that data as a text stream, so we would have the
   ability to print out IPv4 network packets from a network interface
   we implemented using that buffer. And without writing any more code
   or introducing any more bugs.  *)
 
functor TranslateAbstRep
   (structure ConcRep : ConcRep
    structure AbstRep : AbstRep
    structure StrmRep : StrmRep
        where type S = ConcRep.X
    val conv : AbstRep.Y -> StrmRep.T
    val iconv : StrmRep.T -> AbstRep.Y
    exception Stream)
    :> AbstRep
       where type X = ConcRep.X
         and type Y = AbstRep.Y =
struct
   type X = ConcRep.X
   type Y = AbstRep.Y
   val set : Y -> X =
       let val buf = ConcRep.fromList []
       in fn y => StrmRep.setItem (buf,conv y)
       end
   val get : X -> Y =
          fn x => (case StrmRep.getItem x
                     of SOME (y,_) => iconv y
                      | _ => raise Stream)
end
