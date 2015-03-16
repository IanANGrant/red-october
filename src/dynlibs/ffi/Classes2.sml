val _ = load "ColourClasses";

signature ClassMethods =
sig
   type IVs
   type initargs
   val init : initargs -> IVs
   val tini : IVs -> initargs
   val equal : IVs -> IVs -> bool
end

signature SubClassMethods =
sig
   include ClassMethods
   type superinitargs
   val SuperInitArgs : initargs -> superinitargs
   val mkSuperNew : IVs -> (initargs -> 'b) -> superinitargs -> 'b
end

signature ClassInstanceMethods =
sig
   type IVs
   type Self
   type 'a Wrap
   type initargs
   val New : initargs -> Self Wrap
   val Clone : 'a Wrap -> unit -> 'a Wrap
   val Equal : 'a Wrap -> 'b Wrap -> bool
   val wrapMethod : (IVs -> 'a -> 'b) -> 'c Wrap -> 'a -> 'b
end

signature ClassPrivate =
sig
   type IVs
   type initargs
   structure ClassMethods : ClassMethods
      where type IVs = IVs
        and type initargs = initargs
   type 'a F =
     {equal : 'a -> bool,
      clone : unit -> 'a,
      getivs : unit -> IVs,
      setivs : IVs -> unit}
   datatype Self = SELF of Self F
   datatype 'a Wrap = WRAP of ('a Wrap) F * 'a
   type Vars
   val wrap_ :
      ('a -> unit -> IVs) ->
      ('a -> IVs -> unit) ->
      ('a -> 'a -> bool) ->
      ('a -> unit -> 'a) -> 'a -> 'a Wrap
   val wrap : Self -> Self Wrap
   val unwrap : 'a Wrap -> 'a
   val view : 'a Wrap -> 'a Wrap F
   val init : initargs -> Vars
   val new : initargs -> Self
   val getivs : Vars -> unit -> IVs
   val setivs : Vars -> IVs -> unit
   val clone : Vars * (initargs -> 'a Wrap) -> unit -> 'a
   val equal : 'a Wrap -> 'b Wrap -> bool
   structure InstanceMethods : ClassInstanceMethods
       where type IVs = IVs
         and type Self = Self
         and type 'a Wrap = 'a Wrap
         and type initargs = initargs
end

functor ClassInstance
  (structure ClassMethods : ClassMethods)
  :> ClassPrivate
       where type IVs = ClassMethods.IVs
         and type initargs = ClassMethods.initargs =
struct
   type IVs = ClassMethods.IVs
   type initargs = ClassMethods.initargs
   structure ClassMethods = ClassMethods
   type 'a F = { getivs : unit -> IVs,
                 setivs : IVs -> unit,
                 equal : 'a -> bool,
                 clone : unit -> 'a}
   datatype Self = SELF of Self F
   datatype 'a Wrap = WRAP of ('a Wrap) F * 'a
   fun unwrap (WRAP(_,obj)) = obj
   fun view (WRAP(r,_)) = r
   fun wrap_ getivsf setivsf equalf clonef =
     fn p =>
      let val wrapf = wrap_ getivsf setivsf equalf clonef
      in WRAP ({ getivs = (getivsf p), 
                 setivs = (setivsf p), 
                 equal = (equalf p) o unwrap, 
                 clone = wrapf o (clonef p)},p)
      end
   val wrap =
      let fun getf sel = fn (SELF pr) => (sel pr)
      in fn p => wrap_ (getf #getivs) (getf #setivs) (getf #equal) (getf #clone) p
      end          
   abstype Vars = VARS of { ivs : IVs ref }
   with
      fun init iargs = VARS {ivs = (ref (ClassMethods.init iargs))}
      fun getivs (VARS ({ivs = ref iv}))() = iv
      fun setivs (VARS ({ivs = ivr})) (ivs) = ivr := ivs
      fun equal (self) (p) =
            ClassMethods.equal
               (#getivs (view self) ())
                (#getivs (view p) ())
      fun clone (VARS {ivs = ref iv},selfclass)() =
                     (unwrap o selfclass)
                     (ClassMethods.tini iv)
   end
   fun new (iargs : initargs) =
      let val iv : Vars = init (iargs)
          fun self () = 
                       SELF {
                         getivs = getivs(iv),
                         setivs = setivs(iv),
                         equal = fn (p) => equal (wrap(self())) (wrap p),
                         clone = clone (iv,wrap o new)}
      in self ()
      end
   structure InstanceMethods =
   struct
      type IVs = IVs
      type Self = Self
      type 'a Wrap = 'a Wrap
      type initargs = initargs
      val New : initargs -> Self Wrap = wrap o new
      val Equal : 'a Wrap -> 'b Wrap -> bool = equal
      val Clone = fn p => #clone (view p)
      fun wrapMethod f =
         fn p => fn x => f (#getivs (view p) ()) x
   end
end

signature SubClassInstanceMethods =
sig
   type IVs
   type SubIVs
   type Self
   type 'a Wrap
   type 'a SuperWrap
   type initargs
   val New : initargs -> Self Wrap
   val Clone : 'a Wrap -> unit -> 'a Wrap
   val Equal : 'a Wrap -> 'b Wrap -> bool
   val toSuper :  Self Wrap -> Self SuperWrap
   val fromSuper : Self SuperWrap -> Self Wrap
   val wrapSubClassMethod : (SubIVs -> 'a -> 'b) -> 'c Wrap -> 'a -> 'b
   val wrapMethod : (IVs -> 'a -> 'b) -> 'c Wrap -> 'a -> 'b
end

signature SubClassPrivate =
sig
   type SuperIVs
   type superinitargs
   structure Super : ClassPrivate
      where type initargs = superinitargs
   type IVs
   type initargs
   structure ClassMethods : SubClassMethods
   type 'a F = { getattr : unit -> IVs,
                 setattr : IVs -> unit,
                 getivs : unit -> SuperIVs,
                 setivs : SuperIVs -> unit,
                 equal : 'a -> bool,
                 clone : unit -> 'a}
   datatype Self = SELF of Self F
   datatype 'a Wrap = WRAP of 'a Wrap F * 'a 
   type Vars = {attr : IVs ref, sup : Super.Vars}
   val equal : 'a Wrap -> 'b Wrap -> bool
   val init : initargs -> Vars
   val new : initargs -> Self
   val getattr : Vars -> unit -> IVs
   val setattr : Vars -> IVs -> unit
   val wrap : Self -> Self Wrap
   val unwrap : 'a Wrap -> 'a
   val view : 'a Wrap -> 'a Wrap F
   val wrapSuper : Self -> Self Super.Wrap
   val SuperWrapFromWrap : 'a Wrap -> 'a Wrap Super.Wrap
   structure InstanceMethods : SubClassInstanceMethods
       where type IVs = SuperIVs
         and type initargs = initargs
         and type Self = Self
         and type 'a Wrap = 'a Wrap
end

functor SubClassInstance
  (structure SuperClass : ClassPrivate
   structure ClassMethods : SubClassMethods
       sharing type ClassMethods.superinitargs = SuperClass.initargs)
  :> SubClassPrivate
        where type SuperIVs = SuperClass.IVs
          and type IVs = ClassMethods.IVs
          and type InstanceMethods.SubIVs = ClassMethods.IVs
          and type initargs = ClassMethods.initargs =
struct
   type IVs = ClassMethods.IVs
   type initargs = ClassMethods.initargs
   structure ClassMethods : SubClassMethods = ClassMethods
   structure Super : ClassPrivate = SuperClass
   type SuperIVs = SuperClass.IVs
   type superinitargs = SuperClass.initargs
   type Vars = {attr : IVs ref, sup : Super.Vars}
   type 'a F = { getattr : unit -> IVs,
                 setattr : IVs -> unit,
                 getivs : unit -> SuperIVs,
                 setivs : SuperIVs -> unit,
                 equal : 'a -> bool,
                 clone : unit -> 'a}
   datatype Self = SELF of Self F
   datatype 'a Wrap = WRAP of 'a Wrap F * 'a
   fun unwrap (WRAP(_,obj)) = obj
   fun view (WRAP(r,_)) = r
   fun wrap (cp as SELF cpr) =
         WRAP ({ getattr = (#getattr cpr),
                 setattr = (#setattr cpr),
                 getivs = (#getivs cpr),
                 setivs = (#setivs cpr),
                 equal = (#equal cpr) o unwrap, 
                 clone = wrap o (#clone cpr)},cp)
   fun wrapSuper (cp as SELF cpr) =
            Super.WRAP ({ getivs = (#getivs cpr),
                          setivs = (#setivs cpr), 
                          equal = (#equal cpr) o Super.unwrap, 
                          clone = wrapSuper o (#clone cpr)},cp)
   fun SuperWrapFromWrap (wcp) =
            Super.WRAP ({ getivs = (#getivs (view wcp)),
                          setivs = (#setivs (view wcp)), 
                          equal = (#equal (view wcp)) o Super.unwrap, 
                          clone = SuperWrapFromWrap o (#clone (view wcp))}, wcp)
   fun init iargs = {attr = (ref (ClassMethods.init iargs)),
                     sup = Super.init (ClassMethods.SuperInitArgs iargs)}
   fun getattr (iv : Vars)() = !(#attr iv)
   fun setattr (iv : Vars)(a) = (#attr iv) := a
   fun equal (self)(p) = Super.equal (SuperWrapFromWrap self) (SuperWrapFromWrap p)
        andalso ClassMethods.equal (#getattr (view self) ()) (#getattr (view p) ())
   fun new iargs =
      let val iv = init iargs
          val superNew = ClassMethods.mkSuperNew (!(#attr iv)) new
          fun self () = SELF {
                         getattr = getattr(iv),
                         setattr = setattr(iv),
                         getivs = Super.getivs(#sup iv),
                         setivs = Super.setivs(#sup iv),
                         equal = fn p => equal (wrap(self())) (wrap p),
                         clone = Super.clone (#sup iv, wrapSuper o superNew)}
      in self ()
      end
   structure InstanceMethods =
   struct
      type SubIVs = IVs 
      type IVs = SuperIVs
      type initargs = initargs
      type Self = Self
      type 'a Wrap = 'a Wrap
      type 'a SuperWrap = 'a Super.Wrap
      val New = wrap o new
      val Equal = equal
      val Clone = fn p => #clone (view p)
      val toSuper = wrapSuper o unwrap
      val fromSuper = wrap o Super.unwrap
      fun wrapSubClassMethod f =
         fn p => fn x => f (#getattr (view p) ()) x
      fun wrapMethod f =
         fn p => fn x => f (#getivs (view p) ()) x
   end
end

