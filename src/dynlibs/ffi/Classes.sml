val _ = load "ColourClasses";

signature ClassGetters =
sig
   type IVs
   val ivs : { ivs : IVs } -> { ivs : IVs }
end

signature ClassSetters =
sig
   type IVs
   val ivs : { ivs : IVs } -> { ivs : IVs } -> unit
end

signature ClassMethods =
sig
   type IVs
   type initargs
   structure Get : ClassGetters
       where type IVs = IVs
   structure Set : ClassSetters
       where type IVs = IVs
   val init : initargs -> { ivs : IVs }
   val tini : { ivs : IVs } -> initargs
   val equal : { ivs : IVs } -> { ivs : IVs } -> bool
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
      getivs : unit -> { ivs : IVs },
      setivs : { ivs : IVs } -> unit}
   datatype Self = SELF of Self F
   datatype 'a Wrap = WRAP of ('a Wrap) F * 'a
   type Vars
   val wrap_ :
      ('a -> unit -> {ivs : IVs}) ->
      ('a -> {ivs : IVs} -> unit) ->
      ('a -> 'a -> bool) ->
      ('a -> unit -> 'a) -> 'a -> 'a Wrap
   val wrap : Self -> Self Wrap
   val unwrap : 'a Wrap -> 'a
   val view : 'a Wrap -> 'a Wrap F
   val init : initargs -> Vars
   val new : initargs -> Self
   val getivs : Vars -> unit -> { ivs : IVs }
   val setivs : Vars -> { ivs : IVs } -> unit
   val clone : Vars * (initargs -> 'a Wrap) -> unit -> 'a
   val equal : 'a Wrap -> 'b Wrap -> bool
   structure InstanceMethods :
   sig
      val New : initargs -> Self Wrap
      val Clone : 'a Wrap -> unit -> 'a Wrap
      val Equal : 'a Wrap -> 'b Wrap -> bool
      val GetIVs : 'a Wrap -> unit -> { ivs : IVs }
      val SetIVs : 'a Wrap -> { ivs : IVs } -> unit
   end
end

signature AttrClassPrivate =
sig
   type SuperIVs
   structure Super : ClassPrivate
         where type IVs = SuperIVs
   type IVs
   type initargs
   structure ClassMethods : ClassMethods
      where type IVs = IVs
        and type initargs = initargs
   structure Attr : Attr
   datatype attr = Attr of Attr.attr
                 | Supr of Attr.super
   type 'a F = { getattr : unit -> attr,
                 setattr : attr -> unit,
                 getx : unit -> int,
                 gety : unit -> int,
                 move : int * int -> unit,
                 equal : 'a -> bool,
                 clone : unit -> 'a}
   datatype Self = SELF of Self F
   datatype 'a Wrap = WRAP of 'a Wrap F * 'a 
   type Vars = {attr : attr ref, sup : Super.Vars}
   val New : int * int * attr -> Self Wrap 
   val Clone : 'a Wrap -> unit -> 'a Wrap
   val Equal : 'a Wrap -> 'b Wrap -> bool
   val toPoint :  Self Wrap -> Self Super.Wrap
   val fromPoint : Self Super.Wrap -> Self Wrap
   val Getx : 'a Wrap -> unit -> int
   val Gety : 'a Wrap -> unit -> int
   val Move : 'a Wrap -> int * int -> unit
   val GetAttr : 'a Wrap -> unit -> attr
   val SetAttr : 'a Wrap -> attr -> unit
   val equal : 'a Wrap -> 'b Wrap -> bool
   val init : int * int * attr -> Vars
   val new : int * int * attr -> Self
   val getattr : Vars -> unit -> attr
   val setattr : Vars -> attr -> unit
   val wrap : Self -> Self Wrap
   val unwrap : 'a Wrap -> 'a
   val view : 'a Wrap -> 'a Wrap F
   val wrapSuper : Self -> Self Super.Wrap
   val SuperWrapFromWrap : 'a Wrap -> 'a Wrap Super.Wrap
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
   type 'a F = { getivs : unit -> { ivs : IVs},
                 setivs : { ivs : IVs} -> unit,
                 equal : 'a -> bool,
                 clone : unit -> 'a}
   datatype Self = SELF of Self F
   datatype 'a Wrap = WRAP of ('a Wrap) F * 'a
   fun unwrap (WRAP(_,obj)) = obj
   fun view (WRAP(r,_)) = r
   fun wrap_ getivsf setivsf equalf clonef =
     fn p =>
      let val wrapf = wrap_ getivsf setivsf equalf clonef
      in
            WRAP ({ getivs = (getivsf p), 
                    setivs = (setivsf p), 
                    equal = (equalf p) o unwrap, 
                    clone = wrapf o (clonef p)},p)
      end
   val wrap =
      let fun getf sel = fn (SELF pr) => (sel pr)
      in fn p => wrap_ (getf #getivs) (getf #setivs) (getf #equal) (getf #clone) p
      end          
   abstype Vars = VARS of { ivs : IVs }
   with
      fun init iargs = VARS (ClassMethods.init iargs)
      fun getivs (VARS iv)() = ClassMethods.Get.ivs iv
      fun setivs (VARS iv) (ivs) = ClassMethods.Set.ivs iv ivs
      fun equal (self) (p) =
            ClassMethods.equal
               (#getivs (view self) ())
                (#getivs (view p) ())
      fun clone (VARS iv,selfclass)() =
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
      val New : initargs -> Self Wrap = wrap o new
      val Equal : 'a Wrap -> 'b Wrap -> bool = equal
      val Clone = fn p => #clone (view p)
      val GetIVs = fn p => #getivs (view p)
      val SetIVs = fn p => #setivs (view p)
   end
end

signature PointClassGetters =
sig
   include ClassGetters
   type CoOrd
   val x : { ivs : IVs } -> unit -> CoOrd
   val y : { ivs : IVs } -> unit -> CoOrd
end

signature PointClassSetters =
sig
   include ClassSetters
   type CoOrd
   val move : { ivs : IVs } -> CoOrd * CoOrd -> unit
end

signature PointClassMethods =
sig
   type IVs
   type initargs
   type CoOrd
   structure Get : PointClassGetters
       where type IVs = IVs
         and type CoOrd = CoOrd
   structure Set : PointClassSetters
       where type IVs = IVs
         and type CoOrd = CoOrd
   val init : initargs -> { ivs : IVs }
   val tini : { ivs : IVs } -> initargs
   val equal : { ivs : IVs } -> { ivs : IVs } -> bool
end

structure PointClassGetters : PointClassGetters =
struct
   type IVs = { x : int ref, y : int ref } ref
   type CoOrd = int
   val ivs : { ivs : IVs } -> { ivs : IVs }
         = fn ivs  => ivs
   val x : { ivs : IVs } -> unit -> CoOrd
         = fn ivs => fn () => !(#x (!(#ivs ivs)))
   val y : { ivs : IVs } -> unit -> CoOrd
         = fn ivs => fn () => !(#y (!(#ivs ivs)))
end

structure PointClassSetters : PointClassSetters =
struct
   type IVs = { x : int ref, y : int ref } ref
   type CoOrd = int
   val ivs : { ivs : IVs } -> { ivs : IVs } -> unit
         = fn ivs => fn ivs' => (#ivs ivs) := (!(#ivs ivs'))
   val move : { ivs : IVs } -> CoOrd * CoOrd -> unit
         = fn ivs => fn (dx,dy) =>
               let val r = !(#ivs ivs)
                   val (x,y) = (!(#x r),!(#y r))
               in ((#x r) := x + dx;(#y r) := y + dy)
               end 
end

structure PointClassMethods =
struct
   type IVs = { x : int ref, y : int ref } ref
   type CoOrd = int
   type initargs = int * int
   structure Get : PointClassGetters
       where type IVs = IVs
         and type CoOrd = CoOrd = PointClassGetters
   structure Set : PointClassSetters
       where type IVs = IVs
         and type CoOrd = CoOrd = PointClassSetters
   val init : initargs -> { ivs : IVs }
        = fn (x,y) => { ivs = ref {x = ref x,y = ref y} }
   val tini : { ivs : IVs } -> initargs
        = fn { ivs = ref {x = ref x,y = ref y} } => (x,y)
   val equal : { ivs : IVs } -> { ivs : IVs } -> bool
        = fn { ivs = ref {x = ref x,y = ref y} } =>
          fn { ivs = ref {x = ref x',y = ref y'} } =>
             x = x' andalso y = y'
end

structure PointClass =
  ClassInstance
    (structure ClassMethods : ClassMethods = PointClassMethods);

local open PointClass.InstanceMethods
in
   val p1 = New (1,2);
   val p2 = Clone p1 ();
   val p3 = New (2,1);
   val true = Equal p1 p2;
   val true = Equal p2 p2;
   val true = Equal p1 p2;
   val false = Equal p2 p3;
   val ivs1 = GetIVs p1 ();
   val ivs2 = GetIVs p2 ();
end

(*
functor AttrClassInstance
  (structure SuperClass : ClassPrivate
   structure Attr : Attr)
    :> AttrClassPrivate
        where type SuperIVs = SuperClass.IVs
          and type IVs = ClassMethods.IVs
          and type initargs = ClassMethods.initargs
          and type 'a Super.Wrap = 'a SuperClass.Wrap
          and type Attr.attr = Attr.attr
          and type Attr.super = Attr.super =
struct
   type SuperIVs = SuperClass.IVs
   structure Super : ClassPrivate
         where type IVs = SuperIVs = SuperClass
   type IVs = ClassMethods.IVs
   type initargs = ClassMethods.initargs
   structure ClassMethods : ClassMethods
      where type IVs = IVs
        and type initargs = initargs = ClassMethods
   structure Super
         :> ClassPrivate
              where type IVs = SuperIVs = SuperClass 
   structure Attr = Attr
   datatype attr = Attr of Attr.attr
                 | Supr of Attr.super
   val eq : attr * attr -> bool =
        fn (Attr a, Attr a') => Attr.eq (a, a')
         | (Supr s, Attr a) => Attr.eq (Attr.fromSuper s, a)
         | (Attr a, Supr s) => Attr.eq (a, Attr.fromSuper s)
         | (Supr s, Supr s') => Attr.eq (Attr.fromSuper s, Attr.fromSuper s')
   type Vars = {attr : attr ref, sup : Super.Vars}
   type 'a F = { getattr : unit -> attr,
                 setattr : attr -> unit,
                 getx : unit -> int,
                 gety : unit -> int,
                 move : int * int -> unit,
                 equal : 'a -> bool,
                 clone : unit -> 'a}
   datatype Self = SELF of Self F
   datatype 'a Wrap = WRAP of 'a Wrap F * 'a
   fun unwrap (WRAP(_,obj)) = obj
   fun view (WRAP(r,_)) = r
   fun wrap (cp as SELF cpr) =
         WRAP ({ getattr = (#getattr cpr),
                 setattr = (#setattr cpr),
                 getx = (#getx cpr),
                 gety = (#gety cpr), 
                 move = (#move cpr), 
                 equal = (#equal cpr) o unwrap, 
                 clone = wrap o (#clone cpr)},cp)
   fun wrapSuper p =
      let fun getf sel = fn (SELF cpr) => (sel cpr)
      in Super.wrap_ (getf #getivs) (getf #setivs) (getf #equal) (getf #clone) p
      end          
   fun SuperWrapFromWrap (wcp) =
      let fun getf sel = fn wcp => (sel (view wcp))
      in Super.wrap_ (getf #getivs) (getf #setivs) (getf #equal) (getf #clone) p
      end          
(*   fun wrapSuper (cp as SELF cpr) =
            Super.WRAP ({ getx = (#getx cpr),
                          gety = (#gety cpr), 
                          move = (#move cpr), 
                          equal = (#equal cpr) o Super.unwrap, 
                          clone = wrapSuper o (#clone cpr)},cp) 
   fun SuperWrapFromWrap (wcp) =
            Super.WRAP ({ getx = (#getx (view wcp)),
                          gety = (#gety (view wcp)), 
                          move = (#move (view wcp)), 
                          equal = (#equal (view wcp)) o Super.unwrap, 
                          clone = SuperWrapFromWrap o (#clone (view wcp))}, wcp) *)
   fun init (x,y,a : attr) = {attr = ref a, sup = Super.init (x,y)}
   fun getattr (iv : Vars)() = !(#attr iv)
   fun setattr (iv : Vars)(a) = (#attr iv) := a
   fun equal (self)(p) = Super.equal (SuperWrapFromWrap self) (SuperWrapFromWrap p)
        andalso eq (#getattr (view self) (), #getattr (view p) ())
   fun new (x,y,a : attr) =
      let val iv = init (x,y,a)
          fun mkSuperNew a = fn (x,y) => new(x,y,a)
          val superNew = mkSuperNew (getattr(iv)())
          fun self () = SELF {
                         getattr = getattr(iv),
                         setattr = setattr(iv),
                         getx = Super.getx(#sup iv),
                         gety = Super.gety(#sup iv),
                         move = Super.move(#sup iv),
                         equal = fn p => equal (wrap(self())) (wrap p),
                         clone = Super.clone (#sup iv, wrapSuper o superNew)}
      in self ()
      end
   type 'a SuperWrap = 'a SuperClass.Wrap
   val New = wrap o new
   val Equal = equal
   val Clone = fn p => #clone (view p)
   val toPoint = wrapSuper o unwrap
   val fromPoint = wrap o Super.unwrap
   val Move = fn p => #move (view p)
   val Getx = fn p => #getx (view p)
   val Gety = fn p => #gety (view p)
   val GetAttr = fn p => #getattr (view p)
   val SetAttr = fn p => #setattr (view p)
end
*)
