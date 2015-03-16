signature PointClassPrivate =
sig
   type 'a F =
     {equal : 'a -> bool,
      clone : unit -> 'a,
      getx : unit -> int,
      gety : unit -> int,
      move : int * int -> unit}
   datatype Self = SELF of Self F
   datatype 'a Wrap = WRAP of ('a Wrap) F * 'a
   type Vars
   val New : int * int -> Self Wrap
   val Clone : 'a Wrap -> unit -> 'a Wrap
   val Equal : 'a Wrap -> 'b Wrap -> bool
   val Getx : 'a Wrap -> unit -> int
   val Gety : 'a Wrap -> unit -> int
   val Move : 'a Wrap -> int * int -> unit
   val init : int * int -> Vars
   val new : int * int -> Self
   val clone : Vars * (int * int -> 'a Wrap) -> unit -> 'a
   val equal : 'a Wrap -> 'b Wrap -> bool
   val getx : Vars -> unit -> int
   val gety : Vars -> unit -> int
   val move : Vars -> int * int -> unit
   val wrap : Self -> Self Wrap
   val unwrap : 'a Wrap -> 'a
   val view : 'a Wrap -> 'a Wrap F
end

signature AttrPointClassPrivate =
sig
   structure Super : PointClassPrivate
   structure Attr : Attr
   type attr = Attr.attr
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

functor PointClassInstance()
  :> PointClassPrivate =
struct
   type 'a F = { getx : unit -> int,
                 gety : unit -> int,
                 move : int * int -> unit,
                 equal : 'a -> bool,
                 clone : unit -> 'a}
   datatype Self = SELF of Self F
   datatype 'a Wrap = WRAP of ('a Wrap) F * 'a
   fun unwrap (WRAP(_,obj)) = obj
   fun view (WRAP(r,_)) = r
   fun wrap (p as SELF pr) : Self Wrap =
         WRAP ({ getx = (#getx pr), 
                 gety = (#gety pr), 
                 move = (#move pr), 
                 equal = (#equal pr) o unwrap, 
                 clone = wrap o (#clone pr)},p)
   abstype Vars = VARS of {x : int ref, y : int ref}
   with
      fun init (x,y) = VARS {x = ref x, y = ref y}
      fun getx (VARS iv)() = !(#x iv)
      fun gety (VARS iv)() = !(#y iv)
      fun move (VARS iv) (dx,dy) = (
                   (#x iv) := !(#x iv) + dx;
                   (#y iv) := !(#y iv) + dy)
      fun equal (self) (p) = (#getx (view self))() = (#getx (view p))()
                     andalso (#gety (view self))() = (#gety (view p))()
      fun clone (VARS iv,selfclass)() = (unwrap o selfclass) (!(#x iv),!(#y iv))
   end
   fun new (x,y) =
      let val iv : Vars = init(x,y)
          fun self () = SELF {
                         getx = getx(iv),
                         gety = gety(iv),
                         move = move(iv),
                         equal = fn (p) => equal (wrap(self())) (wrap p),
                         clone = clone (iv,wrap o new)}
      in self ()
      end
   val Equal : 'a Wrap -> 'b Wrap -> bool = equal
   val New : int * int -> Self Wrap = wrap o new
   val Clone = fn p => #clone (view p)
   val Move = fn p => #move (view p)
   val Getx = fn p => #getx (view p)
   val Gety = fn p => #gety (view p)
end

functor AttrPointClassInstance
  (structure SuperClass : PointClassPrivate
   structure Attr : Attr)
    :> AttrPointClassPrivate
        where type 'a Super.Wrap = 'a SuperClass.Wrap
          and type Attr.attr = Attr.attr
          and type Attr.super = Attr.super =
struct
   structure Super = SuperClass
   structure Attr = Attr
   type attr = Attr.attr
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
   fun wrapSuper (cp as SELF cpr) =
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
                          clone = SuperWrapFromWrap o (#clone (view wcp))}, wcp)
   fun init (x,y,a : attr) = {attr = ref a, sup = Super.init (x,y)}
   fun getattr (iv : Vars)() = !(#attr iv)
   fun setattr (iv : Vars)(a) = (#attr iv) := a
   fun equal (self)(p) = Super.equal (SuperWrapFromWrap self) (SuperWrapFromWrap p)
        andalso Attr.eq (#getattr (view self) (), #getattr (view p) ())
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

signature AbstractPointClass =
sig
   type Self
   type 'a Wrap
   val Equal : 'a Wrap -> 'b Wrap -> bool
   val Clone : 'a Wrap -> unit -> 'a Wrap
   val Move : 'a Wrap -> int * int -> unit
   val Getx : 'a Wrap -> unit -> int
   val Gety : 'a Wrap -> unit -> int
end

signature PointClass =
sig
   include AbstractPointClass
   val New : int * int -> Self Wrap
end

signature AttrPointClass =
sig
   include AbstractPointClass
   structure Point : PointClass
   structure Attr : Attr
   type 'a SuperWrap = 'a Point.Wrap
   val New : int * int * Attr.attr -> Self Wrap
   val toPoint : Self Wrap -> Self SuperWrap
   val fromPoint : Self SuperWrap -> Self Wrap
   val GetAttr : 'a Wrap -> unit -> Attr.attr
   val SetAttr : 'a Wrap -> Attr.attr -> unit
end

functor AttrPoint
   (structure Attr : Attr
    structure PointClass : PointClassPrivate
    structure AttrPointClass : AttrPointClassPrivate
       where type Attr.attr = Attr.attr
       sharing type AttrPointClass.Super.Wrap = PointClass.Wrap)
   :> AttrPointClass
       where type Attr.attr = Attr.attr =
struct
   open AttrPointClass
   type 'a SuperWrap = 'a AttrPointClass.Super.Wrap
   type Self = AttrPointClass.Self
   type 'a Wrap = 'a AttrPointClass.Wrap
   structure Attr :> Attr 
      where type attr = Attr.attr = Attr
   structure Point :> PointClass
      where type 'a Wrap = 'a SuperWrap =
      struct
         open PointClass
      end
end

signature AbstractPoint =
sig
   type point
   val Equal : point -> point -> bool
   val Clone : point -> unit -> point
   val Move : point -> int * int -> unit
   val Getx : point -> unit -> int
   val Gety : point -> unit -> int
end

signature Point =
sig
   include AbstractPoint
   val New : int * int -> point
end

signature AttrPoint =
sig
   include AbstractPoint
   type super
   type attr
   val New : int * int * attr -> point
   val toPoint : point -> super
   val fromPoint : super -> point
   val GetAttr : point -> unit -> attr
   val SetAttr : point -> attr -> unit
end

signature AttrPointInstance =
sig
    type Attr
    type AttrPoint
    type Point
    structure Point : Point
       where type point = Point
    structure AttrPoint : AttrPoint
       where type point = AttrPoint
         and type attr = Attr
         and type super = Point
end

functor
  AttrPointInstance
     (structure Attr : Attr
      structure PointClass : PointClassPrivate
      structure AttrPointClass : AttrPointClassPrivate
         where type Attr.attr = Attr.attr
         sharing type AttrPointClass.Super.Wrap = PointClass.Wrap)
     :> AttrPointInstance
         where type Attr = Attr.attr =
struct
   local
      structure AttrPoint1 =
         AttrPoint
           (structure Attr = Attr
            structure PointClass = PointClass
            structure AttrPointClass = AttrPointClass)
              :> AttrPointClass
                  where type Attr.attr = Attr.attr
   in
      type Attr = Attr.attr
      type AttrPoint = AttrPoint1.Self AttrPoint1.Wrap 
      type Point = AttrPoint1.Self AttrPoint1.Point.Wrap
      structure AttrPoint :> AttrPoint
         where type point = AttrPoint
           and type attr = Attr
           and type super = Point =
      struct
         type point = AttrPoint
         type super = Point
         type attr = Attr
         val New : int * int * attr -> point = AttrPoint1.New
         val toPoint : point -> super =  AttrPoint1.toPoint
         val fromPoint : super -> point =  AttrPoint1.fromPoint
         val GetAttr : point -> unit -> attr = AttrPoint1.GetAttr
         val SetAttr : point -> attr -> unit = AttrPoint1.SetAttr
         val Equal : point -> point -> bool = AttrPoint1.Equal
         val Clone : point -> unit -> point = AttrPoint1.Clone
         val Move : point -> int * int -> unit = AttrPoint1.Move
         val Getx : point -> unit -> int = AttrPoint1.Getx
         val Gety : point -> unit -> int = AttrPoint1.Gety
      end
      structure Point :> Point
         where type point = Point =
      struct
         type point = Point
         local open AttrPoint1.Point
         in val New : int * int -> point =
                  fn (x,y) => AttrPoint1.toPoint (AttrPoint1.New (x,y,Attr.default))
            val Equal : point -> point -> bool = Equal
            val Clone : point -> unit -> point = Clone
            val Move : point -> int * int -> unit = Move
            val Getx : point -> unit -> int = Getx
            val Gety : point -> unit -> int = Gety
         end
      end
   end
end
