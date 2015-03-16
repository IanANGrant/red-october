(* Lars Thorup and Mads Tofte's completely awe-inspiring 1994 paper
   "Object-Oriented Programming and Standard ML" on F-Bounded
   Polymorphism.

   With a trivial convenince wrapper to wrap up the wrapping and
   unwrapping. Amazingly, this doesn't seem to start an infinite
   regress ... The functology gets a bit hairy at times, but I have
   tried to do it in a regular way, and as Thorup and Tofte point out,
   all the really tricky type stuff is deterministically derivable, so
   the coercions can all be generated as part of type inference.

   In the example below, there are is a Point class, with an
   Attributed Point sub-class, and there ia an RGB Colour class with
   an RGB Alpha sub-class, and these can be the attributes of
   Attributed Points. So we have RGB Attributed Points, RGBAlpha
   Attributed Points and unattributed Points. These sub-classes are
   constructed by a sort of class Template functor, which works across
   the class inheritance structure. Then there are some instance
   methods that move one up and down the inheritance tree.

   Anyone who finds the courage to look at the underlying classes will
   see that I have put in an extra layer, or two. I don't really know
   what I mean by this, but I was thinking it might be possible to get
   the functors and methods to commute, so that one could have a class
   of RGB/RGBAlpha Attributed Points, which could all simultaneously
   be viewed as non-attributed Points, without losing the hidden
   attributes.  My idea, vague as it is, was to use the top-level
   abstype in the unattributed Points, to hide the attributes, but
   still allow them to be switched from RGBAlpha <-> RGB, via the
   Attr.atttr <-> Attr.super bijection. I tried three different
   approaches, but each time I got hopelessly lost. These
   least-fixedpoint type structures look much simpler on paper than
   they are in real life!

   This is an outline of the class instance user's view:

                 Subclass <------------------------> Superclass
                             AttrPoint.to/fromPoint
                AttrPoint <------------------------>  Point
                     |                  ^               ^
                     |                  |               |
       AttrPoint.get | Attr                             |
                     |                 ???              |
                     v                                  |
   Subclass  =  RGBA Colour             |               |
      ^              ^                  v               |
      |              |                 Attr.                      I tried to put
  RGBAColour.to/from | RGB Col <---------------------> ??? <---   something here 
      |              |             to/from Super               but nothing's fitted,
      v              v                  ^               |             so far ...
   Superclass =  RGB Colour             |               |
                     ^                                  |
                     |                 ???              |
       AttrPoint.get | Attr                             |
                     |                  |               |
                     |                  v               v
                 AttrPoint <------------------------> Point
                              AttrPoint.to/fromPoint

   That is the view from _above_ the wrapping/unwrapping
   abstraction. There is another very different view of the workings,
   below it. See the paper for a much better explanation that I could
   hope to give. But here's a sketch of what I've added down there. I
   have elided some of the intermediate stages of the construction.
   The double-arrows are meant to indicate Functor applications:

               Self Wrap     PointClass.to/fromPoint   Self SuperWrap
                    ^ ^      ------------------------>      ^ ^
                    | |      ------------------------>      | |
            AttrPoint.point  AttrPoint.to/fromPoint  AttrPoint.super
                    ^ ^                                     ^ ^
                    | |     <------------------------>      | |
                  ColPoint     ColPoint.to/fromPoint       Point
                     |
    ColPoint.GetAttr |
                     |                         ? Point.GetAttr ? -------+
                     v                                                  |
                RGBAColour                                              |
               : Self Wrap  <---------------------> Attr.attr           |
                     ^                                  ^               .
                     |                                  |               .
       RGBAColour.   |     ------------------------>    | Attr.         .
   to/fromRGBColour  |     ------------------------>    | to/fromSuper  .
                     |                                  |               .
                     v                                  v               .
                 RGBColour  <---------------------> Attr.super          |
            : Self SuperWrap                                            |
                     ^                                                  |
                     |                          ? Point.GetAttr ? ------+
    ColPoint.GetAttr |
                     |
                  ColPoint   ColPoint.to/fromPoint     Point
                    | |    <------------------------>   | |
                    v v                                 v v
          AttrPoint.point    AttrPoint.to/fromPoint  AttrPoint.super
                    | |    ------------------------>    | |
                    v v    ------------------------>    v v
               Self Wrap    PointClass.to/fromPoint  Self SuperWrap

   If we could construct some such "orthonal basis" for a sub-typing
   relation, then it might make class template composition a little
   more tractable, especially if it resulted in a choice of different
   routes, so that we could choose the one of least resistance ...
   There are an awful lot of ways to slice this cake, but only one of
   them is the right way. But even if it's not properly sliced, the
   cake is still vey easy to eat, and delicious too!

   In the above diagram, ColPoint is an alias for this:

       structure ColPoint = AttrPointInstance.AttrPoint

   I've checked the code with polyML, SML/NJ and Moscow ML. The
   results are, superficially, wildly different!

   Mosml prints the types thus:

  {structure AttrPoint :
     {type point = AttrPoint,
      type super = Point,
      type attr = RGBA,
      val Clone : AttrPoint -> unit -> AttrPoint,
      val Equal : AttrPoint -> AttrPoint -> bool,
      val GetAttr : AttrPoint -> unit -> RGBA,
      val Getx : AttrPoint -> unit -> int,
      val Gety : AttrPoint -> unit -> int,
      val Move : AttrPoint -> int * int -> unit,
      val New : int * int * RGBA -> AttrPoint,
      val SetAttr : AttrPoint -> RGBA -> unit,
      val fromPoint : Point -> AttrPoint,
      val toPoint : AttrPoint -> Point}

   SML/NJ does it like this:

    structure AttrPoint :
      sig
        type point = AttrPoint
        val Equal : point -> point -> bool
        val Clone : point -> unit -> point
        val Move : point -> int * int -> unit
        val Getx : point -> unit -> int
        val Gety : point -> unit -> int
        type super = Point
        type attr = Attr
        val New : int * int * attr -> point
        val toPoint : point -> super
        val fromPoint : super -> point
        val GetAttr : point -> unit -> attr
        val SetAttr : point -> attr -> unit
      end

   and PolyML does it like this:

   signature AttrPoint =
     sig
       val Clone : point -> unit -> point
       val Equal : point -> point -> bool
       val GetAttr : point -> unit -> attr
       val Getx : point -> unit -> int
       val Gety : point -> unit -> int
       val Move : point -> int * int -> unit
       val New : int * int * attr -> point
       val SetAttr : point -> attr -> unit
       type attr
       val fromPoint : super -> point
       type point
       type super
       val toPoint : point -> super
     end

   signature AttrPointInstance =
     sig
       type Attr
       structure AttrPoint : AttrPoint
       type AttrPoint
       structure Point : Point
       type Point
     end

   So many different ways of saying the saying the same thing, it's a
   wonder that we know what we're talking about at all. But we do seem
   to know. I suppose it's down to those expensive books I've never
   seen. They are probably rather good.

   So Standard ML has had this great little OOP system for over 21
   years, and no-body even mentions it ... but in the mean time,
   someone invented Python, so that's all right then ... *)

   val _ = load "ColourClasses";
   val _ = load "PointClasses";

local
   structure RGBColourClass =
      RGBColourClassInstance()

   structure RGBAlphaColourClass =
      RGBAlphaColourClassInstance
        (structure SuperClass = RGBColourClass)

   structure ColourAttr =
      RGBAlphaColourAttr
        (structure RGBColourClass = RGBColourClass
         structure RGBAlphaColourClass = RGBAlphaColourClass)

  (* We could probably apply the first of the above functors inside
     the second, and the first two inside the third. See Bruno's story
     of the Three Little Foxes in Lewis Carroll's "Sylvie and Bruno
     Vol II" They ate their _selves!_ *)

   structure PointClass =
      PointClassInstance()

   structure RGBAAttrPointClass =
      AttrPointClassInstance
         (structure Attr = ColourAttr.RGBAAttr
          structure SuperClass = PointClass)

   structure RGBAAttrPointInstance =
     AttrPointInstance
        (structure Attr = ColourAttr.RGBAAttr
         structure PointClass = PointClass
         structure AttrPointClass = RGBAAttrPointClass)

   (* The same comment applies here too. *)

   structure RGBAttrPointClass =
      AttrPointClassInstance
         (structure Attr = ColourAttr.RGBAttr
          structure SuperClass = PointClass)

   structure RGBAttrPointInstance =
     AttrPointInstance
        (structure Attr = ColourAttr.RGBAttr
         structure PointClass = PointClass
         structure AttrPointClass = RGBAttrPointClass)

in
   structure ColourAttr = ColourAttr
   structure RGBAAttrPointInstance = RGBAAttrPointInstance
   structure RGBAttrPointInstance = RGBAttrPointInstance
end

local
   structure RGBA = ColourAttr.RGBAlphaColour
   structure RGB = ColourAttr.RGBColour
   structure ColPoint = RGBAAttrPointInstance.AttrPoint
   structure Point = RGBAAttrPointInstance.Point
   structure RGBColPoint = RGBAttrPointInstance.AttrPoint
   structure Point' = RGBAttrPointInstance.Point
in
   val mud = RGBA.New (1.0, 1.0, 0.0, 1.0)
   val mud' = RGB.New (1.0, 1.0, 0.0)

   val c2 = RGBA.toRGBColour mud

   val true = RGB.Equal mud' c2

   val mud'r = RGBA.Getr mud ()
   val mud'g = RGBA.Getg mud ()
   val mud'b = RGBA.Getb mud ()
   val mud'a = RGBA.Geta mud ()

   val () = RGB.Setb c2 1.0

   val white'r = RGBA.Getr mud ()
   val white'g = RGBA.Getg mud ()
   val white'b = RGBA.Getb mud ()
   val white'a = RGBA.Geta mud ()

   val mud'' = RGBA.fromRGBColour mud'

   val mud''r = RGBA.Getr mud'' ()
   val mud''g = RGBA.Getg mud'' ()
   val mud''b = RGBA.Getb mud'' ()
   val mud''a = RGBA.Geta mud'' ()

   val () = RGBA.Setb mud'' 1.0

   val true = RGBA.Equal mud'' mud

   val rp1 = ColPoint.New (1,2,mud)
   val 1 = ColPoint.Getx rp1 ()
   val 2 = ColPoint.Gety rp1 ()

   val c1 = ColPoint.GetAttr rp1 ()
   val true = RGBA.Equal c1 mud

   val p1 = ColPoint.toPoint rp1
   val 1 = Point.Getx p1 ()
   val 2 = Point.Gety p1 ()

   (* p2 is a new object with the same instance
      variables and instance methods as p1 *)
   val p2 = Point.Clone p1 ()

   val 1 = Point.Getx p1 ()
   val 2 = Point.Gety p1 ()

   (* Equality tests the _values_ of instance 
      variables, not their identity *)
   val true = Point.Equal p1 p2

   val p4 = Point.New (1,2)

   val true = Point.Equal p1 p4

   (* Default colour is white *)
   val rp4 = ColPoint.fromPoint p4

   (* rp1 was created with mud colour, but we
      changed the mud to white. *)
   val true = RGBA.Equal
               (ColPoint.GetAttr rp1 ())
               (ColPoint.GetAttr rp4 ())

   val true = ColPoint.Equal rp1 rp4

   val purple = RGBA.New (1.0, 0.0, 1.0, 1.0)

   val () = ColPoint.SetAttr rp1 purple

   (* Although rp1 and and rp3 have different colours, 
     they are equal, when compared as _points._ *)

   val false = ColPoint.Equal rp1 rp4
   val true = Point.Equal p1 p4

   val p3 = ColPoint.toPoint rp1

   val 1 = Point.Getx p3 ()
   val 2 = Point.Gety p3 ()

   val true = Point.Equal p1 p2

   val () = ColPoint.Move rp1 (1,1)

   (* p2 is a "colourless" _clone_ of rp1, so it 
      moves independently of rp1 *)
   val false = Point.Equal p1 p2

   (* p3 is the "colourless" rp1, so it moves with rp1 *)
   val 2 = Point.Getx p3 ()
   val 3 = Point.Gety p3 ()

   val true = Point.Equal (ColPoint.toPoint rp1) p3
end

