(* Lars Thorup and Mads Tofte's completely awe-inspiring 1994 paper
   "Object-Oriented Programming and Standard ML" on F-Bounded
   Polymorphism.

   With a trivial convenince wrapper to wrap up the wrapping and
   unwrapping. Amazingly, this doesn't seem to start an infinite
   regress ... 

   So Standard ML has had this great little OO system for over 21
   years, and no-body even mentions it ... but in the mean time,
   someone invented Python, so that's all right then ... *)

val _ = load "ColourClasses";
val _ = load "PointClasses";

(* Moscow ML prints the types of the following satructures quite
   nicely. For example:

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
*)

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

   structure PointClass =
      PointClassInstance()

   structure AttrPointClass =
      AttrPointClassInstance
         (structure Attr = ColourAttr.Attr
          structure SuperClass = PointClass)

   structure AttrPointInstance =
     AttrPointInstance
        (structure Attr = ColourAttr.Attr
         structure PointClass = PointClass
         structure AttrPointClass = AttrPointClass)
in
   structure ColourAttr = ColourAttr
   structure AttrPointInstance = AttrPointInstance
end

local
   structure RGBA = ColourAttr.RGBAlphaColour
   structure RGB = ColourAttr.RGBColour
   structure ColPoint = AttrPointInstance.AttrPoint
   structure Point = AttrPointInstance.Point
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

   val p2 = Point.Clone p1 ()

   val 1 = Point.Getx p1 ()
   val 2 = Point.Gety p1 ()

   val true = Point.Equal p1 p2

   val p4 = Point.New (1,2)

   val true = Point.Equal p1 p4

   val rp4 = ColPoint.fromPoint p4

   val true = ColPoint.Equal rp1 rp4

   val purple = RGBA.New (1.0, 0.0, 1.0, 1.0)

   val () = ColPoint.SetAttr rp1 purple

   val true = Point.Equal p1 p4

   val p3 = ColPoint.toPoint rp1

   val 1 = Point.Getx p3 ()
   val 2 = Point.Gety p3 ()

   val true = Point.Equal p1 p2

   val () = ColPoint.Move rp1 (1,1)

   val false = Point.Equal p1 p2

   val 2 = Point.Getx p3 ()
   val 3 = Point.Gety p3 ()

   val true = Point.Equal (ColPoint.toPoint rp1) p3
end

