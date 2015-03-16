(* This is one way NOT to do it. What a nightmare! The method
invocation has to be threaded through the object network, you will
never be able to layer it on top as a concrete abstraction. The types
will lock everything down if you try. *)

signature PointClassMethods =
sig
   type IVs
   type CoOrds
   structure ClassMethods
      : ClassMethods
         where type initargs = CoOrds
            and type IVs = { coords : CoOrds ref }
end

functor PointClassMethods
   (type CoOrds
    val equal : CoOrds * CoOrds -> bool)
   :> PointClassMethods
       where type CoOrds = CoOrds
         and type IVs = {coords : CoOrds ref} =
struct
   type IVs = {coords : CoOrds ref}
   type CoOrds = CoOrds
   structure ClassMethods
      :> ClassMethods
         where type initargs = CoOrds
            and type IVs = IVs =
   struct
      type IVs = IVs
      type initargs = CoOrds
      val init : initargs -> IVs
           = fn cs => {coords = ref cs}
      val tini : IVs -> initargs
           = fn {coords = ref cs} => cs
      val equal : IVs -> IVs -> bool
           = fn {coords = ref cs} =>
             fn {coords = ref cs'} =>
                equal (cs, cs')
   end
end

signature PointSubClassMethods =
sig
   type IVs
   type initargs
   type superinitargs
   structure ClassMethods
      : SubClassMethods
         where type initargs = initargs
            and type IVs = IVs
end

functor PointSubClassMethods
   (type initargs
    type superinitargs
    type IVs
    val equal : IVs * IVs -> bool
    val init : initargs -> IVs
    val tini : IVs -> initargs
    val SuperInitArgs : initargs -> superinitargs
    val mkSuperNew : IVs -> (initargs -> 'b) -> superinitargs -> 'b)
   :> PointSubClassMethods
       where type initargs = initargs
         and type ClassMethods.superinitargs = superinitargs
         and type IVs = IVs =
struct
   type IVs = IVs
   type initargs = initargs
   type superinitargs = superinitargs
   structure ClassMethods
      :> SubClassMethods
         where type initargs = initargs
            and type superinitargs = superinitargs
            and type IVs = IVs =
   struct
      type IVs = IVs
      type initargs = initargs
      type superinitargs = superinitargs
      val init : initargs -> IVs
           = fn cs => init cs
      val tini : IVs -> initargs
           = fn cs => tini cs
      val equal : IVs -> IVs -> bool
           = fn cs =>
             fn cs' =>
                equal (cs, cs')
      val SuperInitArgs = SuperInitArgs
      val mkSuperNew = mkSuperNew
   end
end

signature PointClassGetters =
sig
   type IVs
   type CoOrds
   val coords : IVs -> unit -> CoOrds
end

signature PointClassSetters =
sig
   type IVs
   type CoOrds
   val coords : IVs -> CoOrds -> unit
   val move : IVs -> CoOrds -> unit
end

signature PointInstanceMethods =
sig
   type Point
   type CoOrds
   structure Get :
   sig
      include PointClassGetters
         where type IVs = Point
           and type CoOrds = CoOrds
   end
   structure Set :
   sig
      include PointClassSetters
         where type IVs = Point
           and type CoOrds = CoOrds
   end
end

functor PointInstanceMethods
   (type CoOrds
    val move : CoOrds * CoOrds -> CoOrds
    structure ClassInstanceMethods : ClassInstanceMethods
         where type IVs = {coords : CoOrds ref})
   :> PointInstanceMethods
       where type CoOrds = CoOrds
         and type Point = ClassInstanceMethods.Self ClassInstanceMethods.Wrap =
struct
   type CoOrds = CoOrds
   type Point = ClassInstanceMethods.Self ClassInstanceMethods.Wrap
   local
      type IVs = ClassInstanceMethods.IVs
      structure Getters : PointClassGetters =
      struct
         type IVs = IVs
         type CoOrds = CoOrds
         val coords : IVs -> unit -> CoOrds
               = fn ivs => fn () => !(#coords ivs)
      end
      structure Setters : PointClassSetters =
      struct
         type IVs = IVs
         type CoOrds = CoOrds
         val move : IVs -> CoOrds -> unit
               = fn ivs => fn coords =>
                     let val coords' = !(#coords ivs)
                     in (#coords ivs) := move (coords',coords)
                     end 
         val coords : IVs -> CoOrds -> unit
               = fn ivs => fn coords' => (#coords ivs) := coords'
      end
   in
      val wrapMethod = ClassInstanceMethods.wrapMethod
      structure Get =
      struct
         type IVs = Point
         type CoOrds = CoOrds
         val coords : Point -> unit -> CoOrds = wrapMethod Getters.coords
      end
      structure Set =
      struct
         type IVs = Point
         type CoOrds = CoOrds
         val move : Point -> CoOrds -> unit = wrapMethod Setters.move
         val coords : Point -> CoOrds -> unit = wrapMethod Setters.coords
      end
   end
end

signature PointClass =
sig
   include PointInstanceMethods
   val New : CoOrds -> Point
   val Clone : Point -> unit -> Point
   val Equal : Point -> Point -> bool
end

functor Point2Class
  (type CoOrd
   val add : CoOrd * CoOrd -> CoOrd
   val equal : CoOrd * CoOrd -> bool)
     :> PointClass
          where type CoOrds = CoOrd * CoOrd =
struct
   type CoOrds = CoOrd * CoOrd
   local
      structure Class =
         PointClassMethods
           (type CoOrds = CoOrds
            val equal : CoOrds * CoOrds -> bool =
                 fn ((x,y),(x',y')) =>
                     (equal(x,x') andalso
                      equal(y,y')))
      structure ClassInstance =
         ClassInstance
           (structure ClassMethods = Class.ClassMethods)
      structure InstanceMethods =
         PointInstanceMethods
          (type CoOrds = CoOrds
           val move : CoOrds * CoOrds -> CoOrds =
                  fn ((x,y),(x',y')) => (add(x,x'), add(y,y'))
           structure ClassInstanceMethods = ClassInstance.InstanceMethods)
   in
      type Point = ClassInstance.Self ClassInstance.Wrap
      local open ClassInstance.InstanceMethods
      in
         val New : CoOrds -> Point = New
         val Clone : Point -> unit -> Point = Clone
         val Equal : Point -> Point -> bool = Equal
      end
      open InstanceMethods
   end
end

functor Point3Class
  (type CoOrd
   val add : CoOrd * CoOrd -> CoOrd
   val equal : CoOrd * CoOrd -> bool)
     :> PointClass
          where type CoOrds = CoOrd * CoOrd * CoOrd =
struct
   type CoOrds = CoOrd * CoOrd * CoOrd
   local
      structure Class =
         PointClassMethods
           (type CoOrds = CoOrds
            val equal : CoOrds * CoOrds -> bool =
                 fn ((x,y,z),(x',y',z')) =>
                     (equal(x,x') andalso
                      equal(y,y') andalso
                      equal(z,z')))
      structure ClassInstance =
         ClassInstance
           (structure ClassMethods = Class.ClassMethods)
      structure InstanceMethods =
         PointInstanceMethods
          (type CoOrds = CoOrds
           val move : CoOrds * CoOrds -> CoOrds =
                  fn ((x,y,z),(x',y',z')) => (add(x,x'), add(y,y'), add(z,z'))
           structure ClassInstanceMethods = ClassInstance.InstanceMethods)
   in
      type Point = ClassInstance.Self ClassInstance.Wrap

      val New : CoOrds -> Point = ClassInstance.InstanceMethods.New
      val Clone : Point -> unit -> Point = ClassInstance.InstanceMethods.Clone
      val Equal : Point -> Point -> bool = ClassInstance.InstanceMethods.Equal

      open InstanceMethods
   end
end

signature AttrClassGetters =
sig
   type IVs
   type Value
   val value : IVs -> unit -> Value
end

signature AttrClassSetters =
sig
   type IVs
   type Value
   val value : IVs -> Value -> unit
end

signature AttrInstanceMethods =
sig
   type Attr
   type Value
   type IVs
   structure Get :
   sig
      include AttrClassGetters
         where type IVs = Attr
           and type Value = Value
   end
   structure Set :
   sig
      include AttrClassSetters
         where type IVs = Attr
           and type Value = Value
   end
end

functor AttrInstanceMethods
   (type Value
    structure ClassInstanceMethods : ClassInstanceMethods
         where type IVs = {value : Value ref})
   :> AttrInstanceMethods
       where type Value = Value
         and type Attr = ClassInstanceMethods.Self ClassInstanceMethods.Wrap
         and type IVs = ClassInstanceMethods.IVs =
struct
   type Value = Value
   type Attr = ClassInstanceMethods.Self ClassInstanceMethods.Wrap
   type IVs = ClassInstanceMethods.IVs
   local
      structure Getters : AttrClassGetters =
      struct
         type IVs = IVs
         type Value = Value
         val value : IVs -> unit -> Value
               = fn ivs => fn () => !(#value ivs)
      end
      structure Setters : AttrClassSetters =
      struct
         type IVs = IVs
         type Value = Value
         val value : IVs -> Value -> unit
               = fn ivs => fn value' => (#value ivs) := value'
      end
   in
      val wrapMethod = ClassInstanceMethods.wrapMethod
      structure Get =
      struct
         type IVs = Attr
         type Value = Value
         val value : Attr -> unit -> Value = wrapMethod Getters.value
      end
      structure Set =
      struct
         type IVs = Attr
         type Value = Value
         val value : Attr -> Value -> unit = wrapMethod Setters.value
      end
   end
end

signature AttrClass =
sig
   include AttrInstanceMethods

   val New : Value -> Attr
   val Clone : Attr -> unit -> Attr
   val Equal : Attr -> Attr -> bool
end

signature AttrClassMethods =
sig
   type IVs
   type Value
   structure ClassMethods
      : ClassMethods
         where type initargs = Value
            and type IVs = { value : Value ref }
end

functor AttrClassMethods
   (type Value
    val equal : Value * Value -> bool)
   :> AttrClassMethods
       where type Value = Value
         and type IVs = {value : Value ref} =
struct
   type IVs = {value : Value ref}
   type Value = Value
   structure ClassMethods
      :> ClassMethods
         where type initargs = Value
            and type IVs = IVs =
   struct
      type IVs = IVs
      type initargs = Value
      val init : initargs -> IVs
           = fn cs => {value = ref cs}
      val tini : IVs -> initargs
           = fn {value = ref cs} => cs
      val equal : IVs -> IVs -> bool
           = fn {value = ref cs} =>
             fn {value = ref cs'} =>
                equal (cs, cs')
   end
end

signature AttrPointClass =
sig
   type Attr
   type Point
   type Value
   type CoOrds
   structure Point : PointInstanceMethods
      where type Point = Point
        and type CoOrds = CoOrds
   val New : CoOrds * Attr -> Point
   val Clone : Point -> unit -> Point
   val Equal : Point -> Point -> bool
end

functor AttrPointClass
  (type CoOrds
   type Attr
   type Value
   type IVs
   val move : CoOrds * CoOrds -> CoOrds
   val equal : CoOrds * CoOrds -> bool
   structure SubClass : PointSubClassMethods
       where type initargs = CoOrds * Attr
         and type ClassMethods.superinitargs = CoOrds)
     :> AttrPointClass
          where type CoOrds = CoOrds
            and type Attr = Attr
            and type Value = Value =
struct
   type CoOrds = CoOrds
   type Attr = Attr
   type Value = Value
   local
      structure Class =
         PointClassMethods
           (type CoOrds = CoOrds
            val equal = equal)
      structure ClassInstance =
         ClassInstance
           (structure ClassMethods = Class.ClassMethods)
      structure SubClassInstance =
         SubClassInstance
           (structure SuperClass = ClassInstance
            structure ClassMethods = SubClass.ClassMethods) :> SubClassPrivate
                 where type InstanceMethods.IVs = { coords : CoOrds ref }
                   and type InstanceMethods.initargs = CoOrds * Attr
      structure InstanceMethods =
         PointInstanceMethods
          (type CoOrds = CoOrds
           val move = move
           structure ClassInstanceMethods
             :> ClassInstanceMethods
                 where type IVs = { coords : CoOrds ref }
                   and type Self = SubClassInstance.InstanceMethods.Self
                   and type 'a Wrap = 'a SubClassInstance.InstanceMethods.Wrap =
                   SubClassInstance.InstanceMethods)
   in
      type Point = SubClassInstance.Self SubClassInstance.Wrap
      val New : CoOrds * Attr -> Point = SubClassInstance.InstanceMethods.New
      val Clone : Point -> unit -> Point = SubClassInstance.InstanceMethods.Clone
      val Equal : Point -> Point -> bool = SubClassInstance.InstanceMethods.Equal
      structure Point = InstanceMethods
   end
end

functor AttrPoint2Class
  (type CoOrd
   type Attr
   type Value
   type IVs
   val add : CoOrd * CoOrd -> CoOrd
   val equal : CoOrd * CoOrd -> bool
   val zero : CoOrd
   val attrEqual : Attr * Attr -> bool
   val attrZero : Attr) =
struct
   local
      type CoOrds = CoOrd * CoOrd
      structure SubClass =
         PointSubClassMethods
              (type initargs = CoOrds * Attr
               type superinitargs = CoOrds
               type IVs = Attr
               val equal : Attr * Attr -> bool =
                    fn (a,a') =>
                        (attrEqual (a,a'))
               val init : CoOrds * Attr -> Attr =
                    fn (_,a) => a
               val tini : Attr -> CoOrds * Attr =
                    fn a => ((zero,zero),a)
               val SuperInitArgs : initargs -> superinitargs =
                    fn ((x,y),a) => (x,y)
               val mkSuperNew : IVs -> (initargs -> 'b) -> superinitargs -> 'b =
                    fn a => fn f => fn (x,y) => f ((x,y),a))
        structure AttrPointClass =
          AttrPointClass
           (type CoOrds = CoOrds
            type Attr = Attr
            type Value = Value
            type IVs = IVs
            val equal : CoOrds * CoOrds -> bool =
               fn ((x,y),(x',y')) =>
                   (equal (x,x') andalso
                    equal (y,y'))
            val move : CoOrds * CoOrds -> CoOrds =
              fn ((x,y),(x',y')) => (add(x,x'), add(y,y'))
            structure SubClass = SubClass)
   in
      open AttrPointClass
   end
end

functor Attr3Class
  (type CoOrd
   val equal : CoOrd * CoOrd -> bool)
     :> AttrClass
          where type Value = CoOrd * CoOrd * CoOrd =
struct
   type Value = CoOrd * CoOrd * CoOrd
   local
      structure Class =
         AttrClassMethods
           (type Value = Value
            val equal : Value * Value -> bool =
                 fn ((x,y,z),(x',y',z')) =>
                     (equal(x,x') andalso
                      equal(y,y') andalso
                      equal(z,z')))
      structure ClassInstance =
         ClassInstance
           (structure ClassMethods = Class.ClassMethods)
      structure InstanceMethods =
         AttrInstanceMethods
           (type Value = Value
            structure ClassInstanceMethods = ClassInstance.InstanceMethods)
   in
      type Attr = ClassInstance.Self ClassInstance.Wrap
      type Value = Value
      type IVs = InstanceMethods.IVs

      val New : Value -> Attr = ClassInstance.InstanceMethods.New
      val Clone : Attr -> unit -> Attr = ClassInstance.InstanceMethods.Clone
      val Equal : Attr -> Attr -> bool = ClassInstance.InstanceMethods.Equal

      open InstanceMethods
   end
end

structure ColourInt =
   Attr3Class
      (type CoOrd = int
       val equal = op =)

structure AttrPoint2Int =
 AttrPoint2Class
  (type CoOrd = int
   type Attr = ColourInt.Attr
   type Value = ColourInt.Value
   type IVs = ColourInt.IVs

   val add : CoOrd * CoOrd -> CoOrd = Int.+
   val equal : CoOrd * CoOrd -> bool = (op =)
   val zero : CoOrd = 0
   val attrEqual : Attr * Attr -> bool = fn (a,b) => ColourInt.Equal a b
   val attrZero : Attr = ColourInt.New (255,255,255));

structure Point2Int =
   Point2Class
      (type CoOrd = int
       val add = Int.+
       val equal = op =)

local
   open Point2Int
in
   val p1 = New (1,2);
   val p2 = Clone p1 ();
   val true = Equal p1 p2;
   val () = Set.move p1 (1,1);
   val false = Equal p1 p2;
   val c1 = Get.coords p1 ();
   val c2 = Get.coords p2 ();
   val () = Set.coords p2 (2,3);
   val true = Equal p1 p2;
end;

structure Point3Real =
   Point3Class
      (type CoOrd = real
       val add = Real.+
       val equal = Real.==)

local open Point3Real
in
   infix 9 %
   fun obj % method = fn a => method obj a
   val p1 = New (1.0,1.0,2.0);
   val p2 = (p1%Clone) ();
   val () = (p1%Set.move) (1.0,1.0,1.0);
   val c1 = (p1%Get.coords) ();
   val c2 = (p2%Get.coords) ();
end

