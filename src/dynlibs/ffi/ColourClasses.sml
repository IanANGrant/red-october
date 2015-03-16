signature RGBColourClassPrivate =
sig
   type 'a F =
     {clone : unit -> 'a,
      equal : 'a -> bool,
      getr : unit -> real,
      getg : unit -> real,
      getb : unit -> real,
      setr : real -> unit,
      setg : real -> unit,
      setb : real -> unit}
   datatype Self = SELF of Self F
   datatype 'a Wrap = WRAP of ('a Wrap) F * 'a
   type Vars
   val New : real * real * real -> Self Wrap
   val Clone : 'a Wrap -> unit -> 'a Wrap
   val Equal : 'a Wrap -> 'b Wrap -> bool
   val Getr : 'a Wrap -> unit -> real
   val Getg : 'a Wrap -> unit -> real
   val Getb : 'a Wrap -> unit -> real
   val Setr : 'a Wrap -> real -> unit
   val Setg : 'a Wrap -> real -> unit
   val Setb : 'a Wrap -> real -> unit
   val init : real * real * real -> Vars
   val new : real * real * real -> Self
   val clone : Vars * (real * real * real -> 'a Wrap) -> unit -> 'a
   val equal : 'a Wrap -> 'b Wrap -> bool
   val getr : Vars -> unit -> real
   val getg : Vars -> unit -> real
   val getb : Vars -> unit -> real
   val setr : Vars -> real -> unit
   val setg : Vars -> real -> unit
   val setb : Vars -> real -> unit
   val wrap : Self -> Self Wrap
   val unwrap : 'a Wrap -> 'a
   val view :
     'a Wrap ->
     {clone : unit -> 'a Wrap,
      equal : 'a Wrap -> bool,
      getr : unit -> real,
      getg : unit -> real,
      getb : unit -> real,
      setr : real -> unit,
      setg : real -> unit,
      setb : real -> unit}
end

signature RGBAlphaColourClassPrivate =
sig
   structure Super : RGBColourClassPrivate
   type 'a F =
     {clone : unit -> 'a,
      equal : 'a -> bool,
      getr : unit -> real,
      getg : unit -> real,
      getb : unit -> real,
      geta : unit -> real,
      setr : real -> unit,
      setg : real -> unit,
      setb : real -> unit,
      seta : real -> unit}
   datatype Self = SELF of Self F
   datatype 'a Wrap = WRAP of ('a Wrap) F * 'a
   type Vars = {a : real ref, sup : Super.Vars}
   val New : real * real * real * real -> Self Wrap
   val Clone : 'a Wrap -> unit -> 'a Wrap
   val Equal : 'a Wrap -> 'b Wrap -> bool
   val Getr : 'a Wrap -> unit -> real
   val Getg : 'a Wrap -> unit -> real
   val Getb : 'a Wrap -> unit -> real
   val Geta : 'a Wrap -> unit -> real
   val Setr : 'a Wrap -> real -> unit
   val Setg : 'a Wrap -> real -> unit
   val Setb : 'a Wrap -> real -> unit
   val Seta : 'a Wrap -> real -> unit
   val toRGBColour : Self Wrap -> Self Super.Wrap
   val fromRGBColour : Self Super.Wrap -> Self Wrap
   val init : real * real * real * real -> Vars
   val new : real * real * real * real -> Self
   val equal : 'a Wrap -> 'b Wrap -> bool
   val geta : {a : real ref, sup : Super.Vars} -> unit -> real
   val seta : {a : real ref, sup : Super.Vars} -> real -> unit
   val wrap : Self -> Self Wrap
   val unwrap : 'a Wrap -> 'a
   val view :
     'a Wrap -> 'a Wrap F
   val wrapSuper : Self -> Self Super.Wrap
   val SuperWrapFromWrap : 'a Wrap -> 'a Wrap Super.Wrap
end

functor RGBColourClassInstance()
  :> RGBColourClassPrivate =
struct
   val op == = Real.==
   infix 5 ==
   type 'a F = { getr : unit -> real,
                 getg : unit -> real,
                 getb : unit -> real,
                 setr : real -> unit,
                 setg : real -> unit,
                 setb : real -> unit,
                 equal : 'a -> bool,
                 clone : unit -> 'a}
   datatype Self = SELF of Self F
   datatype 'a Wrap = WRAP of ('a Wrap) F * 'a
   fun unwrap (WRAP(_,obj)) = obj
   fun view (WRAP(r,_)) = r
   fun wrap (c as SELF cr) : Self Wrap =
         WRAP ({ getr = (#getr cr), 
                 getg = (#getg cr), 
                 getb = (#getb cr), 
                 setr = (#setr cr), 
                 setg = (#setg cr), 
                 setb = (#setb cr), 
                 equal = (#equal cr) o unwrap, 
                 clone = wrap o (#clone cr)},c)
   abstype Vars = VARS of {r : real ref, g : real ref, b : real ref}
   with
      fun init (r,g,b) = VARS {r = ref r, g = ref g, b = ref b}
      fun getr (VARS iv)() = !(#r iv)
      fun getg (VARS iv)() = !(#g iv)
      fun getb (VARS iv)() = !(#b iv)
      fun setr (VARS iv) (r) = (#r iv) := r
      fun setg (VARS iv) (g) = (#g iv) := g
      fun setb (VARS iv) (b) = (#b iv) := b
      fun equal (self) (p) = (#getr (view self))() == (#getr (view p))()
                     andalso (#getg (view self))() == (#getg (view p))()
                     andalso (#getb (view self))() == (#getb (view p))()
      fun clone (VARS iv,selfclass)() = (unwrap o selfclass) (!(#r iv),!(#g iv),!(#b iv))
   end
   fun new (r,g,b) =
      let val iv : Vars = init(r,g,b)
          fun self () = SELF {
                         getr = getr(iv),
                         getg = getg(iv),
                         getb = getb(iv),
                         setr = setr(iv),
                         setg = setg(iv),
                         setb = setb(iv),
                         equal = fn (p) => equal (wrap(self())) (wrap p),
                         clone = clone (iv,wrap o new)}
      in self ()
      end
   val Equal : 'a Wrap -> 'b Wrap -> bool = equal
   val New : real * real * real -> Self Wrap = wrap o new
   val Clone = fn p => #clone (view p)
   val Getr = fn p => #getr (view p)
   val Getg = fn p => #getg (view p)
   val Getb = fn p => #getb (view p)
   val Setr = fn p => #setr (view p)
   val Setg = fn p => #setg (view p)
   val Setb = fn p => #setb (view p)
end

functor RGBAlphaColourClassInstance
  (structure SuperClass : RGBColourClassPrivate)
    :> RGBAlphaColourClassPrivate 
        where type 'a Super.Wrap = 'a SuperClass.Wrap =
struct
   val op == = Real.==
   infix 5 ==
   structure Super = SuperClass
   type 'a F = { geta : unit -> real,
                 seta : real -> unit,
                 getr : unit -> real,
                 getg : unit -> real,
                 getb : unit -> real,
                 setr : real -> unit,
                 setg : real -> unit,
                 setb : real -> unit,
                 equal : 'a -> bool,
                 clone : unit -> 'a}
   datatype Self = SELF of Self F
   datatype 'a Wrap = WRAP of ('a Wrap) F * 'a
   fun unwrap (WRAP(_,obj)) = obj
   fun view (WRAP(r,_)) = r
   fun wrap (ac as SELF acr) =
         WRAP ({ geta = (#geta acr),
                 seta = (#seta acr),
                 getr = (#getr acr),
                 getg = (#getg acr),
                 getb = (#getb acr),
                 setr = (#setr acr), 
                 setg = (#setg acr), 
                 setb = (#setb acr), 
                 equal = (#equal acr) o unwrap, 
                 clone = wrap o (#clone acr)},ac)
   fun wrapSuper (ac as SELF acr) =
            Super.WRAP ({ getr = (#getr acr),
                          getg = (#getg acr), 
                          getb = (#getb acr), 
                          setr = (#setr acr), 
                          setg = (#setg acr), 
                          setb = (#setb acr), 
                          equal = (#equal acr) o Super.unwrap, 
                          clone = wrapSuper o (#clone acr)},ac)
   fun SuperWrapFromWrap (wac) =
            Super.WRAP ({ getr = (#getr (view wac)),
                          getg = (#getg (view wac)), 
                          getb = (#getb (view wac)), 
                          setr = (#setr (view wac)), 
                          setg = (#setg (view wac)), 
                          setb = (#setb (view wac)), 
                          equal = (#equal (view wac)) o Super.unwrap, 
                          clone = SuperWrapFromWrap o (#clone (view wac))},wac)
   type Vars = {a : real ref, sup : Super.Vars}
   fun init (r,g,b,a) = {a = ref a, sup = Super.init (r,g,b)}
   fun geta (iv : Vars)() = !(#a iv)
   fun seta (iv : Vars)(a) = (#a iv) := a
   fun equal (self)(p) = Super.equal (SuperWrapFromWrap self) (SuperWrapFromWrap p)
                           andalso (#geta (view self)) () == (#geta (view p)) ()
   fun new (r,g,b,a) =
      let val iv : Vars = init (r,g,b,a)
          fun mkSuperNew a = fn (r,g,b) => new(r,g,b,a)
          val superNew = mkSuperNew 1.0
          fun self () = SELF {
                         geta = geta(iv),
                         seta = seta(iv),
                         getr = Super.getr(#sup iv),
                         getg = Super.getg(#sup iv),
                         getb = Super.getb(#sup iv),
                         setr = Super.setr(#sup iv),
                         setg = Super.setg(#sup iv),
                         setb = Super.setb(#sup iv),
                         equal = fn p => equal (wrap(self())) (wrap p),
                         clone = Super.clone (#sup iv, wrapSuper o superNew)}
      in self ()
      end
   val New = wrap o new
   val Equal = equal
   val Clone = fn p => #clone (view p)
   val toRGBColour = wrapSuper o unwrap
   val fromRGBColour = wrap o Super.unwrap
   val Seta = fn p => #seta (view p)
   val Geta = fn p => #geta (view p)
   val Getr = fn p => #getr (view p)
   val Getg = fn p => #getg (view p)
   val Getb = fn p => #getb (view p)
   val Setr = fn p => #setr (view p)
   val Setg = fn p => #setg (view p)
   val Setb = fn p => #setb (view p)
end

signature AbstractRGBColourClass =
sig
   type Self
   type 'a Wrap
   val Equal : 'a Wrap -> 'b Wrap -> bool
   val Clone : 'a Wrap -> unit -> 'a Wrap
   val Getr : 'a Wrap -> unit -> real
   val Getg : 'a Wrap -> unit -> real
   val Getb : 'a Wrap -> unit -> real
   val Setr : 'a Wrap -> real -> unit
   val Setg : 'a Wrap -> real -> unit
   val Setb : 'a Wrap -> real -> unit
end

signature RGBColourClass =
sig
   include AbstractRGBColourClass
   val New : real * real * real -> Self Wrap
end

signature RGBAlphaColourClass =
sig
   include AbstractRGBColourClass
   type 'a SuperWrap
   structure RGBColour : RGBColourClass
   val New : real * real * real * real -> Self Wrap
   val toRGBColour : Self Wrap -> Self SuperWrap
   val fromRGBColour : Self SuperWrap -> Self Wrap
   val Geta : 'a Wrap -> unit -> real
   val Seta : 'a Wrap -> real -> unit
end

functor RGBAlphaColour
   (structure RGBColourClass : RGBColourClassPrivate
    structure RGBAlphaColourClass : RGBAlphaColourClassPrivate
       sharing type RGBColourClass.Wrap = RGBAlphaColourClass.Super.Wrap)
         :> RGBAlphaColourClass 
              where type 'a RGBColour.Wrap = 'a RGBColourClass.Wrap
                and type 'a SuperWrap = 'a RGBColourClass.Wrap = 
struct
   type 'a SuperWrap = 'a RGBAlphaColourClass.Super.Wrap
   type Self = RGBAlphaColourClass.Self
   type 'a Wrap = 'a RGBAlphaColourClass.Wrap
   structure RGBColour :> RGBColourClass
      where type 'a Wrap = 'a SuperWrap =
   struct
      open RGBColourClass
   end
   open RGBAlphaColourClass
end

signature AbstractRGBColour =
sig
   type colour
   val Equal : colour -> colour -> bool
   val Clone : colour -> unit -> colour
   val Getr : colour -> unit -> real
   val Getg : colour -> unit -> real
   val Getb : colour -> unit -> real
   val Setr : colour -> real -> unit
   val Setg : colour -> real -> unit
   val Setb : colour -> real -> unit         
end

signature RGBColour =
sig
   include AbstractRGBColour
   val New : real * real * real -> colour
end

signature RGBAlphaColour =
sig
   include AbstractRGBColour
   type supercolour
   val New : real * real * real * real -> colour
   val toRGBColour : colour -> supercolour
   val fromRGBColour : supercolour -> colour
   val Geta : colour -> unit -> real
   val Seta : colour -> real -> unit
end

signature AbstractAttr =
sig
   type attr
   val default : attr
   val eq : attr * attr -> bool
end

signature Attr =
sig
   include AbstractAttr
   type super
   val toSuper : attr -> super
   val fromSuper : super -> attr
end

signature RGBAlphaColourAttr =
sig type RGB
    type RGBA
    structure RGBAlphaColour
                : RGBAlphaColour
                   where type supercolour = RGB
                          and type colour = RGBA
    structure RGBAAttr
                : Attr
                   where type attr = RGBA
    structure RGBColour
                : RGBColour
                   where type colour = RGB
    structure RGBAttr
                : Attr
                   where type attr = RGB
end

functor RGBAlphaColourAttr
     (structure RGBColourClass : RGBColourClassPrivate
      structure RGBAlphaColourClass : RGBAlphaColourClassPrivate
       sharing type RGBColourClass.Wrap = RGBAlphaColourClass.Super.Wrap)
       :> RGBAlphaColourAttr =
struct
   local
      structure RGBAlphaColour1 =
         RGBAlphaColour
           (structure RGBColourClass = RGBColourClass
            structure RGBAlphaColourClass = RGBAlphaColourClass)
               :> RGBAlphaColourClass
                    where type 'a RGBColour.Wrap = 'a RGBColourClass.Wrap
                      and type 'a SuperWrap = 'a RGBColourClass.Wrap
   in
      type RGB = RGBAlphaColour1.Self RGBAlphaColour1.RGBColour.Wrap
      type RGBA = RGBAlphaColour1.Self RGBAlphaColour1.Wrap
      structure RGBAlphaColour
                :> RGBAlphaColour
                        where type supercolour = RGB
                          and type colour = RGBA =
      struct
         type supercolour = RGB
         type colour = RGBA
         open RGBAlphaColour1
      end
      structure RGBAAttr :> Attr
           where type attr = RGBA
             and type super = RGB =
      struct
         type attr = RGBA
         type super = RGB
         val default = RGBAlphaColour.New (1.0, 1.0, 1.0, 1.0)
         val eq = fn (x,y) => RGBAlphaColour.Equal x y
         val toSuper = RGBAlphaColour.toRGBColour
         val fromSuper = RGBAlphaColour.fromRGBColour
      end
      structure RGBColour
           :> RGBColour
                where type colour = RGB =
      struct
         type colour = RGB
         local open RGBAlphaColour1.RGBColour
         in
            val New : real * real * real -> colour = 
                   fn (r,g,b) => RGBAlphaColour.toRGBColour
                                    (RGBAlphaColour.New (r, g, b, 1.0))
            val Equal : colour -> colour -> bool = Equal
            val Clone : colour -> unit -> colour = Clone
            val Getr : colour -> unit -> real = Getr
            val Getg : colour -> unit -> real = Getg
            val Getb : colour -> unit -> real = Getb
            val Setr : colour -> real -> unit = Setr
            val Setg : colour -> real -> unit = Setg
            val Setb : colour -> real -> unit = Setb
         end
      end
      structure RGBAttr :> Attr
           where type attr = RGB
             and type super = RGBA =
      struct
         type attr = RGB
         type super = RGBA
         val default = RGBColour.New (1.0, 1.0, 1.0)
         val eq = fn (x,y) => RGBColour.Equal x y
         val toSuper = RGBAlphaColour.fromRGBColour
         val fromSuper = RGBAlphaColour.toRGBColour
      end
   end
end
