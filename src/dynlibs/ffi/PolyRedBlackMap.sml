(* Redblackmap --                                                   *) 
(*    applicative maps implemented by Okasaki-style Red-Black trees *)
(* Ken Friis Larsen <ken@friislarsen.net>                           *)
(* Polymorphic mods by Ian Grant                                    *)

(* Redblackmap -- applicative maps as Red-black trees *)

signature PolyRedBlackMap =
sig

type ('a, 'b) dict

exception NotFound

val mkDict    : unit -> ('a, 'b) dict
val insert    : ('a, 'b) dict * 'a * 'b -> ('a, 'b) dict
val find      : ('a, 'b) dict * 'a -> 'b
val bounds    : ('a, 'b) dict * 'a -> ('a * 'b) option * ('a * 'b) option * ('a * 'b) option
val gle       : ('a, 'b) dict * 'a -> ('a * 'b) option
val glt       : ('a, 'b) dict * 'a -> ('a * 'b) option
val lge       : ('a, 'b) dict * 'a -> ('a * 'b) option
val lgt       : ('a, 'b) dict * 'a -> ('a * 'b) option
val peek      : ('a, 'b) dict * 'a -> 'b option
val remove    : ('a, 'b) dict * 'a -> ('a, 'b) dict * 'b
val numItems  : ('a, 'b) dict -> int
val listItems : ('a, 'b) dict -> ('a * 'b) list
val app       : ('a * 'b -> unit) -> ('a,'b) dict -> unit
val revapp    : ('a * 'b -> unit) -> ('a,'b) dict -> unit
val foldr     : ('a * 'b * 'b -> 'b)-> 'b -> ('a,'b) dict -> 'b
val foldl     : ('a * 'b * 'b -> 'b) -> 'b -> ('a,'b) dict -> 'b
val map       : ('a * 'b -> 'b) -> ('a,'b) dict -> ('a, 'b) dict
val transform : ('b -> 'b) -> ('a,'b) dict -> ('a, 'b) dict
end

(* 
   [('a, 'b) dict] is the type of applicative maps from domain type
   'a to range type 'b, or equivalently, applicative dictionaries
   with keys of type 'a and values of type 'b.  They are implemented
   as Okasaki-style red-black trees.

   [mkDict ()] returns a new, empty map.

   [insert(m, i, v)] extends (or modifies) map m to map i to v.

   [find (m, k)] returns v if m maps k to v; otherwise raises NotFound.

   [gle (m, k)] Returns the greatest key-value pair less than or equal
   to k; NONE otherwise.

   [glt (m, k)] Returns the greatest key-value pair less than k; NONE
   otherwise.

   [lge (m, k)] Returns the least key-value pair greater than or equal
   to k; NONE otherwise.

   [lgt (m, k)] Returns the least key-value pair greater than k; NONE
   otherwise.

   [bounds (m, k)] Returns a triple consisting of the greatest
   key-value pair less than k, k itself, and the least key-value pair
   greater than k; when any of these do not exist, NONE is returned in
   the corresponding position of the result.

   [peek(m, k)] returns SOME v if m maps k to v; otherwise returns NONE.

   [remove(m, k)] removes k from the domain of m and returns the
   modified map and the element v corresponding to k.  Raises NotFound
   if k is not in the domain of m.

   [numItems m] returns the number of entries in m (that is, the size
   of the domain of m).

   [listItems m] returns a list of the entries (k, v) of keys k and
   the corresponding values v in m, in order of increasing key values.

   [app f m] applies function f to the entries (k, v) in m, in
   increasing order of k (according to the ordering ordr used to
   create the map or dictionary).

   [revapp f m] applies function f to the entries (k, v) in m, in
   decreasing order of k.

   [foldl f e m] applies the folding function f to the entries (k, v)
   in m, in increasing order of k.

   [foldr f e m] applies the folding function f to the entries (k, v)
   in m, in decreasing order of k.

   [map f m] returns a new map whose entries have form (k, f(k,v)),
   where (k, v) is an entry in m.

   [transform f m] returns a new map whose entries have form (k, f v),
   where (k, v) is an entry in m.
*)

structure PolyRedBlackMap :> PolyRedBlackMap  =
struct

  type sym = AbsObjRepr.sym

  val to_repr : 'a -> sym = AbsObjRepr.objAbsRepr
  val from_repr : sym -> 'a = AbsObjRepr.absReprObj

  datatype ('a, 'b) tree = 
           LEAF
         | RED   of sym * 'b * ('a, 'b) tree * ('a, 'b) tree
         | BLACK of sym * 'b * ('a, 'b) tree * ('a, 'b) tree

  type ('a, 'b) dict  =  ('a, 'b) tree * int

  exception NotFound

  val compare = AbsObjRepr.compare_abs

  fun mkDict () = (LEAF, 0)

  fun numItems (_, n) = n

  fun find ((tree, n), key') =
      let val key = to_repr key'
          fun loopShared k x left right =
              case compare(key, k) of
                  EQUAL   => x
                | LESS    => loop left
                | GREATER => loop right
          and loop LEAF                       = raise NotFound
            | loop (RED(k, x, left, right))   = loopShared k x left right
            | loop (BLACK(k, x, left, right)) = loopShared k x left right
      in  loop tree end

  fun bounds ((tree, n), key') =
      let val key = to_repr key'
          fun loop tree (b,m,a) = 
              let fun branch k x left right =
                      case compare(key, k) of
                         EQUAL   => let val r = loop left (b,SOME (from_repr k,x),a)
                                    in loop right r end
                       | LESS    => loop left (b,m,SOME (from_repr k, x))
                       | GREATER => loop right (SOME (from_repr k, x),m,a)
              in case tree 
                   of  LEAF                      => (b,m,a)
                    | (RED(k, x, left, right))   => branch k x left right
                    | (BLACK(k, x, left, right)) => branch k x left right
              end
      in loop tree (NONE,NONE,NONE)
      end

  fun gle p =
     case bounds p
       of (SOME e,NONE,_) => SOME e
        | (_,SOME e,_) => SOME e
        | _ => NONE

  fun glt p =
     case bounds p
       of (SOME e,_,_) => SOME e
        | _ => NONE

  fun lge p =
     case bounds p
       of (_,NONE,SOME e) => SOME e
        | (_,SOME e,_) => SOME e
        | _ => NONE

  fun lgt p =
     case bounds p
       of (_,_,SOME e) => SOME e
        | _ => NONE

  fun peek (set, key) = SOME(find(set, key)) 
                        handle NotFound => NONE

  fun lbalance z zd (RED(y,yd,RED(x,xd,a,b),c)) d =
      RED(y,yd,BLACK(x,xd,a,b),BLACK(z,zd,c,d))
    | lbalance z zd (RED(x,xd,a,RED(y,yd,b,c))) d =
      RED(y,yd,BLACK(x,xd,a,b),BLACK(z,zd,c,d))
    | lbalance k x left right = BLACK(k, x, left, right)
                              
  fun rbalance x xd a (RED(y,yd,b,RED(z,zd,c,d))) =
      RED(y,yd,BLACK(x,xd,a,b),BLACK(z,zd,c,d))
    | rbalance x xd a (RED(z,zd,RED(y,yd,b,c),d)) =
      RED(y,yd,BLACK(x,xd,a,b),BLACK(z,zd,c,d))
    | rbalance k x left right = BLACK(k, x, left, right)
                        
  exception GETOUT  

  fun insert (set as (tree, n), key', data) =
      let val key = to_repr key'
          val addone = ref true
          fun ins LEAF = RED(key,data,LEAF,LEAF)
	    | ins (BLACK(k,x,left,right)) =
              (case compare(key, k) of
                   LESS    => lbalance k x (ins left) right
                 | GREATER => rbalance k x left (ins right)
                 | EQUAL   => (addone := false; BLACK(key, data, left, right)))
	    | ins (RED(k, x,left,right)) =
              (case compare(key, k) of
                   LESS    => RED(k, x, (ins left), right)
                 | GREATER => RED(k, x, left, (ins right))
                 | EQUAL   => (addone := false; RED(key, data, left, right)))
      in  (case ins tree of
                RED x => BLACK x
              | tree  => tree          
          , if !addone then n+1 else n) end

  fun push LEAF stack = stack
    | push tree stack = tree :: stack

  fun pushNode left k x right stack = 
      left :: (BLACK(k, x, LEAF, LEAF) :: (push right stack))

  fun getMin []             some none = none
    | getMin (tree :: rest) some none = 
      case tree of
          LEAF                 => getMin rest some none
        | RED  (k, x, LEAF, b) => some k x (push b rest)
        | BLACK(k, x, LEAF, b) => some k x (push b rest)
        | RED  (k, x, a, b)    => getMin(pushNode a k x b rest) some none
        | BLACK(k, x, a, b)    => getMin(pushNode a k x b rest) some none

  fun getMax []             some none = none
    | getMax (tree :: rest) some none = 
      case tree of
          LEAF                 => getMax rest some none
        | RED  (k, x, a, LEAF) => some k x (push a rest)
        | BLACK(k, x, a, LEAF) => some k x (push a rest)
        | RED  (k, x, a, b)    => getMax(pushNode b k x a rest) some none
        | BLACK(k, x, a, b)    => getMax(pushNode b k x a rest) some none

  fun fold get f e (tree, n) =
      let fun loop stack acc =
              get stack (fn k =>fn x =>fn stack => loop stack (f(from_repr k,x,acc))) acc
      in  loop [tree] e end

  fun foldl f = fold getMin f

  fun foldr f = fold getMax f

  fun listItems set = foldr (fn(k,x,res) => (k,x)::res) [] set

  fun appAll get f (tree, n) =
      let fun loop stack = get stack (fn k => fn x => (f(from_repr k,x); loop)) ()
      in  loop [tree] end

  fun app f = appAll getMin f

  fun revapp f = appAll getMax f

  (* remove a la Stefan M. Kahrs *)
  fun redden (BLACK arg) = RED arg
    | redden _ = raise Fail "Redblackmap.redden: impossible" 

  fun balleft y yd (RED(x,xd,a,b)) c                = 
      RED(y, yd, BLACK(x, xd, a, b), c)
    | balleft x xd bl (BLACK(y, yd, a, b))          = 
      rbalance x xd bl (RED(y, yd, a, b))
    | balleft x xd bl (RED(z,zd,BLACK(y,yd,a,b),c)) = 
      RED(y, yd, BLACK(x, xd, bl, a), rbalance z zd b (redden c))
    | balleft _ _ _ _ = raise Fail "Redblackmap.balleft: impossible"  

  fun balright x xd a             (RED(y, yd ,b,c)) = 
      RED(x, xd, a, BLACK(y, yd, b, c))
    | balright y yd (BLACK(x,xd,a,b))          br = 
      lbalance y yd (RED(x,xd,a,b)) br
    | balright z zd (RED(x,xd,a,BLACK(y,yd,b,c))) br = 
      RED(y, yd, lbalance x xd (redden a) b, BLACK(z, zd, c, br))
    | balright _ _ _ _ = raise Fail "Redblackmap.balright: impossible" 


  (* [append left right] constructs a new tree t.
  PRECONDITIONS: RB left /\ RB right 
              /\ !e in left => !x in right e < x
  POSTCONDITION: not (RB t)
  *)
  fun append LEAF right                    = right
    | append left LEAF                     = left
    | append (RED(x,xd,a,b)) (RED(y,yd,c,d))     = 
      (case append b c of
	   RED(z, zd, b, c) => RED(z, zd, RED(x, xd, a, b), RED(y, yd, c, d))
         | bc           => RED(x, xd, a, RED(y, yd, bc, d)))
    | append a (RED(x,xd,b,c))                = RED(x, xd, append a b, c)
    | append (RED(x,xd,a,b)) c                = RED(x, xd, a, append b c) 
    | append (BLACK(x,xd,a,b)) (BLACK(y,yd,c,d)) = 
      (case append b c of
	   RED(z, zd, b, c) => RED(z, zd, BLACK(x,xd,a,b), BLACK(y,yd,c,d))
         | bc           => balleft x xd a (BLACK(y, yd, bc, d)))
   
  fun remove ((tree, n), key') =
      let val key = to_repr key'
          fun delShared k x a b =
              case compare(key, k) of
                  EQUAL   => (x, append a b)
                | LESS    => 
                  let val (res, a') = del a
                  in  (res, case a of
                                BLACK _ => balleft k x a' b
                              | _       => RED(k, x, a', b)) end
                | GREATER => 
                  let val (res, b') = del b
                  in  (res, case b of
                                BLACK _ => balright k x a b'
                              | _       => RED(k, x, a, b')) end  
          and del LEAF                = raise NotFound
            | del (RED(k, x, a, b))   = delShared k x a b
            | del (BLACK(k, x, a, b)) = delShared k x a b

          val (res, tree) = case del tree of
                                (res, RED arg) => (res, BLACK arg)
                              | x              => x
      in  ((tree, n-1), res) end

  fun map f (tree, n) = 
      let fun loop LEAF = LEAF
            | loop (RED(k,x,a,b)) = 
              let val a = loop a
                  val x = f(from_repr k,x)
              in  RED(k,x,a, loop b) end
            | loop (BLACK(k,x,a,b)) = 
              let val a = loop a
                  val x = f(from_repr k,x)
              in  BLACK(k,x,a, loop b) end
      in  (loop tree, n) end

  fun transform f (tree, n) = 
      let fun loop LEAF = LEAF
            | loop (RED(k,x,a,b)) = 
              let val a = loop a
              in  RED(k, f x, a, loop b) end
            | loop (BLACK(k,x,a,b)) = 
              let val a = loop a
              in  BLACK(k, f x, a, loop b) end
      in  (loop tree, n) end
end

(*

val prb = mkDict();
val prb = insert (prb,12,"twelve");
val prb = insert (prb,13,"twelveplusone");
val l1 = listItems prb;
val prb = insert (prb,42,"forty-two");
val l1b = listItems prb;

val prb2 = mkDict();
val prb2 = insert (prb2,(1,2),"twelve");
val prb2 = insert (prb2,(1,3),"thirteen");
val prb2 = insert (prb2,(1,4),"fourteen");
val prb2 = insert (prb2,(4,2),"forty-two");
val l2 = listItems prb2;

datatype flags =
   FLAG_ONE
 | FLAG_ANOTHER
 | FLAG_AND_ANOTHER

val flagVals =
   #[(FLAG_ONE,0wx2),
     (FLAG_ANOTHER,0wx4),
     (FLAG_AND_ANOTHER,0wx8)];

val (prb3,prb3a) =
       Vector.foldr
         (fn ((f,v),(a1,a2)) =>
              (insert (a1,f,v),
               insert (a2,v,f)))
         (mkDict(),mkDict())
         flagVals;

val (l3,l3a) =
  (listItems prb3,
   listItems prb3a);



*)
