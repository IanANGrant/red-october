(* Redblackmap --                                                   *) 
(*    applicative maps implemented by Okasaki-style Red-Black trees *)
(* Ken Friis Larsen <ken@friislarsen.net>                           *)
(* Polymorphic mods by Ian Grant                                    *)

structure PolyRedBlackMap :> PolyRedBlackMap  =
struct

  (* These should be functor parameters *)

  val to_repr : 'a -> ObjRepr.sym = ObjRepr.valAbsRepr
  val from_repr : ObjRepr.sym -> 'a = ObjRepr.absReprVal
  val compare = ObjRepr.compare_abs

   (* datatype colour =
         RED
       | BLACK
      datatype ('a,'b) tree =
         LEAF
       | BRANCH of colour * ObjRepr.sym * 'b * ('a, 'b) tree * ('a, 'b) tree

   *)

  datatype ('a, 'b) tree = 
           LEAF
         | RED   of ObjRepr.sym * 'b * ('a, 'b) tree * ('a, 'b) tree
         | BLACK of ObjRepr.sym * 'b * ('a, 'b) tree * ('a, 'b) tree

  type ('a, 'b) dict  =  ('a, 'b) tree * int

  exception NotFound

  fun mkDict () = (LEAF, 0)

  fun numItems (_, n) = n

  fun find ((tree, n), key') =
      let val key = to_repr key'
          fun loopShared k x left right = (* If colour were a value rather than the
                                             rectype constructor this would be one fn *)
              case compare(key, k) of
                  EQUAL   => x
                | LESS    => loop left
                | GREATER => loop right
          and loop LEAF                       = raise NotFound
            | loop (RED(k, x, left, right))   = loopShared k x left right
            | loop (BLACK(k, x, left, right)) = loopShared k x left right
      in loop tree end

  fun bounds ((tree, n), key') =
      let val key = to_repr key'
          fun loop tree (b,m,a) = 
              let fun branch k x left right =
                      case compare(key, k) of
                         EQUAL   => let val r = loop left (b,SOME (from_repr k,x),a)
                                    in loop right r end
                       | LESS    => loop left (b,m,SOME (from_repr k, x))
                       | GREATER => loop right (SOME (from_repr k, x),m,a)
              in case tree  (* ... and so would this *)
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
   
  fun remove ((tree, n), key') = (* Convert to cps-style to get tail-recursive version? *)
      let val key = to_repr key'
          fun delShared k x a b =
              case compare(key, k) of
                  EQUAL   => (x, append a b)
                | LESS    => 
                  let val (res, a') = del a
                  in  (res, case a of (* Pass this (fn b' => case ... ) in as the continuation thunk? *)
                                BLACK _ => balleft k x a' b
                              | _       => RED(k, x, a', b)) end
                | GREATER => 
                  let val (res, b') = del b
                  in  (res, case b of
                                BLACK _ => balright k x a b'
                              | _       => RED(k, x, a, b')) end  
          and del LEAF                = raise NotFound (* With the colour field this would 
                                                          be one tail-rec loop *)
            | del (RED(k, x, a, b))   = delShared k x a b
            | del (BLACK(k, x, a, b)) = delShared k x a b

          val (res, tree) = case del tree of
                                (res, RED arg) => (res, BLACK arg)
                              | x              => x
      in  ((tree, n-1), res) end

   (* Motivation is to defunctionalize it a la Reynolds,
      and then run it as a set of co-processes to get a
      distributed database. See DefInt.sml for details. *)

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
load "Redblackmap";

fun updfn (i,(m,j)) =
      (Redblackmap.insert
        (m,i,String.str (Char.chr (Char.ord #"q" + j))),j+1)
val t1' = Redblackmap.mkDict(Int.compare);
val (t1,_) = List.foldl updfn (t1',0) [2,3,5,7,11,13,17];
val t1l = Redblackmap.listItems(t1);
val t1rs = List.map (fn n => Redblackmap.partition (t1,n)) [1,6,7,8,19];
local open Redblackmap in
   val t2rs =
         List.map
           (fn (tfn,nm) =>
             (nm, List.map
                    (fn n => (n,tfn (t1,n)))
                    [1,6,7,8,19]))
           [(gle,"gle"),(glt,"glt"),(lge,"lge"),(lgt,"lgt")];
end
val t3rs = List.map (fn n => Redblackmap.bounds (t1,n)) [1,6,7,8,19];

(* So it must work, then. This is *functional* programming, you know. *)

*)
