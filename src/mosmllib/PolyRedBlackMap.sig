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

   [mkDict ordr] returns a new, empty map whose keys have ordering
   ordr.

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
   key-value pair less than k, k and least key-value pair greater than
   k; when any of these do not exist, NONE is returned.

   [partition (m, k)] returns a triple (bs,SOME (k,v),as) if m maps k
   to v or (bs,NONE,as) otherwise; where bs (as) are the key-value
   pairs which are predecessors (successors) of k. The bs are (lower
   bounds) in descending order, the as are (upper bounds) in ascending
   order.
   
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
