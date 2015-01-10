(* Redblackmap -- applicative maps as Red-black trees *)
signature Redblackmap =
sig

type ('key, 'a) dict

type ('key, 'a) dictRepr

exception NotFound

val toRepr : ('key, 'a) dict -> ('key, 'a) dictRepr
val fromRepr : ('key * 'key -> order) -> ('key, 'a) dictRepr -> ('key, 'a) dict

val mkDict    : ('key * 'key -> order) -> ('key, 'a) dict
val insert    : ('key, 'a) dict * 'key * 'a -> ('key, 'a) dict
val find      : ('key, 'a) dict * 'key -> 'a
val bounds    : ('key, 'a) dict * 'key -> ('key * 'a) option * ('key * 'a) option * ('key * 'a) option
val gle       : ('key, 'a) dict * 'key -> ('key * 'a) option
val glt       : ('key, 'a) dict * 'key -> ('key * 'a) option
val lge       : ('key, 'a) dict * 'key -> ('key * 'a) option
val lgt       : ('key, 'a) dict * 'key -> ('key * 'a) option
val partition : ('key, 'a) dict * 'key -> ('key * 'a) list * ('key * 'a) option * ('key * 'a) list
val peek      : ('key, 'a) dict * 'key -> 'a option
val remove    : ('key, 'a) dict * 'key -> ('key, 'a) dict * 'a
val numItems  : ('key, 'a) dict -> int
val listItems : ('key, 'a) dict -> ('key * 'a) list
val app       : ('key * 'a -> unit) -> ('key,'a) dict -> unit
val revapp    : ('key * 'a -> unit) -> ('key,'a) dict -> unit
val foldr     : ('key * 'a * 'b -> 'b)-> 'b -> ('key,'a) dict -> 'b
val foldl     : ('key * 'a * 'b -> 'b) -> 'b -> ('key,'a) dict -> 'b
val map       : ('key * 'a -> 'b) -> ('key,'a) dict -> ('key, 'b) dict
val transform : ('a -> 'b) -> ('key,'a) dict -> ('key, 'b) dict
end

(* 
   [('key, 'a) dict] is the type of applicative maps from domain type
   'key to range type 'a, or equivalently, applicative dictionaries
   with keys of type 'key and values of type 'a.  They are implemented
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
