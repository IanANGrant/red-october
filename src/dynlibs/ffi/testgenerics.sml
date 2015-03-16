val _ = load "GenericWord8ArraySlice";
val _ = load "GenericMappedWord8ArraySlice";

val arr1 = GenericMappedWord8Array.array (256, 0w0);
val arr2 = GenericWord8Array.array (256, 0w0);

val _ = GenericMappedWord8Array.update (arr1, 0, 0w1);
val _ = GenericWord8Array.update (arr2, 0, 0w2);

val v1 = GenericMappedWord8Array.sub (arr1, 0);
val v2 = GenericMappedWord8Array.sub (arr2, 0);

val v1' = GenericWord8Array.sub (arr1, 0);
val v2' = GenericWord8Array.sub (arr2, 0);

val s1 =  GenericWord8ArraySlice.full arr1;
val s2 =  GenericWord8ArraySlice.full arr2;

val v1s' = GenericWord8ArraySlice.sub (s1, 0);
val v2s' = GenericWord8ArraySlice.sub (s2, 0);

val s1' =  GenericMappedWord8ArraySlice.full arr1;
val s2' =  GenericMappedWord8ArraySlice.full arr2;

val v1s'' = GenericMappedWord8ArraySlice.sub (s1', 0);
val v2s'' = GenericMappedWord8ArraySlice.sub (s2', 0);

val v1s''' = GenericWord8ArraySlice.sub (s1', 0);
val v2s''' = GenericWord8ArraySlice.sub (s2', 0);

(* So the concrete representations are very different, because
MappedWord8Arrays are off-heap externally (to ML) owned blocks
of memory, and the ordinary Word8Arrays are on the ML heap (GC'ed
etc.), but they have the same type when cast to GenericArrays, so we
can mix them, and their slices, and use them wherever we would have
used ordinary, specific, concrete instantiations of the signature.

So we can pile these abstractions on top of one another and build up
arbitrarily complex representations. For example, we could define a
type of array that permutes the byte values and/or addresses via a
lookup table, and uses another arbitrary underlying array
representation as storage. This might split the buffer into several
pages and shuffle them, or it might store them on a disk, or transmit
them across a network, or keep them in a FIFO, and so on and so forth,
_ad nausiem_. Or we could use a memory mapped file as a buffer and
make an array representation which automatically re-mapped the buffer
to follow a _cursor_ around the file. Then we could just address the
bytes in the file as simple array elements, and all the underlying
technical stuff will be nicely containied and managed. The buffer
might just as well be a memory mapped FIFO or a database "large
object" file handle, so that we could use PostgreSQL as a persistent
object store.

It's taken me seven years to figure this one out. So, I'm slow. Mind
you, if it had been explained in a BOOK, or a PAPER, then perhaps I
would have got here a _little_ quicker. And no, the journey has _not_
been _fun._ Interesting, maybe, but I don't know anyone (and I
wouldn't really want to know any such person) who would call this
fun.  *)
