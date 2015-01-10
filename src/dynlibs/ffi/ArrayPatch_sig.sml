signature ArrayPatch =
sig
   type elem
   type patch
   type slice
   type vector
   eqtype array

   val fromArray : array -> patch
   val fromSlice : slice -> patch
   val fromList : slice list -> patch
   val concat : patch list -> patch
   val tabulate : int * (int -> elem) -> patch
   val length : patch -> int
   val slices : patch -> slice list
   val array : patch -> array
   val vector : patch -> vector
   val slice : patch * int * int option -> slice
   val patch : patch * patch * int -> patch
   val subpatch : patch * int * int option -> patch
   val superpatch : patch * patch * int -> patch


   val update   : patch * int * elem  -> unit
   val appi     : (int * elem -> unit) -> patch -> unit
   val app      : (elem -> unit) -> patch -> unit
   val foldl    : (elem * 'b -> 'b) -> 'b -> patch -> 'b
   val foldr    : (elem * 'b -> 'b) -> 'b -> patch -> 'b   
   val sub      : patch * int -> elem

end

(* A patch is the opposite of a slice in the sense that you patch
   slices together to make another whole thing, albeit a patchwork
   thing. 
   
   This has a very similar interface to the underlying ArraySlice
   unit, but with two extra operations corresponding to the duals of
   slice and subslice. The first of these duals is called 'patch' and
   the other is _not_ called 'subpatch'.

   [fromArray a] creates a new patch consisting of the single array
   slice which is the whole of a.

   [fromSlice s] creates a new patch consisting of the single array
   slice s.

   [fromList l] creates a new patch consisting of the concatenation of
   the array slices in l. The slices are patched together in the order
   in which they occur in the list.

   [concat l] creates a new patch consisting of the concatenation of
   the patches in l. The component slices of the patches are patched
   together in the order in which they occur in the patches within the
   containing list.

   [tabulate (n,f)] creates a "lazy patch" consisting of a single
   slice of a tabulated array. The array so 'created' is not actually
   allocated: rather its values are computed as and when they are
   referenced. This can be used to effect "sparse arrays" by patching
   the "holes" with a suitable tabulation. And by referencing another
   patch or array, the tabulate function f can effect a copy-on-write
   patch.

   [slices p offs len] returns a list of the contiguous subslices
   of the array slices underlying the part of patch p beginning at
   offset offs, and ending at offs + len. The list is in the natural
   order for sequential access of the elements indexed from 0 to len.

   [array p]

   [vector p]

   These operations make a new (and in each case contiguous) array (or
   vector) covering the whole patch p.

   [slice p offs (SOME len)] will construct a new array and copy the
   contents of the specified part of the patch into it, returning a
   contiguous array slice which covers the new array.

   [patch p q offs] replaces the parts of one or more slices, starting
   at the offset offs into the patch p, with the slices from patch
   q. If length q > length p + offs then the length of the patch is
   increased accordingly. If offset > len p then a Subscript exception
   is raised.

   [subpatch p offs (SOME len)] gives the moral equivalent of a
   subslice of a patch which is a patch of length len taken from part
   of the larger patch p, beginning at offset offs from the start of
   p. To get a contiguous array slice from a patch, use slice.

   The above operations are all well-founded and rather boring.
   Fortunately we have:

   [superpatch p q offs] patches the patch q "over" the existing
   patches (if any) of p, starting at the offset offs from the start
   of p. Because patches are effectively references, it is possible to
   create weird and interesting loops. In the presence of superpatches,
   none of the above operations are necessarily well-founded.

   This could be used to make a wave-table synthesizer, for example.  A
   well-known trick with such a thing is to make an ever-rising scale
   by amplitude modulating the components of a chord. This is a sort
   of time-domain analogue of Escher's ever-ascending staircase.

   More prosaically, it could abstract the details of managing a ring
   buffer, or a FIFO, which could be made dynamically extensible.

   But the musical applications are much more interesting. One might
   use this to program rhythms on a midi channel, and simultaneously
   control stage lighting effects.

   Or one might try to use this for some Blixer Bargeld style
   audio-sample looping effects, perhaps. Make an audio input
   ring-buffer and connect it by a 'pedal' to an audio output buffer,
   also a loop. When the peddle is actuated, take a new sample from
   the input buffer and overlay it on the output loop so that the
   period of the new loop is some fraction or multiple of the existing
   loops. Use primitives such as doubling and halving the length of
   the loops to put down "layers of rhythm"

   Or in graphics, one could use this structure to produce rhythmic
   patterns in the space domain. Obvious examples would be Lissajous'
   and those "spiro-graph" type patterns, but one could make them much
   more interesting by rhythmically varying the colour and line-styles
   "at the same time". Or in three-dimensions, Euler's spherical
   harmonics which appear in the Quantum Mechanics of simple
   molecules.

   There are arithmetic applications too. One could use this to
   implement a prime seive, or to enumerate permutations, or to
   automatically generate frequency distributions from uniform binary
   distributions U[0,1].

   Another number-theoretical application would be to explore the
   Chinese Remainder theorem relating the solutions of simultaneous
   equations in modulo arithmetic.

   And in "higher" mathematics, there are sure to be applications to
   Fourier transforms and integration of functions like the heat
   equation and simple harmonic motion. These will lead via the "nth
   roots of unity" and FFT, back into to number theory (integer
   multiplication represented as convolution in the frequency domain)
   and then back to digital electronics: the address bus of a digital
   computer is effectively a harmonic series. There are sure to be
   applications of these ideas to "programmable bus logic" whereby one
   could program algrithms so that they are done in a "non-local"
   way. For example, it may be possible to implement some interesting
   signal-processing functions (such as wave-table synthesis!) in
   terms of a sum-and-multiply primitive which is carried out using
   something like DMA, whereby the CPU is relegated to programming the
   bus logic and initiating the operations, which are then effected by
   simply sequentially addressing the results.

   Perhaps we are making a big deal of it. Nevertheless, if even half
   of this were true, do you know of any another single function that
   can do more? *)
