(* A patch is the opposite of a slice in the sense that you patch
   slices together to make another whole thing, albeit a patchwork
   thing. 
   
   This has a very similar interface to the ArraySlice unit, but with
   two extra operations corresponding to the duals of slice and
   subslice: they are patch and one other:

   [fromArray a] creates a new patch consisting of the single array
   slice which is the whole of a.

   [fromSlice s] creates a new patch consisting of the single array
   slice s.

   [patch p s offs] patches an array slice s over the existing patches
   in p, starting at the offset offs.

   [subslices p offs len] will return a list of the contiguous
   subslices of the array slices underlying the part of patch p
   beginning at offset offs, of length len. The list is in the natural
   order for sequential access of the elements indexed from 0 to len.

   The [array p] and [vector p] operations make a new (and in each case
   contiguous) array (or vector) of the whole patch p.

   [slice p offs len] will construct a new array and copy the contents
   of the specified part of the patch into it, returning a contiguous
   array slice which is whole of the new array.

   [subpatch p offs len] gives the moral equivalent of a subslice of a
   patch of length len taken from part of a larger patch p beginning
   at offset p. To get a contiguous array slice from a patch, use
   slice.

   The above operations are all well-founded and rather
   boring. Fortunately we have:

   [superpatch p s offs] patches the patch s "over" the existing
   patches (if any) of p, starting at the offset offs. Because patches
   and slices are effectively references, it is possible to create
   weird and interesting loops. In the presence of superpatches, none
   of the above operations are necessarily well-founded.

   One might try to use this for some Blixer Bargeld style
   audio-sample looping effects, perhaps.

   More prosaically, it could abstract the details of managing a ring
   buffer, or a FIFO, which could be made dynamically extensible.

   But the musical applications are much more interesting. Try using
   this to program rhythms on a midi channel, and simultaneously
   control stage lighting effects.

   You could use it to make a wave-table synthesizer, for example.  A
   well-known trick is amplitude modulating the components of a chord
   to make an ever rising scale. This is a time-domain analogue of
   Escher's ever-ascending staircase.

   In graphics, one could use this structure to produce rhythmic
   patterns in the space domain. Obvious examples would be Lissajous'
   and those "spiro-graph" type patterns, but one could make them much
   more interesting by rhythmically varying the colour and line-styles
   "at the same time". Or in three-dimensions, Euler's spherical
   harmonics, and the Quantum mechanics of simple molecules.

   There are arithmetic applications too. One could use this to
   implement a prime seive, or to enumerate permutations, or to
   automatically generate frequency distributions from uniform binary
   distributions U[0,1].

   And in "higher" mathematics, there are sure to be applications to
   Fourier transforms and integration of functions like the heat
   equation and simple harmonic motion.

   Perhaps we are making a big deal of it. Nevertheless, if even half
   of this were true, do you know of any another single function that
   can do more? *)

