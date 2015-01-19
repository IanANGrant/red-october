(* An ML interface for representations of compound structures (values)
   as arrays of octets. These octets could be the values of elements
   of Word8Arrays, or of MappedWord8Arrays: arrays in off-heap
   (i.e. not GCed) buffers allocated by malloc or mmap.

   The sorts of values that we want to represent are things such as
   arrays, structures and unions of the kind that are used in C
   language system and application interfaces.

   The interface should allow components of the represented structures
   to be independently accessed or updated, and of course the results
   of those operations should reflect, or be reflected in, the
   underlying concrete representation.

   There are two aspects to the interface this unit provides. One is a
   purely applicative part, which provides functions to encode and
   decode values, and the other is the imperative interface which
   represents the values as mutable objects with concrete underlying
   representations. The applicative interface presents encoded values
   as unstructured arrays. The imperative interface presents encoded
   values as a hierarchical structure, with separately
   addressable/updateable parts. It is the imperative interface,
   needless to say, which presents most of the difficulty. In the
   specification that follows, we will largely ignore the applicative
   implementation; the assumption being that the applicative interface
   can be implemented simply as one instance of the imperative one,
   and a simple instance at that.

   The underlying representation will need to be some specific
   encoding of the represented value. For example it may be as a word
   of some fixed size, in bigendian or littleendian order, or it may
   be some sort of encoding such as the Ocaml runtime encoding of a
   word w as the machine word 2*w+1.  Not all encodings are fixed
   length. For example an ISO/IEC BER encoded word may need one or
   more octets to represent, depending on the particular value being
   represented.

   This unit should contain just the mechanisms for managing
   collections of represented values, and providing methods to access
   and update their constituent parts. In normal circumstances this
   would merely be a matter of memory management, and of contiguous
   regions of memory at that. But here we will allow the underlying
   representation of a compound structure in terms of arrays of octets
   to be in non-contiguous locations. So for example a single IP
   packet could be represented by this unit as one whole structure,
   even if its concrete representation happens to have been split into
   several fragments each in a different ethernet frame, each of which
   is stored in a separate buffer. This interface will allow us to
   validate the IP packet, rewrite part or parts of the header and
   retransmit it, perhaps on a different network interface, and to do
   this all in-place, i.e. it will be a so-called zero-copy
   implementation.

   We will use an intermediate abstraction we call an "array patch" to
   hide the details. This is a generalisation of the idea of a
   functional array, where we present a single uniform array, but
   allow the underlying representation to be an arbitrarily complex
   patchwork of non-contiguous regions, each of which is a slice of an
   actual array. To be able to accommodate variable length encodings
   the interface will need to allow an arbitrary contiguous slice of
   octets to be replaced by another slice, the length of which could be
   greater than or less than that of the slice it is replacing.

   To present the represented values as ML datatypes we use the
   polymorphic value introspection interface provided by the Values
   unit. To make this work, we need to be able to define a mapping
   between the abstract representation of values which the
   introspection interface presents and the concrete representation as
   arrays of octets. This mapping will, for example, specify the
   encoding of a particular integer as being 32 bit, machine order,
   and perhaps that of another integer in the same compound value as
   being encoded as a 16 bit integer in network byte order.

   This mapping is non-trivial in general. For example, it may need to
   represent elements in a different order from the one in which they
   appear in the abstract representation. Say we have an ML record
   {one=1, two=2, three=3}, which in Moscow ML is represented as a
   3-tuple, but where the components are in ascending lexicographic
   order of the labels. The corresponding representation as a tuple is
   #[1,3,2], because in the lexicographical ordering "three" < "two".

   We need to be able to make these units work with other MLs, and
   indeed other languages. One way to do this is to specify the
   processes which translate from/to abstract values to/from concrete
   representations as mini-language-interpreters. The languages being
   abstract syntax representations of primitive constructors and
   deconstructors. The abstract syntax representations would be
   modelled in Standard ML by abstract datatypes. The process of
   translating a general polymorphic type 'a to and from an abstract
   syntax representation could be specified in a type-safe manner
   using type-directed partial evaluation. Then the process of
   translating from any one abstract syntax representation to any
   other, i.e. the process of translating mini-languages from one to
   another, could be specified purely formally by using a
   general-purpose term-rewriting language.

   We need to be able to represent bit-fields, the lengths of which
   will not necessarily be an integral multiple of 8. We place a
   restriction on bit-fields, which dictates that they must always
   appear in contiguous groups, the total length of which is, in bits,
   a multiple of 8. This will allow us to express, e.g. all uses of
   bit-fields in C structures, and all uses in current ISO/IEC
   transfer encodings. This covers nearly all applications of
   bitfields without making it necessary to support sub-octet
   allocation in the underlying registers.  Bit-fields will be
   implemented by a unit at a level above this one, which will
   represent a group of bit-fields by a single register of an integer
   number of octets, and allow their values to be referenced and
   updated independently etc.

   Therefore the mapping needs to be specified to this unit as a
   functional interface which provides standard methods to get/set
   values and to determine the required number of octets in the
   underlying representation. Consequently, we need a neutral
   representation which is independent of the ML type of the abstract
   object represented, and also independent of the type of the
   underlying octet arrays (Word8Array, MappedWord8Array etc.) which
   carry the encoded concrete representation.

   One possible neutral representation is the Standard Basis
   Word8Vector type. But these are immutable, so references and
   updates would involve allocating and garbage collecting
   values. Instead, we will provide an abstract interface by taking
   higher-order functions for the reference and update specifying the
   mapping, and we will apply these higher-order functions to the
   underlying array primitives, effectively specialising the mapping
   to whatever is the type of octet array which contains the
   underlying concrete representation. Then the reference/update
   functions will operate directly on the underlying representation,
   but still remain independent of whichever particular one we choose.

   Another requirement we have is that represented values which are
   components of compound structures can be referenced and updated
   independently of each other. For example, we might have a compound
   structure in a mapped array which is asynchronously updated by an
   interrupt or signal handler, and we may be need to repeatedly and
   frequently inspect one particular component of that structure in a
   loop. Obviously, we want to be able to do this without decoding the
   entire structure, most of which is irrelevant. Yet on other
   occasions we will need to reference or update the entire structure
   as a single compound value, an ML record, say, representing all the
   components.

   To abstract this, we can use a generic representation of compound
   values as vectors of "registers". The only attributes an individual
   register has are its length in octets, its underlying container and
   its value. The length is fixed and can only be referenced, the
   underlying container can be both updated and referenced, and the
   value can be both updated and referenced. Then, outside this unit,
   we can add another layer interpreting registers as being
   representations of specific ML types like words, pointers
   etc. Within this unit, then, compound values are treated as vectors
   of registers and this unit will provide functions which construct
   these vectors according to a specification, and which deconstruct
   them, providing access to the individual registers representing the
   component values.

   The higher-level unit which presents the registers as having
   particular ML types can be implemented using type-directed partial
   evaluation, by 'scanning' values from their representation as
   registers, and 'printing' values into the registers. This will
   allow one register to be presented at different ML types. For
   example, we might at one point need a 32 bit word to be presented
   as a word, and at another point as a list of flags, each of which
   represents one of the bits in that word which are set. In either
   case however, the underlying representation in this unit of the
   word value as a register is the same.

   We also need to consider how to construct representations. In some
   cases, we will be presented with ML values and we will need to
   allocate new arrays of octets to contain the representations, in
   other cases we will be presented with the underlying
   representations "ready made". These two methods of construction
   also correspond to scanning and printing, but here we are scanning
   and printing from/into the underlying arrays of octets rather than
   the individual registers. But since registers are just slices of
   the underlying arrays of octets, the operations are in fact the
   same in either case. This suggests then that we should be using the
   same abstract ML types to represent both. Then we can consider any
   compound structure as a single register when necessary, simply by
   taking its underlying representation. We need this anyway, for the
   cases where the elements of compound structures are themselves
   compound structures. So, that's settled then: we'll store vectors
   of registers in bigger registers, and then, to get off the ground
   we need only a primitive register which is stored in an array of
   octets. And then this collapses onto the underlying patch
   representation of registers: a patch is just an array of registers.

   We need to be able to allocate space in the underlying arrays, for
   example, when updating a value in a variable-length encoding, the
   updated value may need more or less space than the old
   value. Therefore we need an underlying unit providing a managed
   allocation interface to pools of space. The memory management
   should be concealed beneath the aforementioned patch interface, so
   that it is transparent to this unit. The only information that this
   interface requires is that of the underlying "container structure"
   mentioned above. The container structure will be just a list of
   addresses and lengths of the contiguous octets representing the
   value at that time. In the case of applicative structures on the ML
   heap the addresses will be liable to change. It is the case of
   applicative structures represented in off-heap/system mapped memory
   that the addresses are likely to be useful.

   Another underlying allocation model we will need is one which can
   be used by a "scanner" which constructs an abstract value from a
   sequence of octets. In the example we gave above of a zero-copy
   packet rewriter, we need the scanner to simultaneously construct
   the underlying container as it constructs the value, so that we can
   alter the contents of the packet fragments in-place. Therefore we
   need a memory manager which will allocate octets as required in
   strict sequential order, from a pre-existing array (parts of each
   of the ethernet frame buffers, in the running example.)

   An obvious generalisation is in the opposite direction: how we
   represent the structures.  All we are doing in computing, _really,_
   is composing constructors and deconstructors in various more or
   less complicated ways.  Thus far we've been assuming that the aim
   is a Standard ML module with functions which provide an API for
   Standard ML programs to use. But there are other possible targets.
   For example, we could compose assembler code to do the necessary
   deconstruction and construction of IP packets as described above,
   and then JIT compile it into a running OS kernel module
   implementing a packet-filter-cum-routing engine. Composing machine
   code, or even C for that matter, will not be much more difficult
   than composing ML functions. The key is to use intentional
   semantics. So we use different mini-languages, and different sets
   of rewrites. Instead of representing values, these mini-languages
   allow us to represent methods of deconstructing and constructing
   values.

   Complicated, isn't it? I don't think, however, that the complexity
   is purely a result of anything we've introduced here, I think it's
   just that the complexities of representation are something we're
   very, very practiced at "seeing through".  Like using a pen or a
   pencil, one tends to forget how long it took to learn to do, and if
   pressed for an explanation "How do you write words with a pen?" it
   becomes clear that we don't actually know what it is we are doing
   when we write with a pen. So the act of writing is inherently very
   complicated, but we don't consciously experience that complexity
   until pressed to describe it.


 *)
