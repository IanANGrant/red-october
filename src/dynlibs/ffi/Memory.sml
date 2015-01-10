Rethinking the static buffer.

There are several problems:

   1. The code is a pile of utter crap (I should know, because I wrote it.)

   2. Static buffers are allocated piecemeal by relatively expensive system calls.

   3. The finalization mechanism is too restrictive. It could not
      accommodate, e.g. munmap calls, never mind the associated
      shm_unlink to release a shared memory object.

   4. The Caml runtime GC (this version, at any rate) does not have
      any mechanisms to incorporate external memory management regimes.

These could all be addressed by using a hierarchical system of
finalizing allocated blocks. For example, one might have, at the
lowest level, 1 say, a system shared memory handle. This would be
finalized, as far as any one process is concerned, once all the level
2 mmap/mprotect mappings of that shared memory object (by that
process) have been finalized. Each of the level 2 mmap mappings would
only be finalized when all of the allocated blocks (level 3) of that
mapping have been finalized. Some of these could be reference-counted
allocations, others could be along the lines of a conservative GC, and
yet others might be managed by a slab allocation scheme, for example.
A fourth level of memory management might be used when a memory
manager requires an underlying memory manager, which would be in some
sense significantly simpler, one would hope,

To implement this, we could define a class of memory manager which
exposed a certain API peculiar to its "management style": by reference
counting, heap-tracing, malloc/free, or another GC regime.

