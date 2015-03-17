/*
 * Cryptographic API.
 *
 * SHA-256, as specified in
 * http://csrc.nist.gov/groups/STM/cavp/documents/shs/sha256-384-512.pdf
 *
 * SHA-256 code by Jean-Luc Cooke <jlcooke@certainkey.com>.
 *
 * Copyright (c) Jean-Luc Cooke <jlcooke@certainkey.com>
 * Copyright (c) Andrew McDonald <andrew@mcdonald.org.uk>
 * Copyright (c) 2002 James Morris <jmorris@intercode.com.au>
 * SHA224 Support Copyright 2007 Intel Corporation <jonathan.lynch@intel.com>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option) 
 * any later version.
 *
 */
// #include <crypto/internal/hash.h>
// #include <linux/init.h>
// #include <linux/module.h>
// #include <linux/mm.h>
#include <string.h>
#include <linux/types.h>

#include "sha.h"

__be32 htonl(u32 hostlong);

u32 ntohl(__be32 netlong);

#define __be32_to_cpu(l) ntohl(l)
#define cpu_to_be32(l) htonl(l)

/* We've included the inline definitions of these operators to make
   clear the fact that rotation (n ror32 r) is just an operator which
   is equivalent to addition: it is (n div r) + (n mod r) * 2^(32-r),
   or something like that. It follows that rotation won't
   change the distribution in any essential way, it is just a
   permutation of the labels (and it is therefore invertible.) */

/**
 * ror64 - rotate a 64-bit value right
 * @word: value to rotate
 * @shift: bits to roll
 */
static inline __u64 ror64(__u64 word, unsigned int shift)
{
	return (word >> shift) | (word << (64 - shift));
}

/**
 * rol32 - rotate a 32-bit value left
 * @word: value to rotate
 * @shift: bits to roll
 */
static inline __u32 rol32(__u32 word, unsigned int shift)
{
	return (word << shift) | (word >> (32 - shift));
}

/**
 * ror32 - rotate a 32-bit value right
 * @word: value to rotate
 * @shift: bits to roll
 */
static inline __u32 ror32(__u32 word, unsigned int shift)
{
	return (word >> shift) | (word << (32 - shift));
}

/**
 * rol16 - rotate a 16-bit value left
 * @word: value to rotate
 * @shift: bits to roll
 */
static inline __u16 rol16(__u16 word, unsigned int shift)
{
	return (word << shift) | (word >> (16 - shift));
}

/**
 * ror16 - rotate a 16-bit value right
 * @word: value to rotate
 * @shift: bits to roll
 */
static inline __u16 ror16(__u16 word, unsigned int shift)
{
	return (word >> shift) | (word << (16 - shift));
}

/**
 * rol8 - rotate an 8-bit value left
 * @word: value to rotate
 * @shift: bits to roll
 */
static inline __u8 rol8(__u8 word, unsigned int shift)
{
	return (word << shift) | (word >> (8 - shift));
}

/**
 * ror8 - rotate an 8-bit value right
 * @word: value to rotate
 * @shift: bits to roll
 */
static inline __u8 ror8(__u8 word, unsigned int shift)
{
	return (word >> shift) | (word << (8 - shift));
}

#include <asm/byteorder.h>

typedef struct sha256_state ctxt_t;

static inline u32 Ch(u32 x, u32 y, u32 z)
{
	return z ^ (x & (y ^ z));
}

static inline u32 Maj(u32 x, u32 y, u32 z)
{
	return (x & y) | (z & (x | y));
}

#define e0(x)       (ror32(x, 2) ^ ror32(x,13) ^ ror32(x,22))
#define e1(x)       (ror32(x, 6) ^ ror32(x,11) ^ ror32(x,25))
#define s0(x)       (ror32(x, 7) ^ ror32(x,18) ^ (x >> 3))
#define s1(x)       (ror32(x,17) ^ ror32(x,19) ^ (x >> 10))

/* Why are these next two separate inlined functions when they are
   each only called from one place? It would not be any _more_
   difficult to read if this text appeared _in situ_, would it? */

static inline void LOAD_OP(int I, u32 *W, const u8 *input)
{

  /* Why do we have to re-order the input data? Can't we just re-order
     the constants and redefine the primitive operators so it works in
     native machine order? Is it because if we re-ordered the
     constants then "they" couldn't use just one _very_ small
     Boyer-Moore / Knuth-Morris-Pratt style pattern matcher, which
     could be implemented in, say, the bus logic of an undocumented
     DMA channel in some supporting chip-set or other, or the
     controller logic on a disk-drive, or the ALU of some particular
     CPU, or some particular "microcode update" of some particular
     CPU? Then they could detect any function that computes sha256
     checksums using this code, ... Not that that would be
     particularly interesting ... */

	W[I] = __be32_to_cpu( ((__be32*)(input))[I] );
}

static inline void BLEND_OP(int I, u32 *W)
{
        /* Careful treatment of the first two words of each block */
        /*                              vvvvvvv    vvvvvvv        */
	W[I] = s1(W[I-2]) + W[I-7] + s0(W[I-15]) + W[I-16];
}

static void sha256_transform(ctxt_t *ctxt, const u8 *input)
{
        u32 *state = ctxt->state;
	u32 a, b, c, d, e, f, g, h, t1, t2;
	u32 W[64];
	int i;

	/* load the input.

           Why? Each of these values will only ever be accessed at
	   most twice, so we could just do them inline at the point
	   where the values are used. */

	for (i = 0; i < 16; i++)
		LOAD_OP(i, W, input);

	/* now blend.

           Again, why do we keep all this data when it's only accessed
	   once? Each of the 4 pairs of unrolled loops below needs
	   only to access four of the previous blended values, and the
	   first 8 bytes of each block's input data. */

	for (i = 16; i < 64; i++)
		BLEND_OP(i, W);

	/* load the state into our registers.

           Why? It's a sequential update of two pairs of registers per
	   8 words of the buffer. So why not just update the state
	   in-place. Surely the compiler will be able to handle the
	   optimisations of the array accesses? Or is that what you
	   _don't_ want to happen? ... */

	a=state[0];  b=state[1];  c=state[2];  d=state[3];
	e=state[4];  f=state[5];  g=state[6];  h=state[7];

	/* now iterate */
	t1 = h + e1(e) + Ch(e,f,g) + 0x428a2f98 + W[ 0];
	t2 = e0(a) + Maj(a,b,c);    d+=t1;    h=t1+t2;

        /* Why is t2 a separate "register"? Why not just use t1 again?
	   Perhaps it would be too hard to deconvolve the joint
	   distribution of t1+t2 if we did that. Doing it this way,
	   the effects on the "registers" are kept in two independent,
	   and therefore separably-solvable parts through each step.  */

	t1 = g + e1(d) + Ch(d,e,f) + 0x71374491 + W[ 1];
	t2 = e0(h) + Maj(h,a,b);    c+=t1;    g=t1+t2;
	t1 = f + e1(c) + Ch(c,d,e) + 0xb5c0fbcf + W[ 2];
	t2 = e0(g) + Maj(g,h,a);    b+=t1;    f=t1+t2;
	t1 = e + e1(b) + Ch(b,c,d) + 0xe9b5dba5 + W[ 3];
	t2 = e0(f) + Maj(f,g,h);    a+=t1;    e=t1+t2;
	t1 = d + e1(a) + Ch(a,b,c) + 0x3956c25b + W[ 4];
	t2 = e0(e) + Maj(e,f,g);    h+=t1;    d=t1+t2;
	t1 = c + e1(h) + Ch(h,a,b) + 0x59f111f1 + W[ 5];
	t2 = e0(d) + Maj(d,e,f);    g+=t1;    c=t1+t2;
	t1 = b + e1(g) + Ch(g,h,a) + 0x923f82a4 + W[ 6];
	t2 = e0(c) + Maj(c,d,e);    f+=t1;    b=t1+t2;
	t1 = a + e1(f) + Ch(f,g,h) + 0xab1c5ed5 + W[ 7];
	t2 = e0(b) + Maj(b,c,d);    e+=t1;    a=t1+t2;

        /* Maybe there's a case for unrolling the inner 8 iterations
           of this loop.  But why unroll the outer loop? Do you really
           think an extra 8 loop tests and branches are going to make
           a significant difference to the performance of this
           program?  If so, why didn't you unroll the blend operation?
           That is a lot more computational effort than eight
           branches, isn't it?  

           Is it because, if you reduced it to one line of code in two
           nested loops, then people might not think it was quite so
           secure? Or are you worried someone (like a C compiler,
           perhaps!) might even spot some further "optimisations" which
           could make it _even simpler_ than that? */

	t1 = h + e1(e) + Ch(e,f,g) + 0xd807aa98 + W[ 8];
	t2 = e0(a) + Maj(a,b,c);    d+=t1;    h=t1+t2;
	t1 = g + e1(d) + Ch(d,e,f) + 0x12835b01 + W[ 9];
	t2 = e0(h) + Maj(h,a,b);    c+=t1;    g=t1+t2;
	t1 = f + e1(c) + Ch(c,d,e) + 0x243185be + W[10];
	t2 = e0(g) + Maj(g,h,a);    b+=t1;    f=t1+t2;
	t1 = e + e1(b) + Ch(b,c,d) + 0x550c7dc3 + W[11];
	t2 = e0(f) + Maj(f,g,h);    a+=t1;    e=t1+t2;
	t1 = d + e1(a) + Ch(a,b,c) + 0x72be5d74 + W[12];
	t2 = e0(e) + Maj(e,f,g);    h+=t1;    d=t1+t2;
	t1 = c + e1(h) + Ch(h,a,b) + 0x80deb1fe + W[13];
	t2 = e0(d) + Maj(d,e,f);    g+=t1;    c=t1+t2;
	t1 = b + e1(g) + Ch(g,h,a) + 0x9bdc06a7 + W[14];
	t2 = e0(c) + Maj(c,d,e);    f+=t1;    b=t1+t2;
	t1 = a + e1(f) + Ch(f,g,h) + 0xc19bf174 + W[15];
	t2 = e0(b) + Maj(b,c,d);    e+=t1;    a=t1+t2;

	t1 = h + e1(e) + Ch(e,f,g) + 0xe49b69c1 + W[16];
	t2 = e0(a) + Maj(a,b,c);    d+=t1;    h=t1+t2;
	t1 = g + e1(d) + Ch(d,e,f) + 0xefbe4786 + W[17];
	t2 = e0(h) + Maj(h,a,b);    c+=t1;    g=t1+t2;
	t1 = f + e1(c) + Ch(c,d,e) + 0x0fc19dc6 + W[18];
	t2 = e0(g) + Maj(g,h,a);    b+=t1;    f=t1+t2;
	t1 = e + e1(b) + Ch(b,c,d) + 0x240ca1cc + W[19];
	t2 = e0(f) + Maj(f,g,h);    a+=t1;    e=t1+t2;
	t1 = d + e1(a) + Ch(a,b,c) + 0x2de92c6f + W[20];
	t2 = e0(e) + Maj(e,f,g);    h+=t1;    d=t1+t2;
	t1 = c + e1(h) + Ch(h,a,b) + 0x4a7484aa + W[21];
	t2 = e0(d) + Maj(d,e,f);    g+=t1;    c=t1+t2;
	t1 = b + e1(g) + Ch(g,h,a) + 0x5cb0a9dc + W[22];
	t2 = e0(c) + Maj(c,d,e);    f+=t1;    b=t1+t2;
	t1 = a + e1(f) + Ch(f,g,h) + 0x76f988da + W[23];
	t2 = e0(b) + Maj(b,c,d);    e+=t1;    a=t1+t2;

	t1 = h + e1(e) + Ch(e,f,g) + 0x983e5152 + W[24];
	t2 = e0(a) + Maj(a,b,c);    d+=t1;    h=t1+t2;
	t1 = g + e1(d) + Ch(d,e,f) + 0xa831c66d + W[25];
	t2 = e0(h) + Maj(h,a,b);    c+=t1;    g=t1+t2;
	t1 = f + e1(c) + Ch(c,d,e) + 0xb00327c8 + W[26];
	t2 = e0(g) + Maj(g,h,a);    b+=t1;    f=t1+t2;
	t1 = e + e1(b) + Ch(b,c,d) + 0xbf597fc7 + W[27];
	t2 = e0(f) + Maj(f,g,h);    a+=t1;    e=t1+t2;
	t1 = d + e1(a) + Ch(a,b,c) + 0xc6e00bf3 + W[28];
	t2 = e0(e) + Maj(e,f,g);    h+=t1;    d=t1+t2;
	t1 = c + e1(h) + Ch(h,a,b) + 0xd5a79147 + W[29];
	t2 = e0(d) + Maj(d,e,f);    g+=t1;    c=t1+t2;
	t1 = b + e1(g) + Ch(g,h,a) + 0x06ca6351 + W[30];
	t2 = e0(c) + Maj(c,d,e);    f+=t1;    b=t1+t2;
	t1 = a + e1(f) + Ch(f,g,h) + 0x14292967 + W[31];
	t2 = e0(b) + Maj(b,c,d);    e+=t1;    a=t1+t2;

	t1 = h + e1(e) + Ch(e,f,g) + 0x27b70a85 + W[32];
	t2 = e0(a) + Maj(a,b,c);    d+=t1;    h=t1+t2;
	t1 = g + e1(d) + Ch(d,e,f) + 0x2e1b2138 + W[33];
	t2 = e0(h) + Maj(h,a,b);    c+=t1;    g=t1+t2;
	t1 = f + e1(c) + Ch(c,d,e) + 0x4d2c6dfc + W[34];
	t2 = e0(g) + Maj(g,h,a);    b+=t1;    f=t1+t2;
	t1 = e + e1(b) + Ch(b,c,d) + 0x53380d13 + W[35];
	t2 = e0(f) + Maj(f,g,h);    a+=t1;    e=t1+t2;
	t1 = d + e1(a) + Ch(a,b,c) + 0x650a7354 + W[36];
	t2 = e0(e) + Maj(e,f,g);    h+=t1;    d=t1+t2;
	t1 = c + e1(h) + Ch(h,a,b) + 0x766a0abb + W[37];
	t2 = e0(d) + Maj(d,e,f);    g+=t1;    c=t1+t2;
	t1 = b + e1(g) + Ch(g,h,a) + 0x81c2c92e + W[38];
	t2 = e0(c) + Maj(c,d,e);    f+=t1;    b=t1+t2;
	t1 = a + e1(f) + Ch(f,g,h) + 0x92722c85 + W[39];
	t2 = e0(b) + Maj(b,c,d);    e+=t1;    a=t1+t2;

	t1 = h + e1(e) + Ch(e,f,g) + 0xa2bfe8a1 + W[40];
	t2 = e0(a) + Maj(a,b,c);    d+=t1;    h=t1+t2;
	t1 = g + e1(d) + Ch(d,e,f) + 0xa81a664b + W[41];
	t2 = e0(h) + Maj(h,a,b);    c+=t1;    g=t1+t2;
	t1 = f + e1(c) + Ch(c,d,e) + 0xc24b8b70 + W[42];
	t2 = e0(g) + Maj(g,h,a);    b+=t1;    f=t1+t2;
	t1 = e + e1(b) + Ch(b,c,d) + 0xc76c51a3 + W[43];
	t2 = e0(f) + Maj(f,g,h);    a+=t1;    e=t1+t2;
	t1 = d + e1(a) + Ch(a,b,c) + 0xd192e819 + W[44];
	t2 = e0(e) + Maj(e,f,g);    h+=t1;    d=t1+t2;
	t1 = c + e1(h) + Ch(h,a,b) + 0xd6990624 + W[45];
	t2 = e0(d) + Maj(d,e,f);    g+=t1;    c=t1+t2;
	t1 = b + e1(g) + Ch(g,h,a) + 0xf40e3585 + W[46];
	t2 = e0(c) + Maj(c,d,e);    f+=t1;    b=t1+t2;
	t1 = a + e1(f) + Ch(f,g,h) + 0x106aa070 + W[47];
	t2 = e0(b) + Maj(b,c,d);    e+=t1;    a=t1+t2;

	t1 = h + e1(e) + Ch(e,f,g) + 0x19a4c116 + W[48];
	t2 = e0(a) + Maj(a,b,c);    d+=t1;    h=t1+t2;
	t1 = g + e1(d) + Ch(d,e,f) + 0x1e376c08 + W[49];
	t2 = e0(h) + Maj(h,a,b);    c+=t1;    g=t1+t2;
	t1 = f + e1(c) + Ch(c,d,e) + 0x2748774c + W[50];
	t2 = e0(g) + Maj(g,h,a);    b+=t1;    f=t1+t2;
	t1 = e + e1(b) + Ch(b,c,d) + 0x34b0bcb5 + W[51];
	t2 = e0(f) + Maj(f,g,h);    a+=t1;    e=t1+t2;
	t1 = d + e1(a) + Ch(a,b,c) + 0x391c0cb3 + W[52];
	t2 = e0(e) + Maj(e,f,g);    h+=t1;    d=t1+t2;
	t1 = c + e1(h) + Ch(h,a,b) + 0x4ed8aa4a + W[53];
	t2 = e0(d) + Maj(d,e,f);    g+=t1;    c=t1+t2;
	t1 = b + e1(g) + Ch(g,h,a) + 0x5b9cca4f + W[54];
	t2 = e0(c) + Maj(c,d,e);    f+=t1;    b=t1+t2;
	t1 = a + e1(f) + Ch(f,g,h) + 0x682e6ff3 + W[55];
	t2 = e0(b) + Maj(b,c,d);    e+=t1;    a=t1+t2;

	t1 = h + e1(e) + Ch(e,f,g) + 0x748f82ee + W[56];
	t2 = e0(a) + Maj(a,b,c);    d+=t1;    h=t1+t2;
	t1 = g + e1(d) + Ch(d,e,f) + 0x78a5636f + W[57];
	t2 = e0(h) + Maj(h,a,b);    c+=t1;    g=t1+t2;
	t1 = f + e1(c) + Ch(c,d,e) + 0x84c87814 + W[58];
	t2 = e0(g) + Maj(g,h,a);    b+=t1;    f=t1+t2;
	t1 = e + e1(b) + Ch(b,c,d) + 0x8cc70208 + W[59];
	t2 = e0(f) + Maj(f,g,h);    a+=t1;    e=t1+t2;
	t1 = d + e1(a) + Ch(a,b,c) + 0x90befffa + W[60];
	t2 = e0(e) + Maj(e,f,g);    h+=t1;    d=t1+t2;
	t1 = c + e1(h) + Ch(h,a,b) + 0xa4506ceb + W[61];
	t2 = e0(d) + Maj(d,e,f);    g+=t1;    c=t1+t2;
	t1 = b + e1(g) + Ch(g,h,a) + 0xbef9a3f7 + W[62];
	t2 = e0(c) + Maj(c,d,e);    f+=t1;    b=t1+t2;
	t1 = a + e1(f) + Ch(f,g,h) + 0xc67178f2 + W[63];
	t2 = e0(b) + Maj(b,c,d);    e+=t1;    a=t1+t2;

        /* Oh gosh, it ADDS the new state values to the old ones. But
	   it's already added the old values, at the beginning. So the
	   state distribution at the end of each block is always
	   convolved with the distribution that it was in at the
	   beginning of that block ... Why would anyone want to do
	   _that?_ */

	state[0] += a; state[1] += b; state[2] += c; state[3] += d;
	state[4] += e; state[5] += f; state[6] += g; state[7] += h;

	/* clear any sensitive info ... Good idea! */

	a = b = c = d = e = f = g = h = t1 = t2 = 0;
	memset(W, 0, 64 * sizeof(u32));

        /* And of course no compiler would _dare_ optimise either of
           these two lines away. Which is jolly lucky, because
           otherwise this program would leave copies of all your
           un-encrypted data, and the intermediate states of the
           checksum of each block, on the stack. So now you know why
           gcc defers pops, and why the only way you can turn this
           "feature" off, is by turning off all optimisations, and
           saying -fno-defer-pop, or something.

           Not that anyone would be interested in your data, unless
           you were using this routine to do Diffie-Helman PAK
           exchange and then your shared secret would become even
           _more_ widely shared than you thought it was. But if you
           _are_ using PAK Diffie-Helman, then you presumably don't
           care about things like that, because you're worried about
           how to compute a 2048 bit prime number, and how to choose a
           suitable generator for it. */
}

int sha224_init(ctxt_t *ctxt)
{
  u32 *state = ctxt->state;

	state[0] = SHA224_H0;
	state[1] = SHA224_H1;
	state[2] = SHA224_H2;
	state[3] = SHA224_H3;
	state[4] = SHA224_H4;
	state[5] = SHA224_H5;
	state[6] = SHA224_H6;
	state[7] = SHA224_H7;

  /* I wonder _why_ these checksums _all_ start in a fixed, well-known
     state. It seems pretty obvious that if the checksum was the
     result of an actual _computation,_ then it would be much
     stronger. For example, if instead of publishing a fixed checksum,
     using a fixed start state and a fixed algorithm, the provider
     published the intensional specification of a function which,
     given a start state, an offset, and the block length of the data,
     computed the checksum from that state, for just that block, and
     returned it.

     Then, if you could compute such checksums _really quickly,_ using
     a DSP chip, or a good FFT package, this could operate as a
     zero-knowledge challenge-response protocol, and it would also
     work as a classical commitment scheme: you generate a file, and
     you compute the checksum function for it, and publish that. Then
     people can verify that the data they later receive from you is
     indeed the data you had in hand, otherwise you could not have
     computed the checksum function.

     This would be done by letting the other party choose a starting
     state value at random, from which she computes the checksum for a
     given block. Then she sends you that checksum, and tells you the
     block offset and length, but NOT the starting state. You compute
     the starting state she must have used by inverting the checksum
     calculation---which you can do, because you have all the input
     data---and then you prove that you know her starting state, not
     by sending her the result, but by using that same starting state
     to compute the checksum of a _different_ block of the same input
     data. Then you send her this new checksum, and tell her the block
     offset and length, and she can verify that you probably know her
     randomly chosen starting state by using it, with the published
     function, to compute the checksum for the new block you have
     chosen. It should match the one you sent her.

     The two parties can repeat this exchange as many times as they
     like, as long as the blocks chosen are mutually exclusive, and
     the random start states un-correlated (i.e. statistically
     independent distributions, not deterministically chosen by a
     pRNG!), if not, there a risk of leaking the data through the
     distribution of the values of the checksums.  Then when the data
     finally arrive, she can use them to verify that the published
     function is indeed _exactly_ the function which computes the
     block checksums for that set of data, and she can do this by
     using the same algorithm you used to generate the intensional
     function from the data, given that particular generic checksum
     algorithm.

     The actual checksum algorithm used will typically not be known by
     her, until the data arrive. This can be done by randomly
     "hybridising" various different algorithms. For example, one
     could 'tile' the different algorithms sha224, sha256, sha512,
     sha384 etc, covering the whole of the data, and then composing
     the concatenations of the resulting checksums with a randomly
     chosen permutation, and then computing the checksum of the
     resulting data again. 

     This composition of encodings and checksum calculations would
     have to be done by machine, perhaps by using Knuth-Bendix
     completion to calculate a suitable reduction relation on
     expressions, by which the composition of these functions,
     partially applied to the input data, could be mechanically, and
     therefore deterministically, simplified, producing some one
     unique intensional specification of that function from start
     state to checksum.

     But before using this idea, you should explicitly verify that the
     checksum algorithm you are using really is a maximum entropy
     function: this is to say that, given a description of the
     statistical behaviour of the input data, and the encoding you are
     using (which could be in the form of a Markov chain describing the
     transition probabilities on the channel) you need to be sure
     that, whatever the actual values of either your data input to the
     encoder, or the starting state, the resulting distribution of
     possible checksum values is flat, (and sufficiently wide!) Then
     you will know that, if someone knows the checksum and the offset,
     length and start state for some block, the particular bytes
     actually in that block could, _a priori,_ be any values
     whatsoever.

     One could use a similar method to forward-generate
     one-time-pads. What you would have to do is something like this:
     use a stream protocol which allows the sender to arbitrarily
     insert instructions to change the pad, which she does at random
     intervals. Then at each such point, you take the existing
     permutation you are using, and randomly permute all the
     bytes. Then you choose a random start state, and with that random
     start state, you re-compute the checksum of some data block
     you've earlier encrypted with that same pad, and already
     transmitted. Then you send the other party the new checksum, with
     the state, offset, and length (encoded by the previous pad you
     used, of course) and they have to guess which bytes you swapped,
     which they can do quickly, because they know what data is in that
     block, and therefore they can straightforwardly invert the
     checksum computation: it's not going to be much harder than
     solving a Sudoku puzzle.

     The forward calculation of the checksum, using variables to
     represent the unknown permutation, will give you a set of
     constraints on the permutations, ultimately determined by the
     value of the checksum. It will also provide a probability
     distribution. The probability distribution is computed
     iteratively as the calculation of the checksum progresses: when
     you add any two values --- _if and only if_ they have independent
     distributions --- the distribution of their sum will be the
     convolution of their independent distributions. So you can invert
     the checksum calculation by using Bayes' Theorem and
     deconvolution(*) to invert the conditional probabilities, and
     thereby find the distribution of the possible permutation values,
     which will tell you the most highly-constrained swaps.  Then, to
     search for the values of the permutations, one would set the most
     highly constrained pair of swaps to their most probable values,
     and calculate the new constraints and the new distributions. Then
     repeat, starting with the most probable values of the next most
     highly constrained pair, conditional on the chosen values for the
     first pair, and backtracking to the next most probable, most
     recent branches, when the search fails.

     Note that any permutation can be written as a composition of a
     number of pair-wise swaps of elements, and furthermore, that any
     such series of swaps can always be written so that the order of
     the pairs is strictly increasing, and each swap is with an
     element further along the list. A fancy word for this process of
     inverting a permutation is "sorting" (think of the bubble-sort
     algorithm: that will find this order, given an arbitrary
     permutation of an ordered list of some sequence of distinct
     values.)

     So you can run the checksum calculation forward, and compute the
     constraints on the swaps as you go. Then by the time you get to
     the end, you will have constructed a search space and a "map," in
     the form of a Markov chain, which will tell you what are the most
     highly constrained swaps, and what are their most probable
     values, so you can back-track and update the permutation matrix
     as you systematically search the space, starting with the most
     likely values.

     Of course, this process will only work if the sender first
     verifies that the permutation chosen has a unique solution,
     i.e. that it is the only permutation which produces that
     particlar checksum, for that particular block and starting value
     chosen. This is easily done, because the sender need only go
     through the same process, but using definite values for the
     permutation swaps, and verifying that the checksum uniquely
     constrains the permutation as computed by inverting the
     constraints, i.e. starting from the computed checksum.

     (*) See this note from WikipediA (Page last modified on 17
         February 2014 at 06:30.):

         http://en.wikipedia.org/wiki/Convolution

         A discrete example is a finite cyclic group of order
         n. Convolution operators are here represented by circulant
         matrices, and can be diagonalized by the discrete Fourier
         transform.

     Note that a diagonalised matrix is very easily inverted! A
     checksum operator such as the sha256 transform makes one or more
     finite cyclic groups when it is considered as a binary operator
     acting on pairs of 256-bit values (one value is the state, the
     other is the input data).  If it is not itself cyclic, then it
     will be composed of one or more cyclic sub-groups. This is simply
     because any sequence of swaps either cycles as a whole, or it is
     composed of one or more mutually exclusive cycles. And in either
     case each cycle is closed, so the action of the operator on the
     elements of the group can be described separately for each closed
     cycle, because they are independent of one another.

     If you want a nice example of a similar idea, see the above
     sha256_transform function. Look at it in the context where it is
     being used under PAK Diffie-Helman, with forward shared-secret
     exchange. PAK blocks are always multiples of the size of the
     "randomising" checksums that are used to compute the key.

     Note how in each block of 8 words of the (blended) input buffer,
     the values of the eight "registers" flow forward, always in two
     separate halves. And at each step, the distribution of the result
     of each half is convolved with a known "whackblat". That makes it
     easy to find the separate distributions of the independent
     inputs, and note that the state updates are each only to two
     separate 32 bit words, each independent of the other at each
     step. Now remember that, in PAK Diffie-Helman, the only
     independent input is the shared secret: the two user identities,
     the generator g and the prime p are typically fixed for the
     duration of the exchange using any given shared secret. Recall
     now that the start state is fixed, and that the first two things
     in each Diffie-Helman block are the identities of the
     participants. Now recall that the new state at the end of each sha256
     block is always added to the old state at the beginning, and that
     the initial state is fixed.

     So, given these facts, and recalling the pair-wise separation of
     the state dependency chains, in each block, and the helpful stack
     trace the C compiler left around, perhaps preserved by the
     judicious application of a stack change after a certain class of
     function call, perhaps when any of these functions are called
     which involve a unique sequence of some parts of these 64 quite
     peculiar 32 bit values, so pretty easy to spot, ... Given all
     this, it ought to be easy enough to de-convolve the combined
     distributions of the whole chain, starting from the checksum
     (given the known length and pad bytes) and thereby determining
     the independent distributions of the first eight bytes of the
     whole chain.

     Now what are those bytes? Well, if you were on a littleendian
     machine, and you put two 16 bit uids, followed by some unknown
     exponent of a large prime of known length, in the first part of
     the first packet of your exchange, then anyone looking at packets
     would quickly learn the distribution of the high order 32 bits of
     the prime exponent, and that would probably significantly
     constrain the possible values of the shared secret. Of course, if
     it is the high order bits of the shared secret that appear next,
     well then, you're fucked, aren't you?

     So it's perhaps good of Intel to have bolted on SHA224, whose
     digest size is seven words (28 bytes). This way, there can be a
     factor of 7 as well as 2 in the patchwork of sums, and the cycles
     won't decompose quite so completely as they do when everything is
     a bi-cycle! */

	ctxt->count = 0;

	return 0;
}

int sha256_init(ctxt_t *ctxt)
{
  u32 *state = ctxt->state;

  /* I wonder why these values are hidden in a header file? Is it
     absolutely necessary, for _someone,_ that the C pre-processor
     sees them in pre-compiled header-files, before the compiler does?
     What about the magic hocus-pocus numbers in sha256_transform,
     don't they also deserve this special treatment?

     There must be something quite peculiarly special about these
     numbers, because the only difference between sha224 and sha256
     with the checksum truncated to the 7 high-order words, is these
     first eight bytes of initial state. */

	state[0] = SHA256_H0;
	state[1] = SHA256_H1;
	state[2] = SHA256_H2;
	state[3] = SHA256_H3;
	state[4] = SHA256_H4;
	state[5] = SHA256_H5;
	state[6] = SHA256_H6;
	state[7] = SHA256_H7;
	ctxt->count = 0;

	return 0;
}

int sha256_update(ctxt_t *ctxt,
                  const u8 *data,
                  unsigned int len)
{
	unsigned int partial, done;
	const u8 *src;
        u8 *buf = ctxt->buf;

	partial = ctxt->count & 0x3f;
	ctxt->count += len;
	done = 0;
	src = data;

	if ((partial + len) > 63) {
		if (partial) {
			done = -partial;
			memcpy(buf + partial, data, done + 64);
			src = buf;
		}

		do {    sha256_transform(ctxt,src);
			done += 64;
			src = data + done;
		} while (done + 63 < len);

		partial = 0;
	}
	memcpy(buf + partial, src, len - done);

	return 0;
}

int sha256_final(ctxt_t *ctxt,u8 *out)
{
        u32 *state = ctxt->state;
	__be32 *dst = (__be32 *)out;
	__be64 bits;
	unsigned int index, pad_len;
	int i;
	static const u8 padding[64] = { 0x80, };

	/* Save number of bits */
	bits = cpu_to_be64(ctxt->count << 3);

	/* Pad out to 56 mod 64. */
	index = ctxt->count & 0x3f;
	pad_len = (index < 56) ? (56 - index) : ((64+56) - index);
	sha256_update(ctxt,padding, pad_len);

	/* Append length (before padding)

           Why? So that we are guaranteed that there are eight known
	   bytes of the contents of the last block? This places a
	   fairly severe constraint on the possible values of input
	   data, doesn't it? Don't we need to prove that this isn't a
	   loose thread someone can pull on, and thereby unravel the
	   lovely little jumper we've just so carefully knitted?

           In case someone wants to claim that this is done in order
           to discriminate between two different files of different
           lengths, which just accidentally happen to have the same
           checksum,... oh please, don't! At least, not before you've
           thought about it for a moment, or two. */

	sha256_update(ctxt,(const u8 *)&bits, sizeof(bits));

	/* Store state in digest */
	for (i = 0; i < 8; i++)
		dst[i] = cpu_to_be32(state[i]);

        /* There we go, back to bigendian ... it must be really
	   important to _someone_ that the computation is broken down
	   into 32 bit words, and the input and output data are
	   bigendian values. Which suggests that if you want to use
	   this algorithm for anything you actually care about, you
	   should avoid 32 bit values, and you should think _very_
	   hard, about exactly what data you decide to put in those
	   first eight bytes of every block! If you decide to use any
	   of this class of functions, then do make sure you use a
	   statistically independent random state for each checksum
	   you compute. And you will want to do something about the
	   length and the fixed padding bytes appearing at the end of
	   the last block, _especially_ if your input is exactly a
	   multiple of the block size, as it will be in many
	   implementations of PAK Diffie-Helman, because then the only
	   thing in the last block is the length and the padding
	   bytes, both known, so the distribution of the state at the
	   end of the last block of actual data is trivially
	   derivable, and you know that this is convolved with the
	   state at the start of the last block of actual data, so you
	   can unravel the pair of chains down to the start, and
	   thereby learn the distribution of the state at the end of
	   the _penultimate_ block and ... so on and so forth ...

           Now sha224 is useful here, because if the last block is the
	   sha224 sum of the preceeding block, then it leaves exactly
	   eight bytes for the length, and _that_ transform can be
	   nested arbitrarily, and composed with arbitrary
	   permutations of the underlying data, but without
	   accumulating an ever increasing chain of known lengths at
	   the end of each block, each time you add another layer,
	   which gives anyone, who could be bothered, a handy little
	   independent un-zipper thread for each layer.

           Intel must have some quite smart people working for them
	   ... At least, these cats seem to be the only ones who have
	   made a contribution to this "program" which is not
	   immediately obviously a rather feeble-minded subversion
	   attempt. Well, they don't need to subvert the source if
	   they can subvert the microcode, and the silcon, and the
	   support chipset, and the network and graphics adapters, and
	   the power management, and ... About the only thing they
	   can't subvert is mice, but if you have everything else,
	   then you could probably have the mouse too, if only you
	   could be bothered. But what _could_ you learn about that
	   company that isn't already streaming down a bit-torrent
	   somewhere?

           But this isn't all. Stay tuned to this channel, or any
           bit-torrent near you, and you might get to hear a bit about
           Gnu GMP and how it uses a certain pRNG with quite
           _peculiarly excellent_ randomness properties, to choose
           exponents for application in a Miller-Rabin primality
           test. But weirdly, before they apply Miller-Rabin, they
           check it's not a Fermat prime, and then they run it by a
           few other likely factors, testing explicitly for
           divisibility. Clearly these fellows don't _completely_
           believe the story Knuth purportedly told about the rate at
           which the probability of the subject being composite falls
           off with each successive iteration of the Miller-Rabin
           test. Perhaps this is because _that_ rate was calculated on
           the basis of each test being an independently chosen random
           exponent, i.e. _not_ a series that are deterministically
           generated, starting from an enormous known prime?

           Well, if you were selling composite mutton dressed as prime
           lamb, you _would_ want to weed out the ones with small
           factors like 2, 3, 5 etc., wouldn't you? I mean, you don't
           want the customers noticing that the product's rubbish
           _before_ they pay you for it, do you?!

           And there are many, _many_ more, equally fascinating things
           yet to find out, such as why the GMP exptm function takes
           up so much memory. And how it works, and what are its
           statistical properties, vis a vis invertibility, especially
           when _so much_ (i.e. that it survived _fifty_ iterations of
           "The Mersenne twister"!) is known about any "probable"
           prime that might be one of its arguments. Particularly so
           if it's a 1024 bit integer, because, well, what else
           _would_ you likely be doing, that needs a 1024 bit integer,
           counting sheep?

           Really, there are so _many,_ so _different,_ but yet
           equally promising possible lines of investigation to pursue
           here, that I can't even decide which to try first. I think
           I'll just avoid this sort of "cryptography" altogether and
           save myself a lot of time. So I'm just going to leave it to
           Bruce; maybe he can build it into the Electronic Frontier's
           Foundation, the masonry of which is looking more than just
           a little flaky, these days ...

*/

	return 0;
}

int sha224_final(ctxt_t *ctxt,u8 *hash)
{
	u8 D[SHA256_DIGEST_SIZE];

	sha256_final(ctxt,D);

	memcpy(hash, D, SHA224_DIGEST_SIZE);
	memset(D, 0, SHA256_DIGEST_SIZE);     /* Probably optimised away ... */

	return 0;
}
