(* Random -- Moscow ML library 1995-04-23, 1999-02-24, 2000-10-24, 
   2004-01-12 *)

(* Perhaps replace with one of Marsaglia's multi-seed generators; see
   CACM 46, 5 (May 2003) 90-93 or http://home.attbi.com/~glynnec1/random.c *)

type generator = {seedref : real ref}

(* Generating random numbers.  Paulson, page 96 *)

val a = 16807.0 
val m = 2147483647.0 

(* The seed must be integral but is represented in a real to get a
   wider range *)

fun nextrand seed = 
    let val t = a*seed 
    in t - m * real(floor(t/m)) end

fun newgenseed 0.0  = raise Fail "Random.newgenseed: bad seed 0.0"
  | newgenseed seed = {seedref = ref (nextrand seed)};

fun newgen () =
    let prim_val getrealtime_ : unit -> real = 1 "sml_getrealtime"
	val r    = getrealtime_ ()
        (* Changed divisor from 10^6 to 10^7 to avoid trunc Overflow *)
	val sec  = real (trunc(r/10000000.0))
	val usec = trunc(r - 10000000.0 * sec);
    in newgenseed (sec + real usec) end;

fun random {seedref as ref seed} = 
    (seedref := nextrand seed; seed / m);

fun randomlist (n, {seedref as ref seed0}) = 
    let fun h 0 seed res = (seedref := seed; res)
	  | h i seed res = h (i-1) (nextrand seed) (seed / m :: res)
    in h n seed0 [] end;

fun range (min, max) = 
    if min >= max then raise Fail "Random.range: empty range" 
    else 
	let val scale = (real max - real min) / m
	in
	    fn {seedref as ref seed} =>
	    (seedref := nextrand seed; floor(real min + scale * seed))
	end;

fun rangelist (min, max) =
    if min >= max then raise Fail "Random.rangelist: empty range" 
    else 
	let val scale = (real max - real min) / m
	in
	    fn (n, {seedref as ref seed0}) => 
	    let fun h 0 seed res = (seedref := seed; res)
		  | h i seed res = 
		h (i-1) (nextrand seed) (floor(real min + scale * seed) :: res)
	    in h n seed0 [] end
	end;
