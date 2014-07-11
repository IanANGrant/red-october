(* test/time.sml
   PS 1995-03-23, 2000-10-24
*)

use "auxil.sml";

local 
    fun fib n = if n<2 then 1 else fib(n-1) + fib(n-2);
    open Time
    val bigt = fromSeconds 987654321 + fromMicroseconds 500012;
    val litt = fromSeconds 454 + fromMicroseconds 501701
    val nbigt = fromSeconds ~987654321 + fromMicroseconds ~500012;
    val nlitt = fromSeconds ~454 + fromMicroseconds ~501701
in

val test1 = 
    check'(fn _ => 
	   zeroTime + bigt = bigt 
	   andalso bigt - zeroTime = bigt
	   andalso zeroTime + nbigt = nbigt 
	   andalso nbigt - zeroTime = nbigt);

val test2a = 
    check'(fn _ => toSeconds zeroTime = 0
	   andalso zeroTime = fromSeconds 0
	   andalso zeroTime = fromMilliseconds 0
	   andalso zeroTime = fromMicroseconds 0);
val test2b = 
    check'(fn _ => toSeconds bigt = 987654321
	   andalso toSeconds litt = 454
	   andalso toMilliseconds litt = 454501
	   andalso toMicroseconds litt = 454501701
	   andalso toSeconds nbigt = ~987654321
	   andalso toSeconds nlitt = ~454
	   andalso toMilliseconds nlitt = ~454501
	   andalso toMicroseconds nlitt = ~454501701);

val test3a = 
    check'(fn _ => fromReal 0.0 = zeroTime
	   andalso fromReal 10.25 = fromSeconds 10 + fromMilliseconds 250
	   andalso fromReal 10.000025 = fromSeconds 10 + fromMicroseconds 25);

val test4a = 
    check'(fn _ => 
	   toReal (fromReal 100.25) = 100.25
	   andalso toReal (fromReal 100.015625) = 100.015625);

val test6a = 
    check'(fn _ => bigt + litt = litt + bigt
	   andalso (bigt + litt) - litt = bigt
	   andalso (bigt - litt) + litt = bigt
	   andalso nbigt + nlitt = nlitt + nbigt
	   andalso (nbigt + nlitt) - nlitt = nbigt
	   andalso (nbigt - nlitt) + nlitt = nbigt
	   andalso nbigt + litt = litt + nbigt
	   andalso (bigt + nlitt) - nlitt = bigt
	   andalso (nbigt - litt) + litt = nbigt);

val test7a = 
    check'(fn _ => litt <= litt andalso litt >= litt
	   andalso zeroTime < litt andalso litt > zeroTime
	   andalso zeroTime > nlitt andalso nlitt < zeroTime
	   andalso litt < bigt andalso bigt > litt
	   andalso nbigt < nlitt andalso nlitt > nbigt
	   andalso not (litt > bigt) 
	   andalso not (bigt < litt) 
	   andalso not(litt < litt)
	   andalso not(litt > litt)
	   andalso not (nbigt > nlitt) 
	   andalso not (nlitt < nbigt) 
	   andalso not(nlitt < nlitt)
	   andalso not(nlitt > nlitt));

val test8a = 
    check'(fn _ => now() <= now() 
	   andalso (now () before fib 27 seq ()) < now());

val test9a = 
    check'(fn _ => fmt ~1 litt  = "455"
	   andalso fmt 0 litt = "455");

val test9b = 
    check'(fn _ => fmt 1 litt = "454.5"
	   andalso fmt 2 litt = "454.50"
	   andalso fmt 3 litt = "454.502"
	   andalso fmt 4 litt = "454.5017"
	   andalso fmt 5 litt = "454.50170"
	   andalso fmt 6 litt = "454.501701"
	   andalso fmt 1 nlitt = "~454.5"
	   andalso fmt 2 nlitt = "~454.50"
	   andalso fmt 3 nlitt = "~454.502"
	   andalso fmt 4 nlitt = "~454.5017"
	   andalso fmt 5 nlitt = "~454.50170"
	   andalso fmt 6 nlitt = "~454.501701");
    
val test9c =
    check'(fn _ => toString zeroTime = "0.000"
	   andalso toString(fromReal(Math.pow(2.0,30.0))-fromReal 1.0)
	   = "1073741823.000"
	   andalso toString(fromReal(Math.pow(2.0,30.0)))
	   = "1073741824.000"
	   andalso toString(fromReal(Math.pow(2.0,30.0))+fromReal 1.0)
	   = "1073741825.000"
	   andalso toString(fromReal(Math.pow(2.0,30.0))-fromReal 1.0+
			    fromReal(Math.pow(2.0,30.0)))
	   = "2147483647.000");

fun chk (s, r) = 
    check'(fn _ => 
	   case fromString s of
	       SOME res => res = fromMicroseconds r
	     | NONE     => false)

val test10a = 
    List.map chk
         [("189", 189000000),
	  ("189.1", 189100000),
	  ("189.125125", 189125125),
	  ("+189", 189000000),
	  ("+189.1", 189100000),
	  ("+189.125125", 189125125),
	  ("~189", ~189000000),
	  ("~189.1", ~189100000),
	  ("~189.125125", ~189125125),
	  ("-189", ~189000000),
	  ("-189.1", ~189100000),
	  ("-189.125125", ~189125125),
	  (".1", 100000),
	  (".125125", 125125),
	  ("+.1", 100000),
	  ("+.125125", 125125),
	  ("~.1", ~100000),
	  ("~.125125", ~125125),
	  ("-.1", ~100000),
	  ("-.125125", ~125125),
	  (" \n\t189crap", 189000000),
	  (" \n\t189.1crap", 189100000),
	  (" \n\t189.125125crap", 189125125),
	  (" \n\t.1crap", 100000),
	  (" \n\t.125125crap", 125125)];

val test10b = 
    List.map (fn s => case fromString s of NONE => "OK" | _ => "WRONG")
         ["", "+ 189", "~ 189", "- 189", "now", "Monday"];
end
