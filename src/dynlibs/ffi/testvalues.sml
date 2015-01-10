val _ = load "Values";

val s = "abcdef";
val _ = Values.dumpb s;
val _ = Values.dumpw s;

val true = Values.value (Values.new (Values.Atom 1,0));

val atoms = Values.cptr true;
