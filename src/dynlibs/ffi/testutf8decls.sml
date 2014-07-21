
val utf8string = "\u03b1\u03c9";

val utf8char = #"\u002A";

val utf8string2 = "\u02A7a0\n";
val _ = print utf8string2

val w = valOf (UTF8.UCSfromUTF8String utf8string);

val ostroke = valOf (UTF8.UCSfromUTF8String "ø");
val ostrokestring = UTF8.UCStoUTF8String 0wx00f8;
val ostrokecl = (explode "hunden g\u00f8r");
val ostrokechar = #"\248";

val ch3 = "\u50d2\u50d0\n";

val ch3sz = UTF8.size ch3;
val whodunit = "hunden g\u00f8r";
val hgsz = String.size whodunit;
val hgsz' = UTF8.size whodunit;

val _ = print ch3;

val testpp = "\u50d2 \^A\042\u50d0僒僐";
val testpp' = "\u50d2 \^A*\u50d0\u50d2\u50d0"
val _ = print (testpp^"\n"^testpp'^"\n");

val padl = UTF8.padLeft #"*" 3 "僐"
val padr = UTF8.padLeft #"*" 5 (UTF8.padRight #"*" 3 "\u50d0")
val _ = print (padl^"\n"^padr^"\n")


val wlc3 = UCSlist ch3;
val slc3 = UTF8list ch3;
val sz3 = UTF8size ch3;
