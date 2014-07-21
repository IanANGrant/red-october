load "UTF8";
load "PP";
load "Substring";

use "ppchar.sml";

val _ = Meta.utf8 := true;

(* I wonder why these aren't in the SML Basis library? A StringCvt.reader
   is just a sequence, and you can always fold a sequence. *)

fun foldl f =
  fn f' => 
    fn r =>
      let fun iter r NONE = r
        | iter r (SOME (a,b)) = iter (f'(a,r)) (f b)
      in fn a => iter r (f a)
      end;

fun foldr f =
  fn f' => 
    fn r =>
      let fun iter r NONE = r
        | iter r (SOME (a,b)) = f'(a,iter r (f b))
      in fn a => iter r (f a)
      end;

fun scanList scanner = List.rev o (foldl scanner op :: []) o Substring.full;

val UTF8scanner = UTF8.scanUTF8 Substring.getc;

val UCSscanner = UTF8.scanUCS Substring.getc;

val UCSlist = scanList UCSscanner

val UTF8list = scanList UTF8scanner

(* This is only for illustration. The version in UTF8.sml is faster. *)
val UTF8size = foldl UCSscanner (fn (_,a)=> 1+a) 0 o Substring.full;

val wordToChar = String.toString o String.str o Char.chr o Word.toInt
fun UCStoChar w = ((fn s => "\\u"^s) o
                   StringCvt.padLeft #"0" 4 o
                   (String.translate (str o Char.toLower)) o
                   Word.toString) w
fun charcat (w,a) = a^(if w > 0wx7f then UCStoChar w else wordToChar w);
val UTF8stringToMLString = foldl UCSscanner charcat "" o Substring.full;

val listpp = ["a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a",
              "ε","a","ε","a","ε","a","ε","a","ε","a","ε","a","ε","a","ε","a","ε","a","ε","a"];

local
   fun strtostr s = ("\""^(UTF8stringToMLString s)^"\"")
   fun ppstring pps s0 = 
       let open PP
           fun ppstr s = add_string pps (strtostr s)
       in
           begin_block pps INCONSISTENT 0;
           ppstr s0;
           end_block pps
    end
in
   val _ = Meta.installPP ppstring
end;

use "testutf8decls.sml";
