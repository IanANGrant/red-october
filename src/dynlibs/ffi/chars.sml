val _ = load "Substring";

structure Stream =
struct
   abstype 'a State =
       State of {get : ('a * 'a State) option}
   with
      fun new get =
         fn s => 
            let fun self s =
                State {get = 
                         case get s
                           of SOME (c,s') => SOME (c,self s')
                            | NONE => NONE}
            in self s
            end
      fun get (State s) = #get s
   end
end

fun prefix (s,r) =
   Stream.new
     (fn (NONE,r) => (case Stream.get r
                        of SOME (c,r') => SOME (c,(NONE,r'))
                         | NONE => NONE)
       | (SOME s,r) =>
         case Stream.get s
           of NONE => (case Stream.get r
                         of SOME (c,r') => SOME (c,(NONE,r'))
                          | NONE => NONE)
            | SOME (c,s) => SOME (c,(SOME s,r)))
     (SOME s,r)

fun stringStreamStrm s =
   let fun getc (SOME str,s) = 
             let val len = String.size str
             in if len > 1
                   then SOME (String.sub(str,0),(SOME (String.substring (str,1,len - 1)),s))
                   else SOME (String.sub(str,0),(NONE,s))
             end
          | getc (NONE,s) = raise Fail "stringStreamStrm: internal error"
   in Stream.new
      (fn (NONE,s) =>
            let fun iter s =
               case Stream.get s
                 of NONE => NONE
                  | SOME (str,s') => 
                      if String.size str = 0
                         then iter s'
                         else getc (SOME str,s')
            in iter s end
        | (SOME str,s') => getc (SOME str,s'))
       (NONE,s)
   end

fun stringliststrm (l:string list) =
   Stream.new List.getItem l

val strm1 = stringliststrm ["abc","def","ghi"];

val strm1cs = stringStreamStrm strm1;

val SOME (r0,cstrm) = Stream.get strm1cs;
val SOME (r1,cstrm) = Stream.get cstrm;
val SOME (r2,cstrm) = Stream.get cstrm;
val SOME (r3,cstrm) = Stream.get cstrm;
val SOME (r4,cstrm) = Stream.get cstrm;
val SOME (r5,cstrm) = Stream.get cstrm;
val SOME (r6,cstrm) = Stream.get cstrm;
val SOME (r7,cstrm) = Stream.get cstrm;
val SOME (r8,cstrm) = Stream.get cstrm;

val cliststrm = fn s =>
           Stream.new
              (fn [] => NONE | (c::cs) => SOME (c,cs))
              (String.explode s);

val substringstrm = fn s =>
           Stream.new
              Substring.getc
              (Substring.full s);

val cstrm0 = substringstrm "ABC";
val cstrm1 = substringstrm "DEF";
val cstrm2 = substringstrm "GHI";
val cstrm3 = prefix (prefix(cstrm0,cstrm1),cstrm2);

val SOME (r0,cstrm3) = Stream.get cstrm3;
val SOME (r1,cstrm3) = Stream.get cstrm3;
val SOME (r2,cstrm3) = Stream.get cstrm3;
val SOME (r3,cstrm3) = Stream.get cstrm3;
val SOME (r4,cstrm3) = Stream.get cstrm3;
val SOME (r5,cstrm3) = Stream.get cstrm3;
val SOME (r6,cstrm3) = Stream.get cstrm3;
val SOME (r7,cstrm3) = Stream.get cstrm3;
val SOME (r8,cstrm3) = Stream.get cstrm3;

fun lookup l =
   fn c => List.find (fn (r as (c',_)) => c' = c) l

(* Could be a generic 'cat' operator on streams implemented by partial function composition *)

fun scanunesc fnnm escchar escs getc =
   fn (c::cs,s) => SOME (c,(cs,s))
    | ([],s) =>
      case getc s
        of SOME (c,s') => 
           if c = escchar
              then case getc s'
                     of NONE => NONE
                      | SOME (c',s'') =>
                        (case lookup escs c'
                           of SOME (_,c'') => SOME (c'',([],s''))
                            | NONE => raise Fail ("Invalid "^fnnm^
                                                  " escape character "^
                                                    (String.str c')))
              else SOME (c,([],s'))
         | NONE => NONE

fun lookdown l =
   fn c => List.find (fn (r as (_,c')) => c' = c) l

fun scanesc fnm escchar escs getc =
   fn (c::cs,s) => SOME (c,(cs,s))
    | ([],s) =>
      case getc s
        of SOME (c,s') =>
              (case lookdown escs c
                 of NONE => SOME (c,([],s'))
                  | SOME (c',_) => SOME (escchar,([c'],s')))
         | NONE => NONE

fun foldl getc scanner =
  fn f =>
    fn acc =>
      let val getItem = scanner getc
      in fn s =>
            let fun iter acc NONE = f (NONE,acc)
                  | iter acc (SOME (v,s')) = iter (f (SOME v,acc)) (getItem s')
            in iter acc (getItem s)
            end
      end

fun proc_string (SOME c,acc) = acc^(String.str c)
  | proc_string (NONE,acc) = acc

fun proc_list (SOME c,acc) = c::acc
  | proc_list (NONE,acc) = List.rev acc

fun scanList scanner f acc =
    fn l => foldl List.getItem scanner f acc ([],l)

fun scanSubstring scanner f acc =
   fn l => foldl Substring.getc scanner f acc ([],l)

val ml_escchar = #"\\"
val ml_escs =
    [(#"\\",#"\\"), (#"\"",#"\""), (#"a",#"\a"),
     (#"b",#"\b"),  (#"t",#"\t"),  (#"n",#"\n"),
     (#"v",#"\v"),  (#"f",#"\f"),  (#"r",#"\r")]

fun scanmlunesc f acc = scanunesc "ML" ml_escchar ml_escs f acc
fun scanmlesc f acc = scanesc "ML" ml_escchar ml_escs f acc

fun unescMLString f acc = scanSubstring scanmlunesc f acc o Substring.full
fun unescMLList f acc = scanList scanmlunesc f acc

fun escMLString f acc = scanSubstring scanmlesc f acc o Substring.full
fun escMLList f acc = scanList scanmlesc f acc

val unescMLStringString = unescMLString proc_string ""
val unescMLListList = unescMLList proc_list []

val escMLStringString = escMLString proc_string ""
val escMLListList = escMLList proc_list []

val unescMLStringList = unescMLString proc_list []
val unescMLListString = unescMLList proc_string ""

val teststrs = ["a\\t\\abcd\\n", "\\\"abcd\\\""];
val testss = List.map unescMLStringString teststrs;
val testsl = List.map (String.implode o unescMLListList o String.explode) teststrs;
val testss' = List.map escMLStringString testss;
val testsl' = List.map escMLStringString testsl;
