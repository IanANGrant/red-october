load "Regex";
load "Substring";
load "Scanners";
load "Printers";
load "Evil";

fun consumer (a,s) = a^s

val match_decint = Int.scan StringCvt.DEC Substring.getc

val preproc = fn p => fn x => if p x then SOME x else NONE

val p = fn n => n >= 1 andalso n <= 32

val netmscanner = Scanners.atomicScanner match_decint (preproc p)

val netmprinter = Printers.atomicPrinter Int.toString

val res = netmscanner (Substring.all ("24 and garbage"))

val p = fn n => n >= 0 andalso n <= 255

val ipnscanner = Scanners.atomicScanner match_decint (preproc p)

val ipnprinter = Printers.atomicPrinter Int.toString

val p = fn n => n >= 0 andalso n <= 255

val Word8Scanner = Scanners.atomicScanner match_decint (preproc p)

val Word8Printer = Printers.atomicPrinter Int.toString

(*--------------------------------------------------------------------------------

   Some Greek Lego BRICS from 300BC Alexandria

+ val getOpt         : 'a option * 'a -> 'a 
+ val isSome         : 'a option -> bool 
+ val valOf          : 'a option -> 'a 
+ val filter         : ('a -> bool) -> 'a -> 'a option 
+ val map            : ('a -> 'b) -> 'a option -> 'b option
+ val app            : ('a -> unit) -> 'a option -> unit
+ val join           : 'a option option -> 'a option
+ val compose        : ('a -> 'b) * ('c -> 'a option) -> ('c -> 'b option)
+ val mapPartial     : ('a -> 'b option) -> ('a option -> 'b option)
+ val composePartial : ('a -> 'b option) * ('c -> 'a option) -> ('c -> 'b option)

----------------------------------------------------------------------------------*)

val anchor = "^"
val optws = "[ \\t]*"

fun delim rexp =
      Regex.getItem (Regex.regcomp (anchor^optws^rexp) []) []

val dots = delim ("\\.")

fun preproc n = SOME n

fun postproc [] = NONE
  | postproc (l : int list) =
       if List.length l = 4
          then SOME (List.rev l)
          else NONE

val IPAddrScan = Scanners.listScanner dots Word8Scanner preproc [] op :: postproc;
val res = IPAddrScan (Substring.all "192.168.0.1,192.168.1.1 and  garbage");
val SOME (res,rest) = res
val rest' = Substring.string rest;

val IPAddrPrinter = Printers.listPrinter
                       (fn c => fn (acc,()) => c (acc,"."))
                       ipnprinter
                       []
                       (fn (a::rest) => (a,rest)
                         | _ => raise Fail "IPAddrPrinter: impossible")

val ldelim = optws^","
val match_ldelim = Regex.getItem (Regex.regcomp (anchor^ldelim) []) []

fun lpreproc ipadd = SOME ipadd

fun lpostproc [] = NONE
  | lpostproc l = SOME (List.rev l)

val IPAddrListScan = Scanners.listScanner match_ldelim IPAddrScan lpreproc [] op :: lpostproc

val IPAddrListPrinter = Printers.listPrinter
                       (fn c => fn (acc,()) => c (acc,","))
                       IPAddrPrinter
                       []
                       (fn (a::rest) => (a,rest)
                         | _ => raise Fail "IPAddrListPrinter: impossible")

val res = IPAddrListScan (Substring.all "192. 168 . 0.1 , 192.168.1.1,192.168.0.1,192.168.2.1 and garbage");

val SOME (res,rest) = res
val rest' = Substring.string rest;

val res' =  IPAddrListPrinter consumer ("",res)

val lbrack = optws^"\\["
val rbrack = optws^"\\]"

val lbrackre = Regex.regcomp (anchor^lbrack) []
val rbrackre = Regex.regcomp (anchor^rbrack) []

val match_lbrack = Regex.getItem lbrackre []
val match_rbrack = Regex.getItem rbrackre []
val match_fld = IPAddrListScan 

val preproc = (fn l => SOME l)
          
fun postproc1 (elt2) = (SOME elt2)

fun postproc2 (elt2) = (SOME elt2)

val bracketedlist = Scanners.seqScanner
                      (Scanners.seqScanner
                           match_lbrack
                           match_fld
                           preproc preproc (fn (_,x) => x) postproc1)
                      match_rbrack
                      preproc preproc (fn (x,_) => x) postproc2;

val SOME (res,rest) = bracketedlist (Substring.all " [ 192.168.0.1,192.168.1.1 ] and garbage ");

val bracketedlistPrinter =
     Printers.seqPrinter 
        (Printers.seqPrinter
           (fn c => fn (acc,()) => c (acc,"["))
           IPAddrListPrinter (fn p => p))
        (fn c => fn (acc,()) => c (acc,"]"))
        (fn p => (((),p),()))

val res' =  bracketedlistPrinter consumer ("",res)

datatype addrlist = 
   AddrList of int list list
 | Addr of int list

fun preproc1 ipaddr = SOME (Addr ipaddr)

fun preproc2 ipaddrl = SOME (AddrList ipaddrl)

fun postproc l = SOME l

val IPScanner = Scanners.altScanner IPAddrScan bracketedlist preproc1 preproc2 postproc

val SOME (res,rest) = IPScanner (Substring.all " [ 192.168.0.1,192.168.1.1 ] and garbage ");
val SOME (res2,rest2) = IPScanner (Substring.all "192.168.1.1 and garbage ");

val IPPrinter = Printers.altPrinter
                 IPAddrPrinter
                 bracketedlistPrinter
                 (fn f1 => fn f2 => fn (Addr a) => f1 a
                                     | (AddrList a) => f2 a);

val res'' =  IPPrinter consumer ("",res);
val res2'' =  IPPrinter consumer ("",res2);

val netmdelim = optws^"/"

val netmdelimre = Regex.regcomp (anchor^netmdelim) []

val match_netmdelim = Regex.getItem netmdelimre []

val preproc = (fn l => SOME l)
          
fun postproc (elt2) = (SOME elt2)

val netmscan =
     Scanners.seqScanner
        match_netmdelim
        netmscanner
        preproc
        preproc
        (fn (_,x) => x)
        postproc

val netmPrinter =
     Printers.seqPrinter 
        (fn c => fn (acc,()) => c (acc, "/"))
        netmprinter
        (fn p => ((),p))

val dnetmscanner = Scanners.optScanner netmscan preproc

val dnetmPrinter = Printers.altPrinter
                   netmPrinter
                   (fn c => fn (acc,_) => acc)
                   (fn f1 => fn f2 => fn (SOME a) => f1 a
                                       | (NONE) => f2 ());

fun postproc (elt1,elt2) = (SOME (elt1,elt2))

val addrmaskscanner =
   Scanners.seqScanner
      IPAddrScan
      dnetmscanner
      preproc
      preproc
      (fn x => x)
      postproc;

val addrmaskPrinter =
     Printers.seqPrinter 
        IPAddrPrinter
        dnetmPrinter
        (fn p => p)

val SOME (res2,rest2) = addrmaskscanner (Substring.all "192.168.0.1/24 and garbage ");

val res2' = addrmaskPrinter consumer ("",res2)

val match_decint = Word.scan StringCvt.DEC Substring.getc

val match_hexint = Word.scan StringCvt.HEX Substring.getc

val preproc = fn p => fn x => if p x then SOME x else NONE

val p4 = fn n => n >= 0w0 andalso n <= 0wxf
val p8 = fn n => n >= 0w0 andalso n <= 0wxff
val p16 = fn n => n >= 0w0 andalso n <= 0wxffff
val p32 = fn n => n >= 0w0 andalso n <= 0wx3fffffff

val decWordScanner = fn p => Scanners.atomicScanner match_decint (preproc p)
val hexWordScanner = fn p => Scanners.atomicScanner match_hexint (preproc p)

val hexPref = optws^"0x"

val hexPrefRe = Regex.regcomp (anchor^hexPref) []

val match_hexPref = Regex.getItem hexPrefRe []

val preproc = (fn l => SOME l)
          
fun postproc (elt2) = (SOME elt2)

val hexWordScan = fn p =>
     Scanners.seqScanner
        match_hexPref
        (hexWordScanner p)
        preproc
        preproc
        (fn (_,x) => x)
        postproc

val decWordScan = decWordScanner

val WordScanner = fn p => Scanners.altScanner (hexWordScan p) (decWordScan p) preproc preproc postproc

val hexWordPrinter = fn c => fn (acc,n) => c (acc,Word.fmt StringCvt.HEX n)

val WordPrinter =
     Printers.seqPrinter 
        (fn c => fn (acc,()) => c (acc, "0x"))
        hexWordPrinter
        (fn p => ((),p))

val SOME (res2,rest2) = WordScanner p8 (Substring.all "0xc0 and garbage");

val res2' = WordPrinter consumer ("",res2)

val SOME (res3,rest3) = WordScanner p16 (Substring.all "0xc0ff and garbage");

val res3' = WordPrinter consumer ("",res3)

val SOME (res4,rest4) = WordScanner p4 (Substring.all "0xf and garbage");

val res4' = WordPrinter consumer ("",res4)

val SOME (res5,rest5) = WordScanner p32 (Substring.all "0xEFFFFF and garbage");

val res5' = WordPrinter consumer ("",res5)
