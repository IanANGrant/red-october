signature UTF8 = sig

datatype transition = Process of Char.char -> transition
                    | Value of string * Word.word
                    | Error of string;

type elem = String.string

exception BadUTF8 of string

type ('a, 'b) reader = 'b -> ('a * 'b) option

val start : transition

val scanUTF8 : (char, 'a) reader -> (elem, 'a) reader
val scanUCS : (char, 'a) reader -> (word, 'a) reader
val UCStoUTF8String : word -> string
val UCSfromUTF8String : string -> word option
val size : string -> int
val size_ : string -> int
val padLeft : char -> int -> string -> string
val padRight : char -> int -> string -> string
val UTF8fromUTF8string : string -> string option

val scanUTF8Transition : ('b -> (char * 'b) option) -> 'b -> (string * 'b) option

end
