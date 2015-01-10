structure Event =
struct
   datatype event = 
       String of string * string
     | Word of string * word
     | Int of string * int
   type tx_msg = event
   type rx_msg = event

   fun toString (String (s,v)) = "String <"^s^",\""^v^"\">"
     | toString (Word (s,v)) = "Word <"^s^",0wx"^(Word.toString v)^">"
     | toString (Int (s,v)) = "Int <"^s^","^(Int.toString v)^">"
end
