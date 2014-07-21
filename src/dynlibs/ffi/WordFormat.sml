signature WordFormat =
sig
   type word
   val toString : word -> string
   val pp : ppstream -> word -> unit
end

functor WordFormat(structure Word : sig type word val wordSize : int val toString : word -> string end)
  : WordFormat 
       where type word = Word.word =
struct 
   type word = Word.word
   local
      val numhexdigs = Word.wordSize div 4 + (if Word.wordSize mod 4 = 0 then 0 else 1)
      val toString = (fn s => "0wx"^s) o
                        StringCvt.padLeft #"0" numhexdigs o
                       (String.translate (str o Char.toLower)) o
                       Word.toString
      fun ppword pps w0 = 
          let open PP
              fun ppw w = add_string pps (toString w)
          in
              begin_block pps INCONSISTENT 0;
              ppw w0;
              end_block pps
          end
   in
      val toString = toString
      val pp = ppword
   end
end
