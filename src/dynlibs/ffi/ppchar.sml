(* Print chars in a way that doesn't cause the emacs comint buffer to
   show octal escape sequences for 'unprintable' characters such as ø! *)

val _ = (load "PP"; load "Char"; load "Strbase");

local
   fun chartostr c = ("#\""^(Strbase.toMLescape c)^"\"")
   fun ppchar pps c0 = 
       let open PP
           fun ppc c = add_string pps (chartostr c)
       in
           begin_block pps INCONSISTENT 0;
           ppc c0;
           end_block pps
    end
in
   val _ = Meta.installPP ppchar
end
