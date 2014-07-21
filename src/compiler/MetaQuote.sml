val quotedef_table = (Hasht.new 50 : (string, ((Lexing.lexbuf -> Parser.token) * string)) Hasht.t);

val clear_quotedefs = Hasht.clear quotedef_table;

fun is_quotedef_name s =
   let val _ = Hasht.find quotedef_table s
   in true end 
   handle Subscript => false;

fun quotedef_decl s =
   Hasht.find quotedef_table s;

fun new_quotedef_name s t =
   Hasht.insert quotedef_table s t;

fun quotedef_names () = 
   Hasht.fold (fn n => fn _ => fn l => n::l) [] quotedef_table;
