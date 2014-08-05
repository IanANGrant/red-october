datatype term =
     Fn of string * term list
  | Var of string

datatype outcome =
     No
   | Yes of (string * term) list

val goal : string frag list -> term
val rules : string frag list -> (term * term list) list
val prolog : (term * term list) list -> term -> outcome
