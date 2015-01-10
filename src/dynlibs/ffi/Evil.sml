(* In case you're wondering, this is so that we can do Regexp matches
   on CharArrays without having to repeatedly copy them to and from
   CharVectors. *)

prim_val magic : 'a -> 'b = 1 "identity"

prim_type vector_
type array_ = vector_ ref

type vec_slice = substring
type arr_slice = CharArraySlice.slice

val from_vector : vector_ -> array_ = fn v => magic (ref v)
val to_vector : array_ -> vector_ = fn v => magic (!v)

val from_vec_slice : vec_slice -> vector_ * int * int = magic
val to_vec_slice : vector_ * int * int -> vec_slice = magic

val from_arr_slice : arr_slice -> array_ * int * int = magic
val to_arr_slice : array_ * int * int -> arr_slice = magic

fun CharArraySliceToCharVectorSlice (s : arr_slice) =
   let val (arr,offs,len) = from_arr_slice s
   in to_vec_slice (to_vector arr,offs,len)
   end

fun CharVectorSliceToCharArraySlice (s : vec_slice) =
   let val (vec,offs,len) = from_vec_slice s
   in to_arr_slice (from_vector vec,offs,len)
   end

(* 
load "Evil";

val slc = Evil.CharVectorSliceToCharArraySlice (Substring.full "abcd");
val subslc = CharArraySlice.subslice (slc,1,NONE);
val substr = Evil.CharArraySliceToCharVectorSlice subslc;
val str = Substring.string substr;

(* Why is it called Evil? Because a pure functional program can end up
   with hidden state if it calls something that makes injudicious use
   of this unit. Watch this: *)

fun splat s =
   let val cas = Evil.CharVectorSliceToCharArraySlice (Substring.full s)
   in CharArraySlice.update (cas,0,#"*")
   end

val s = "abc";
val _ = splat s;
val s' = s;

*)

