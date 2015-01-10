load "Redblackmap";
load "GenericArray_sig";
load "GenericArraySlice_sig";
load "Word8Array";
load "Word8ArraySlice";
load "SparseArrays";

structure SparseArray =
 SparseArray
  (structure Array = Word8Array : GenericArray
   structure ArraySlice = Word8ArraySlice : GenericArraySlice)

open SparseArray

fun toList a =
   let val l = length a
       fun iter acc 0 = List.rev acc
         | iter acc i = iter ((sub (a,l-i)) :: acc) (i-1)
   in iter [] l
   end

val a3 = fromList (List.tabulate (10,Word8.fromInt));
val l3 = length a3;
val es3 = toList a3;

val a1 = array (10,0w0);
val l1 = length a1;
val es1 = toList a1;

val a2 = tabulate (10,fn i => Word8.fromInt i);
val l2 = length a2;
val es2 = toList a2;

val 0wxFF = sub(a1,10) handle Subscript => 0wxFF;
val 0wxFF = sub(a2,10) handle Subscript => 0wxFF;
val 0wxFF = sub(a3,10) handle Subscript => 0wxFF;

val a4 = array (10,0w0);
val _ = update (a4,0,0wxFF);
val _ = update (a4,1,0wxFE);
val _ = update (a4,2,0wxFD);
val _ = update (a4,9,0wxF6);
val _ = update (a4,4,0wxFB);
val true = (update (a4,10,0wxF5);false) handle Subscript => true;
val es4 = toList a4;

