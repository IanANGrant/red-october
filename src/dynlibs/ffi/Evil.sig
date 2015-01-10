type vec_slice = CharVectorSlice.slice
type arr_slice = CharArraySlice.slice
 
val CharArraySliceToCharVectorSlice : arr_slice -> vec_slice
val CharVectorSliceToCharArraySlice : vec_slice -> arr_slice

