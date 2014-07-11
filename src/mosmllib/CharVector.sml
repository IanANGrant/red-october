(* CharVector -- SML Basis Library, 1995, 2000-10-26 *)

type vector = string
type elem = Char.char

local 
    prim_val magic : 'a -> 'b = 1 "identity";
in
    val maxLen = Word8Vector.maxLen
	
    val fromList : elem list -> vector 		= magic Word8Vector.fromList
    val tabulate : int * (int -> elem) -> vector = magic Word8Vector.tabulate

    val length   : vector -> int  		= magic Word8Vector.length
    val sub      : vector * int -> elem         = magic Word8Vector.sub
    val update   : vector * int * elem -> vector 
						= magic Word8Vector.update
    val concat   : vector list -> vector        = magic Word8Vector.concat
    val find     : (elem -> bool) -> vector -> elem option 
	                                        =  magic Word8Vector.find
    val exists   : (elem -> bool) -> vector -> bool
	                                        =  magic Word8Vector.exists
    val all      : (elem -> bool) -> vector -> bool
	                                        =  magic Word8Vector.all

    val collate  : (elem * elem -> order) -> vector * vector -> order 
                                                = magic String.collate

    val app      : (elem -> unit) -> vector -> unit
						= magic Word8Vector.app
    val map      : (elem -> elem) -> vector -> vector
						= magic Word8Vector.map
    val findi    : (int * elem -> bool) -> vector -> (int * elem) option
	                                        =  magic Word8Vector.findi

    fun foldl (f : elem * 'b -> 'b) (e : 'b) (v : vector) : 'b
	= Word8Vector.foldl (magic f) e (magic v)

    fun foldr (f : elem * 'b -> 'b) (e : 'b) (v : vector) : 'b
	= Word8Vector.foldr (magic f) e (magic v)
	
    fun appi (f : int * elem -> unit) (v : vector) : unit
	= Word8Vector.appi (magic f) (magic v)

    fun mapi (f : int * elem -> elem) (v : vector) : vector
	= magic(Word8Vector.mapi (magic f) (magic v))

    fun foldli (f : int * elem * 'b -> 'b) (e : 'b) (v : vector) : 'b
        = Word8Vector.foldli (magic f) e (magic v)

    fun foldri (f : int * elem * 'b -> 'b) (e : 'b) (v : vector) : 'b
	= Word8Vector.foldri (magic f) e (magic v)

(*
    val foldl    : (elem * 'b -> 'b) -> 'b -> vector -> 'b
						= magic Word8Vector.foldl
    val foldr    : (elem * 'b -> 'b) -> 'b -> vector -> 'b
						= magic Word8Vector.foldr
	
    val appi     : (int * elem -> unit) -> vector * int * int option -> unit
						= magic Word8Vector.appi
    val foldli   : (int * elem * 'b -> 'b) -> 'b -> vector*int*int option -> 'b
						= magic Word8Vector.foldli
    val foldri   : (int * elem * 'b -> 'b) -> 'b -> vector*int*int option -> 'b
						= magic Word8Vector.foldri
*)
end

