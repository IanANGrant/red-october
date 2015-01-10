val _ = load "MemDebug";
val _ = load "MappedWord8Array";
val _ = load "MappedWord8ArraySlice";

   local open Dynlib

         val slfh =
             dlopen {lib = "",
                     flag = Dynlib.RTLD_LAZY,
                     global = false }

      fun slfsym s = dlsym slfh s
      fun slfsymp s = cptr (slfsym s)

         val ih =
             dlopen {lib = "libcinfo.so",
                     flag = Dynlib.RTLD_LAZY,
                     global = false }
      fun sym s = dlsym ih s
      fun symp s = cptr (sym s)
   in
      val freep = symp "my_free"
      val memdebug_setp = symp "memdebug_set"
      val memdebug_final1p = symp  "memdebug_finalize1"
      val memdebug_final2p = symp  "memdebug_finalize2"
   end

val call_final1 : Dynlib.cptr -> unit
   = Lightning32.app1 memdebug_final1p

fun finalize (arr : MappedWord8Array.array) =
   let val cptr : Dynlib.cptr = Obj.magic (Obj.obj_field (Obj.repr arr) 0)
   in call_final1 cptr
   end

val final1paddr = ValRepr.wordFromCptr(memdebug_final1p);
val final2paddr = ValRepr.wordFromCptr(memdebug_final2p);

val _ = MemDebug.debug_set true;

val len = 1024;

val membuf = MappedWord8Array.malloc (len);
val addr = ValRepr.wordFromCptr (membuf);
val arr = MappedWord8Array.alloc_final (freep,1,membuf,len);

val oarr' = Obj.repr arr;
val oarrsz = Obj.obj_size oarr';
val oarrtag = Obj.obj_tag oarr';

val oarrcptr : Dynlib.cptr = Obj.magic (Obj.obj_field oarr' 0);
val oarr = Obj.obj_field oarr' 0;
val oarsz =  Obj.obj_size oarr;
val oartag = Obj.obj_tag oarr;

val fnlcptr : Dynlib.cptr = Obj.magic (Obj.obj_field oarr 0);
val fnladdr = ValRepr.wordFromCptr(fnlcptr);

val ar = Obj.obj_field oarr 1;
val arsz =  Obj.obj_size ar;
val arcptr : Dynlib.cptr = Obj.magic (Obj.obj_field ar 0);
val arraddr = ValRepr.wordFromCptr(MappedWord8Array.get_cptr arr);
val arcptraddr = ValRepr.wordFromCptr(arcptr);

val arlencptr : Dynlib.cptr = Obj.magic (Obj.obj_field oarr 2);
val arlenv = ValRepr.wordFromCptr(arlencptr);

val ffcptr : Dynlib.cptr = Obj.magic (Obj.obj_field oarr 3);
val ffaddr = ValRepr.wordFromCptr(ffcptr);

val blkcptr : Dynlib.cptr = Obj.magic (Obj.obj_field oarr 4);
val blkaddr = ValRepr.wordFromCptr(blkcptr);

val lencptr : Dynlib.cptr = Obj.magic (Obj.obj_field oarr 5);
val lenv = ValRepr.wordFromCptr(lencptr);

val membuf' = MappedWord8Array.malloc (len);
val addr = ValRepr.wordFromCptr (membuf');
val arr = MappedWord8Array.alloc_final (freep,1,membuf',len);

val _ = MemDebug.report_alloc("testing (pre gc)");

val _ = MemDebug.gc_major();

val _ = MemDebug.report_alloc("testing (post gc)");

val _ = finalize arr;

val _ = MemDebug.report_alloc("testing (post finalize)");

val arr = MappedWord8Array.array (len,0w0);
val addr = ValRepr.wordFromCptr(MappedWord8Array.get_cptr arr);
val lenarr = (MappedWord8Array.length arr);
val arrl = MappedWord8Array.foldr op :: [] arr;

val slc = MappedWord8ArraySlice.full arr;
val _ = MappedWord8ArraySlice.modifyi (fn (i,w) => w + (Word8.fromInt i)) slc;
val arrl' = MappedWord8Array.foldr op :: [] arr;
val _ = MappedWord8ArraySlice.modifyi (fn (i,w) => w - (Word8.fromInt i)) slc;
val arrl'' = MappedWord8Array.foldr op :: [] arr;

val arr2 = MappedWord8Array.tabulate (len,fn w => Word8.fromInt (w mod 0x100));

val _ = MemDebug.dumpb (MappedWord8Array.get_cptr arr2) 256;

val addr2 = ValRepr.wordFromCptr(MappedWord8Array.get_cptr arr2);
val lenarr2 = (MappedWord8Array.length arr2);
val arr2l = MappedWord8Array.foldr op :: [] arr2;

val _ = finalize arr;
val _ = finalize arr2;

val _ = MemDebug.report_alloc("testing (end)");
