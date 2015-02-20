   local open Dynlib
    val dlxh = Dynlib.dlopen {lib = "",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }
         val ih =
             dlopen {lib = "libcinfo.so",
                     flag = Dynlib.RTLD_LAZY,
                     global = false }
      fun symp s = cptr (dlsym ih s)
      val setp = symp "memdebug_set"
      val dumpbp = symp "memdebug_dumpbytes"
      val dumpwp = symp "memdebug_dumpwords"
      val report_allocp = symp "memdebug_report_alloc"

   in
      val debug_set : bool -> unit = Lightning32.app1 setp
      val dumpb : Dynlib.cptr -> int -> unit = Lightning32.app2 dumpbp
      val dumpw : Dynlib.cptr -> int -> unit = Lightning32.app2 dumpwp
      val report_alloc : string -> unit = Lightning32.app1 report_allocp
      val gc_minor : unit-> unit = Lightning32.app1 (Dynlib.cptr (Dynlib.dlsym dlxh "gc_minor"))
      val gc_major : unit-> unit = Lightning32.app1 (Dynlib.cptr (Dynlib.dlsym dlxh "gc_major"))
      val gc_full_major : unit-> unit = Lightning32.app1 (Dynlib.cptr (Dynlib.dlsym dlxh "gc_full_major"))
   end

fun dumpb_offs cptr offs len =
   let in
    dumpb (ValRepr.cptr_offs cptr offs) len
   end

fun dumpw_offs cptr offs len =
    dumpw (ValRepr.cptr_offs cptr offs) len
