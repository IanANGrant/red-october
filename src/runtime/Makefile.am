bin_PROGRAMS = camlrunm

if DEBUG
  bin_PROGRAMS += camlrunmd
endif

EXTRA_PROGRAMS = camlrunmd

if CROSSW32
#   CW32_CFLAGS = -DCAMLRT -DWIN32 -D_CONSOLE -D_MBCS
#   CW32_PROGS = camlrt.dll
#   CW32_LDFLAGS = -mconsole
else !CROSSW32
#   CW32_CFLAGS =
#   CW32_PROGS =
#   CW32_LDFLAGS =
endif !CROSSW32

all-local: ../camlrunm$(EXEEXT)

../camlrunm$(EXEEXT) : camlrunm
	cp camlrunm ../camlrunm$(EXEEXT)

camlrunm_SOURCES = \
   alloc.c     expand.c    floats.c    intern.c    lexing.c    meta.c      parsing.c  str.c  \
   callback.c  extern.c    freelist.c  interncp.c  main.c      minor_gc.c  roots.c    sys.c  \
   compare.c   externcp.c  gc_ctrl.c   interp.c    major_gc.c  misc.c      runtime.c  unix.c \
   fail.c      ints.c      md5sum.c    mosml.c     signals.c   dynlib.c    fix_code.c hash.c \
   io.c        memory.c    stacks.c

# what about this DOS stuff?
# graph.c msdos.c

camlrunmd_SOURCES = $(camlrunm_SOURCES) debugger.c

camlrunm_CFLAGS = $(CW32_CFLAGS) -fno-defer-pop -fomit-frame-pointer -DTHREADED -Dunix
camlrunmd_CFLAGS = -g -I.. -DDEBUG $(camlrunm_CFLAGS)
camlrunm_LDFLAGS = @CAMLRTLDFLAGS@ -rdynamic -Wl,-rpath,@libdir@/mosml
camlrunmd_LDFLAGS = $(camlrunm_LDFLAGS) 

BUILT_SOURCES = primitives prims.c opnames.h jumptbl.h

nodist_camlrunm_SOURCES = $(BUILT_SOURCES) autoconfig.h

nodist_camlrunmd_SOURCES = $(BUILT_SOURCES) autoconfig.h

camlrunm_instdir=@includedir@/mosml

camlrunm_inst_HEADERS =     alloc.h     exec.h      gc.h        io.h       m.h        opnames.h  \
  s.h           unalignd.h \
  autoconfig.h  expand.h    globals.h   jumptbl.h   minor_gc.h  prims.h    signals.h  version.h  \
  callback.h    fail.h      instruct.h  major_gc.h  misc.h      reverse.h  stacks.h \
  config.h      freelist.h  interp.h    md5sum.h    mlvalues.h  roots.h    str.h \
  debugger.h    gc_ctrl.h   intext.h    memory.h    mosml.h     runtime.h  sys.h

CLEANFILES = $(BUILT_SOURCES)

PRIMS = compare.c extern.c   externcp.c floats.c gc_ctrl.c hash.c   \
        intern.c  interncp.c interp.c   ints.c   io.c      lexing.c \
        meta.c    parsing.c  str.c      sys.c    mosml.c   unix.c  \
        md5sum.c  callback.c dynlib.c

primitives : $(PRIMS)
	@SED@ -n -e '/\/\* ML \*\//s/.* \([a-zA-Z0-9_][a-zA-Z0-9_]*\) *(.*/\1/p' \
                   $(PRIMS) > primitives2
	sh -c 'if cmp -s primitives primitives2; \
        then rm primitives2; \
        else mv primitives2 primitives; \
        fi'

prims.c : primitives
	(echo '#include "mlvalues.h"'; \
	 echo '#include "prims.h"'; \
	 @SED@ -e 's/.*/extern value &();/' primitives; \
	 echo 'c_primitive cprim[] = {'; \
	 @SED@ -e 's/.*/  &,/' primitives; \
	 echo '  0 };'; \
         echo 'char * names_of_cprim[] = {'; \
	 @SED@ -e 's/.*/  "&",/' primitives; \
	 echo '  0 };') > prims.c

opnames.h : instruct.h
	@SED@ -e '/\/\*/d' \
            -e 's/enum /char * names_of_/' \
            -e 's/{$$/[] = {/' \
            -e 's/\([A-Z][A-Z_0-9]*\)/"\1"/g' instruct.h > opnames.h

jumptbl.h : instruct.h
	@AWK@ -f ../tools/make-jumptbl instruct.h > jumptbl.h
