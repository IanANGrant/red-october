# Makefile for dynamic foreign function interface

# The runtime system must be compiled with support for dynamic libraries.
# The GNU ffi package must be installed.

# Where to find the GNU ffi header files and the compiled library

FFILIBDIR=/home/ian3/usr/lib
FFIINCDIR=$(FFILIBDIR)/libffi-3.0.13/include/

LIGHTNINGLIBDIR=/home/ian3/usr/lib
LIGHTNINGINCDIR=/home/ian3/usr/include

ADDINCDIRS=-I$(FFIINCDIR) -I$(LIGHTNINGINCDIR)

include ../../Makefile.inc

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I$(INCDIR) $(ADDINCDIRS) -I.

MOSMLTOOLS=camlrunm $(TOOLDIR)

ROOTDIR:=../../

MOSMLC=$(ROOTDIR)/camlrunm $(ROOTDIR)/mosmlcmp -stdlib $(ROOTDIR)/mosmllib -P default
MOSMLL=$(ROOTDIR)/camlrunm $(ROOTDIR)/mosmllnk -stdlib $(ROOTDIR)/mosmllib -P default
MOSMLLEX=$(ROOTDIR)/camlrunm $(ROOTDIR)/mosmllex
MOSMLYACC=$(ROOTDIR)/mosmlyac/mosmlyac

# MOSMLC=mosmlc -orthodox -c
# MOSMLL=mosmlc
# MOSMLLEX=mosmllex
# MOSMLYACC=mosmlyac

all: libmffi.so libcinfo.so Ffi.uo Jit.uo liblgtng.so Lightning32.uo	\
    Lightning32.ui Lightning64.uo Lightning64.ui StaticWord8Array.uo	\
    StaticWord8Array.ui StaticBuffer.ui StaticBuffer.uo			\
    StaticStruct.uo MappedStruct.uo MappedNativeRegister.uo		\
    MappedNativeWordRegister.uo MappedNativeAddressRegister.uo		\
    MappedRegister.uo ObjRepr.uo RealRepr.uo WordRepr.uo RingBuffer.uo	\
    GenericWord.ui SplitFifo.uo SigQFifo.uo FifoBuffer.uo ITimer.uo	\
    SigSet.uo SigAction.uo SigHandler.uo FDSet.uo Fcntl.uo Shm.uo	\
    MMap.uo PSelect.uo MappedFifo.uo ShmMap.uo MemDebug.uo		\
    MappedWord8Array.uo MappedWord8ArraySlice.uo Event.ui testcmd	\
    testcmd_loader DoubleMappedWord8Array.uo DoubleMappedWord8Array.ui	\
    MappedSocket.uo MappedSocket.ui RedBlackMap.uo PolyRedBlackMap.uo	\
    ValRepr.uo Values.uo StaticWord8ArraySlice.uo DuplexChannel.uo	\
    CParser.uo CPPLexer.uo GrammarParser.uo GrammarLexer.uo		\
    SparseArrays.uo Evil.uo Scanners.uo Printers.uo ArrayPatch.uo	\
    IPv4Packet.uo IPv4Consts.uo TCPConsts.uo GrammarSyntax.uo		\
    CSyntax.uo RewriteLexer.uo RewriteParser.uo RewriteMain.uo		\
    Rewrite.uo CPPInfoParser.uo CPPInfoLexer.uo Location2.uo Hasht.uo	\
    WordPrim.uo MachineWord.uo WordN.uo AbstractMachineWord.uo		\
    ArraySliceWordEnc.uo VectorSliceWordEnc.uo WordEncSig.ui		\
    PrimEncSig.ui PrimEnc.uo VectorWordEnc.uo Globals.uo TestWord.uo	\
    WordFormat.uo CompTestWord.uo SpeedTest.uo WordOpsSig.ui		\
    WordOps.uo GenericWord.ui Rot13.uo BitSet.uo PrologParser.uo	\
    PrologLexer.uo PrologSyntax.uo Prolog.uo testlightning

MLMODULES = Lightning32.ui Lightning32.uo Lightning64.ui		\
            Lightning64.uo StaticWord8Array.uo StaticWord8Array.ui	\
            StaticWord8Array.sig Ffi.ui Ffi.uo Jit.ui Jit.uo Ffi.sig	\
            Jit.sig StaticBuffer.uo StaticBuffer.ui StaticBuffer.sig	\
            StaticWord8ArraySlice.uo StaticWord8ArraySlice.ui		\
            StaticWord8ArraySlice.sig SigSet.uo SigSet.ui FDSet.uo	\
            FDSet.ui MemDebug.ui MemDebug.uo ValRepr.ui ValRepr.uo	\
            PSelect.ui PSelect.uo MappedWord8Array.ui			\
            MappedWord8Array.uo MappedWord8ArraySlice.ui		\
            MappedWord8ArraySlice.uo ITimer.ui ITimer.uo WordRepr.uo	\
            WordRepr.ui Values.uo Values.ui MappedStruct.uo		\
            MappedStruct.ui MappedNativeRegister.uo			\
            MappedNativeRegister.ui MappedNativeWordRegister.uo		\
            MappedNativeWordRegister.ui MappedNativeAddressRegister.uo	\
            MappedNativeAddressRegister.ui MappedRegister.uo		\
            MappedRegister.ui ObjRepr.uo ObjRepr.ui RealRepr.uo		\
            RealRepr.ui FifoBuffer.ui FifoBuffer.uo GenericArray.ui	\
            GenericArray.uo GenericArraySlice.ui GenericArraySlice.uo GenericWord.uo GenericWord.ui SysErr.uo SysErr.ui 

testlightning: testlightning.c
	gcc testlightning.c  `pkg-config --cflags --libs glib-2.0` \
              -I./lightning-2.0.4/include -Wl,-rpath,./debug -L./debug \
              -llightning -Wl,-rpath,/home/ian3/usr/lib -o testlightning

jsexample : jsexample.cpp
	g++ `js24-config --cflags` `js24-config --libs` -Wno-invalid-offsetof -o jsexample  jsexample.cpp

install:
	$(INSTALL_DATA) libmffi.so $(DESTDIR)$(LIBDIR)
	$(INSTALL_DATA) libcinfo.so $(DESTDIR)$(LIBDIR)
	$(INSTALL_DATA) liblgtng.so $(DESTDIR)$(LIBDIR)
	$(INSTALL_DATA) $(MLMODULES) $(DESTDIR)$(LIBDIR)

ffi.o: ffi.c mffi.h mem.h types.h
	$(CC) $(CFLAGS) -c -o ffi.o ffi.c

jit: jit.c mffi.h mem.h types.h
	$(CC) $(CFLAGS)  -DTEST_PROGRAM -llightning -o jit jit.c

jit.o: jit.c mffi.h mem.h types.h
	$(CC) $(CFLAGS) -c -o jit.o jit.c

svec.o: svec.c mffi.h mem.h
	$(CC) $(CFLAGS) -c -o svec.o svec.c

mem.o: mem.c mffi.h
	$(CC) $(CFLAGS) `pkg-config --cflags glib-2.0` -c -o mem.o mem.c

types.o: types.c mffi.h mem.h types.h 
	$(CC) $(CFLAGS) -c -o types.o types.c

libmffi.so: svec.o jit.o mem.o ffi.o types.o
	$(DYNLD) $(ADDLDLFAGS) -Wl,-rpath,$(FFILIBDIR) `pkg-config --libs glib-2.0` \
          -o libmffi.so -lffi -llightning mem.o jit.o svec.o ffi.o types.o

lgtng.o: lgtng.c lgt32.c lgt64.c
	$(CC) $(CFLAGS) -DCAMLRUNPATH="\"$(BINDIR)/camlrunm\"" -c -o lgtng.o lgtng.c

sigset.o: sigset.c
	$(CC) $(CFLAGS) -c sigset.c -o sigset.o

fdset.o: fdset.c
	$(CC) $(CFLAGS) -c fdset.c -o fdset.o

fcntl.o: fcntl.c
	$(CC) $(CFLAGS) -c fcntl.c -o fcntl.o

mmap.o: mmap.c
	$(CC) $(CFLAGS) -c mmap.c -o mmap.o

memdebug.o: memdebug.c
	$(CC) $(CFLAGS) -c memdebug.c -o memdebug.o

sigevent.o: sigevent.c
	$(CC) $(CFLAGS) -c sigevent.c -o sigevent.o

siginfo.o: siginfo.c
	$(CC) $(CFLAGS) -c siginfo.c -o siginfo.o

sigaction.o: sigaction.c
	$(CC) $(CFLAGS) -c sigaction.c -o sigaction.o

rtsignals.o: rtsignals.c
	$(CC) $(CFLAGS) -c rtsignals.c -o rtsignals.o

syserr.o: syserr.c
	$(CC) $(CFLAGS) -c syserr.c -o syserr.o

heap.o: heap.c
	$(CC) $(CFLAGS) -c heap.c -o heap.o

PINDIR = pin-2.13-62732-gcc.4.4.7-linux
XEDEX = $(PINDIR)/extras/xed2-ia32
XEDOBJ = $(XEDEX)/examples
XEDOBJS = $(XEDOBJ)/xed-examples-util.o $(XEDOBJ)/xed-dot-prep.o $(XEDOBJ)/xed-dot.o

xed-enc-32.o : xed-enc-32.c
	$(CC) $(CFLAGS) -MMD -I $(XEDEX)/include -I $(XEDEX)/examples -c $^ -o $@

xed-enc-32 : xed-enc-32.o $(XEDOBJS)
	$(CC) -o $@ $^ $(XEDEX)/lib/libxed.a

libcinfo.so: fdset.o sigevent.o rtsignals.o sigset.o fcntl.o mmap.o memdebug.o mem.o \
             siginfo.o sigaction.o syserr.o xed-enc-32.o heap.o
	$(DYNLD) $(ADDLDLFAGS) -Wl,-rpath,$(FFILIBDIR) `pkg-config --libs glib-2.0` \
         -lrt -o libcinfo.so fdset.o sigaction.o siginfo.o sigevent.o rtsignals.o \
                 sigset.o fcntl.o mem.o mmap.o memdebug.o syserr.o xed-enc-32.o heap.o

liblgtng.so: lgtng.o
	$(DYNLD) $(ADDLDLFAGS) -o liblgtng.so -llightning lgtng.o 

Lightning32.ui : Lightning32.sig
	$(MOSMLC) -imptypes Lightning32.sig

Lightning32.uo : Lightning32.sml Lightning32.ui
	$(MOSMLC) -imptypes Lightning32.sml

Lightning64.ui : Lightning64.sig
	$(MOSMLC) -imptypes Lightning64.sig

Lightning64.uo : Lightning64.sml Lightning64.ui
	$(MOSMLC) -imptypes Lightning64.sml

StaticWord8Array.ui : StaticWord8Array.sig
	$(MOSMLC) -imptypes StaticWord8Array.sig

StaticWord8Array.uo : StaticWord8Array.sml StaticWord8Array.ui
	$(MOSMLC) -imptypes StaticWord8Array.sml

MappedWord8Array.ui :  MappedWord8Array.sig 
	$(MOSMLC) -imptypes  MappedWord8Array.sig 

MappedWord8Array.uo : MappedWord8Array.ui ValRepr.ui MappedWord8Array.sml 
	$(MOSMLC) -imptypes ValRepr.ui MappedWord8Array.sml 

MappedWord8ArraySlice.ui :  MappedWord8Array.ui MappedWord8ArraySlice.sig 
	$(MOSMLC) -imptypes  MappedWord8ArraySlice.sig 

MappedWord8ArraySlice.uo : MappedWord8Array.ui MappedWord8ArraySlice.ui MappedWord8ArraySlice.sml 
	$(MOSMLC) -imptypes  MappedWord8ArraySlice.sml 

MappedSocket.ui : MappedSocket.sig MappedWord8Array.ui
	$(MOSMLC) -imptypes  MappedSocket.sig

MappedSocket.uo : MappedSocket.sml MappedWord8Array.ui MappedSocket.ui
	$(MOSMLC) -imptypes  MappedSocket.sml 


StaticBuffer.ui : StaticBuffer.sig
	$(MOSMLC) -imptypes StaticBuffer.sig

StaticBuffer.uo : StaticBuffer.sml StaticBuffer.ui
	$(MOSMLC) -imptypes StaticBuffer.sml

GenericWord.ui GenericWord.uo : GenericWord.sml
	$(MOSMLC) -imptypes -toplevel GenericWord.sml

RealRepr.ui : RealRepr.sig
	$(MOSMLC) -imptypes RealRepr.sig

RealRepr.uo : RealRepr.sml RealRepr.ui
	$(MOSMLC) -imptypes RealRepr.sml

WordRepr.uo WordRepr.ui : WordRepr.sml
	$(MOSMLC) -imptypes WordRepr.sml

StaticWord8ArraySlice.ui : StaticWord8ArraySlice.sig
	$(MOSMLC) -imptypes StaticWord8ArraySlice.sig

StaticWord8ArraySlice.uo : StaticWord8ArraySlice.ui StaticWord8ArraySlice.sml 
	$(MOSMLC) -imptypes StaticWord8ArraySlice.sml

GenericArray.ui GenericArray.uo : GenericArray.sml
	$(MOSMLC) -imptypes -toplevel GenericArray.sml

GenericArraySlice.ui GenericArraySlice.uo : GenericArraySlice.sml
	$(MOSMLC) -imptypes -toplevel GenericArraySlice.sml

ArrayPatch_sig.ui : ArrayPatch_sig.sml
	$(MOSMLC) -imptypes -toplevel ArrayPatch_sig.sml

RingBuffer.uo RingBuffer.ui : RingBuffer.sml GenericWord.ui GenericArraySlice.ui GenericArray.ui
	$(MOSMLC) -imptypes -toplevel GenericWord.ui GenericArraySlice.ui GenericArray.ui RingBuffer.sml

ValRepr.ui ValRepr.uo : ValRepr.sml
	$(MOSMLC) -imptypes -toplevel ValRepr.sml

Values.ui : Values.sig
	$(MOSMLC) -imptypes Values.sig

Values.uo : Values.ui MemDebug.ui Values.sml
	$(MOSMLC) -imptypes MemDebug.ui Values.sml

MappedNativeRegister.ui : MappedWord8ArraySlice.ui ValRepr.ui MappedNativeRegister.sig
	$(MOSMLC) -imptypes ValRepr.ui MappedNativeRegister.sig

MappedNativeRegister.uo : MappedWord8ArraySlice.ui ValRepr.ui MappedNativeRegister.ui \
                          MappedNativeRegister.sml
	$(MOSMLC) -imptypes ValRepr.ui MappedNativeRegister.sml

MappedNativeWordRegister.ui : MappedWord8ArraySlice.ui ValRepr.ui MappedNativeRegister.ui \
                              MappedNativeWordRegister.sig
	$(MOSMLC) -imptypes ValRepr.ui MappedNativeRegister.ui MappedNativeWordRegister.sig

MappedNativeWordRegister.uo : MappedWord8ArraySlice.ui ValRepr.ui MappedNativeRegister.ui \
                          MappedNativeWordRegister.ui MappedNativeWordRegister.sml
	$(MOSMLC) -imptypes ValRepr.ui MappedNativeRegister.ui MappedNativeWordRegister.sml

MappedNativeAddressRegister.ui : MappedWord8ArraySlice.ui ValRepr.ui MappedNativeRegister.ui \
                              MappedNativeAddressRegister.sig
	$(MOSMLC) -imptypes ValRepr.ui MappedNativeRegister.ui MappedNativeAddressRegister.sig

MappedNativeAddressRegister.uo : MappedWord8ArraySlice.ui ValRepr.ui MappedNativeRegister.ui \
                          MappedNativeAddressRegister.ui MappedNativeAddressRegister.sml
	$(MOSMLC) -imptypes ValRepr.ui MappedNativeRegister.ui MappedNativeAddressRegister.sml

MappedRegister.ui : MappedWord8ArraySlice.ui ValRepr.ui MappedRegister.sig
	$(MOSMLC) -imptypes ValRepr.ui MappedRegister.sig

MappedRegister.uo : MappedWord8ArraySlice.ui ValRepr.ui MappedRegister.ui MappedRegister.sml
	$(MOSMLC) -imptypes ValRepr.ui MappedRegister.sml

SplitFifo.ui SplitFifo.uo : SplitFifo.sml ValRepr.ui GenericWord.ui GenericArraySlice.ui \
                    GenericArray.ui MappedNativeRegister.ui
	$(MOSMLC) -imptypes -toplevel MappedNativeRegister.ui GenericArraySlice.ui \
                    GenericArray.ui ValRepr.ui GenericWord.ui SplitFifo.sml

SigQFifo.ui SigQFifo.uo : SigQFifo.sml ValRepr.ui GenericWord.ui GenericArraySlice.ui \
                    MemDebug.ui GenericArray.ui MMap.ui MappedNativeRegister.ui
	$(MOSMLC) -imptypes -toplevel MappedNativeRegister.ui GenericArraySlice.ui \
                    MemDebug.ui GenericArray.ui ValRepr.ui GenericWord.ui MMap.ui SigQFifo.sml

FifoBuffer.ui FifoBuffer.uo : FifoBuffer.sml GenericWord.ui GenericArraySlice.ui \
                              GenericArray.ui MappedRegister.uo
	$(MOSMLC) -imptypes -toplevel MappedRegister.ui GenericArraySlice.ui GenericArray.ui \
                                      GenericWord.ui FifoBuffer.sml

AbsObjRepr.ui AbsObjRepr.uo : AbsObjRepr.sml ValRepr.ui
	$(MOSMLC) -imptypes -toplevel ValRepr.ui AbsObjRepr.sml

ObjRepr.ui ObjRepr.uo : ObjRepr.sml FifoBuffer.ui ValRepr.ui AbsObjRepr.ui
	$(MOSMLC) -imptypes -toplevel FifoBuffer.ui ValRepr.ui AbsObjRepr.ui ObjRepr.sml

RedBlackMap.ui RedBlackMap.uo : RedBlackMap.sml ObjRepr.ui
	$(MOSMLC) -imptypes -toplevel ObjRepr.ui RedBlackMap.sml

PolyRedBlackMap.uo PolyRedBlackMap.ui : AbsObjRepr.ui PolyRedBlackMap.sml
	$(MOSMLC) -imptypes -toplevel AbsObjRepr.ui PolyRedBlackMap.sml

StaticStruct.ui StaticStruct.uo : StaticStruct.sml GenericWord.ui FifoBuffer.ui StaticWord8Array.ui \
                                  GenericArraySlice.ui GenericArray.ui StaticWord8ArraySlice.ui \
                                  StaticWord8ArraySlice.uo
	 $(MOSMLC) -imptypes -toplevel GenericArraySlice.ui GenericArray.ui GenericWord.ui \
                    StaticWord8Array.ui FifoBuffer.ui StaticWord8ArraySlice.ui StaticStruct.sml


MappedStruct.ui MappedStruct.uo : MappedStruct.sml GenericWord.ui MappedWord8Array.ui \
                                  GenericArraySlice.ui GenericArray.ui MappedWord8ArraySlice.ui \
                                  MappedWord8ArraySlice.uo FifoBuffer.ui ValRepr.ui
	 $(MOSMLC) -imptypes -toplevel GenericArraySlice.ui GenericArray.ui GenericWord.ui \
                    ValRepr.ui MappedWord8Array.ui FifoBuffer.ui MappedWord8ArraySlice.ui MappedStruct.sml

SysErr.uo SysErr.ui : SysErr.sml
	 $(MOSMLC) -imptypes SysErr.sml

ITimer.uo ITimer.ui : ITimer.sml ValRepr.ui MappedStruct.ui SysErr.ui
	 $(MOSMLC) -imptypes -toplevel ValRepr.ui MappedStruct.ui SysErr.ui ITimer.sml

SigSet.uo SigSet.ui : MappedStruct.ui SysErr.ui ValRepr.ui ITimer.ui SigSet.sml 
	 $(MOSMLC) -imptypes -toplevel MappedStruct.ui SysErr.ui ValRepr.ui ITimer.ui SigSet.sml

FDSet.uo FDSet.ui : MappedStruct.ui FDSet.sml SysErr.ui
	 $(MOSMLC) -imptypes -toplevel MappedStruct.ui SysErr.ui FDSet.sml

Shm.uo Shm.ui : Shm.sml FDSet.ui MappedStruct.ui SysErr.ui SigAction.ui
	 $(MOSMLC) -imptypes -toplevel FDSet.ui MappedStruct.ui SysErr.ui SigAction.ui Shm.sml

MMap.uo MMap.ui : SysErr.ui ValRepr.ui MappedWord8Array.ui FDSet.ui Fcntl.ui MMap.sml
	 $(MOSMLC) -imptypes -toplevel SysErr.ui ValRepr.ui MappedWord8Array.ui \
                      FDSet.ui Fcntl.ui MMap.sml

MemDebug.uo MemDebug.ui : ValRepr.ui MemDebug.sml
	 $(MOSMLC) -imptypes ValRepr.ui MemDebug.sml

MappedFifo.ui MappedFifo.uo : SplitFifo.ui SysErr.ui ValRepr.ui MappedFifo.sml 
	$(MOSMLC) -imptypes SplitFifo.ui SysErr.ui ValRepr.ui MappedFifo.sml 

ShmMap.uo ShmMap.ui : ShmMap.sml SysErr.ui ValRepr.ui MappedFifo.ui FifoBuffer.ui MMap.ui Shm.ui DoubleMappedWord8Array.uo
	 $(MOSMLC) -imptypes SysErr.ui ValRepr.ui MappedFifo.ui FifoBuffer.ui MMap.ui Shm.ui DoubleMappedWord8Array.ui ShmMap.sml

DoubleMappedWord8Array.ui : DoubleMappedWord8Array.sig MMap.ui
	 $(MOSMLC) MMap.ui DoubleMappedWord8Array.sig

DoubleMappedWord8Array.uo : DoubleMappedWord8Array.ui DoubleMappedWord8Array.sml \
                                                       Shm.ui MMap.ui ValRepr.ui
	 $(MOSMLC)  ValRepr.ui Shm.ui MMap.ui DoubleMappedWord8Array.sml

Event.ui Event.uo : Event.sml 
	$(MOSMLC) -imptypes Event.sml 

testcmd : testcmd.sml Event.ui SplitFifo.ui DuplexChannel.ui ObjRepr.ui ValRepr.ui ShmMap.ui SysErr.ui \
            MMap.ui Shm.ui Fcntl.ui SigHandler.ui SigAction.ui SigSet.ui ITimer.ui FDSet.ui PSelect.ui
	mosmlc -imptypes -standalone -toplevel -o testcmd Event.ui SplitFifo.ui DuplexChannel.ui \
             ObjRepr.ui ValRepr.ui ShmMap.ui SysErr.ui SigSet.ui ITimer.ui FDSet.ui PSelect.ui \
             MMap.ui Shm.ui Fcntl.ui SigHandler.ui SigAction.ui testcmd.sml

testcmd_loader : testcmd_loader.sml Event.ui SplitFifo.ui DuplexChannel.ui ObjRepr.ui ITimer.ui SysErr.ui \
                 ValRepr.ui ShmMap.ui MMap.ui Shm.ui Fcntl.ui SigHandler.ui SigAction.ui SigSet.ui \
                 FDSet.ui PSelect.ui
	mosmlc -imptypes -standalone -toplevel -o testcmd_loader Event.ui SplitFifo.ui \
                DuplexChannel.ui ObjRepr.ui SigSet.ui ITimer.ui FDSet.ui PSelect.ui \
                SysErr.ui ValRepr.ui ShmMap.ui MMap.ui Shm.ui Fcntl.ui SigHandler.ui \
                SigAction.ui testcmd_loader.sml

SigAction.uo SigAction.ui : MappedStruct.ui ValRepr.ui SigSet.uo SigSet.ui SigAction.sml
	 $(MOSMLC) -imptypes -toplevel ValRepr.ui MappedStruct.ui SigSet.ui SigAction.sml

SigHandler.uo SigHandler.ui : MappedStruct.ui ValRepr.ui SigSet.uo SigSet.ui SigAction.ui \
                              MappedNativeWordRegister.ui MappedNativeAddressRegister.ui \
                              SysErr.ui ITimer.ui SigHandler.sml
	 $(MOSMLC) -imptypes -toplevel MappedStruct.ui ValRepr.ui SigSet.ui SigAction.ui \
                                       MappedNativeWordRegister.ui MappedNativeAddressRegister.ui \
                                       ITimer.ui SysErr.ui SigHandler.sml

PSelect.uo PSelect.ui : MappedStruct.ui SigSet.ui FDSet.ui ITimer.ui SysErr.ui PSelect.sml
	 $(MOSMLC) -imptypes MappedStruct.ui FDSet.ui SigSet.ui ITimer.ui SysErr.ui PSelect.sml

DuplexChannel.uo DuplexChannel.ui : ObjRepr.ui SplitFifo.ui SysErr.ui DuplexChannel.sml
	 $(MOSMLC) -imptypes -toplevel ObjRepr.ui SplitFifo.ui SysErr.ui DuplexChannel.sml

ArrayPatch.uo : ArrayPatch.sml ArrayPatch_sig.ui GenericArray.ui GenericArraySlice.ui
	$(MOSMLC) -imptypes -toplevel GenericArray.ui GenericArraySlice.ui ArrayPatch_sig.ui \
                                      ArrayPatch.sml

Evil.ui : Evil.sig
	$(MOSMLC) -imptypes Evil.sig

Evil.uo : Evil.ui Evil.sml
	$(MOSMLC) -imptypes Evil.sml

Scanners.ui : Scanners.sig
	$(MOSMLC) -imptypes Scanners.sig

Scanners.uo : Scanners.ui Scanners.sml
	$(MOSMLC) -imptypes Scanners.sml

Printers.ui : Printers.sig
	$(MOSMLC) -imptypes Printers.sig

Printers.uo : Printers.ui Printers.sml
	$(MOSMLC) -imptypes Printers.sml

SparseArrays.uo : GenericArray.ui GenericArraySlice.ui SparseArrays.sml
	$(MOSMLC) -imptypes -toplevel GenericArray.ui GenericArraySlice.ui SparseArrays.sml

ConcRep.uo ConcRep.ui : ConcRep.sml GenericArray.ui GenericArraySlice.ui ArrayPatch_sig.ui ArrayPatch.ui
	$(MOSMLC) -imptypes -toplevel GenericArray.ui GenericArraySlice.ui ArrayPatch_sig.ui \
                                      ArrayPatch.ui ConcRep.sml

StrmRep.uo StrmRep.ui : StrmRep.sml ConcRep.uo GenericArray.ui GenericArraySlice.ui \
                        ArrayPatch_sig.ui ArrayPatch.ui
	$(MOSMLC) -imptypes -toplevel ConcRep.ui GenericArray.ui GenericArraySlice.ui \
                                      ArrayPatch_sig.ui ArrayPatch.ui StrmRep.sml

AbstRep.uo AbstRep.ui : AbstRep.sml StrmRep.uo ConcRep.uo GenericArray.ui GenericArraySlice.ui \
                        ArrayPatch_sig.ui ArrayPatch.ui
	$(MOSMLC) -imptypes -toplevel StrmRep.ui ConcRep.ui GenericArray.ui GenericArraySlice.ui \
                        ArrayPatch_sig.ui ArrayPatch.ui AbstRep.sml

IPv4Packet.uo IPv4Packet.ui : IPv4Packet.sml AbstRep.uo StrmRep.uo ConcRep.uo GenericArray.ui \
                              GenericArraySlice.ui ArrayPatch_sig.ui ArrayPatch.ui
	$(MOSMLC) -imptypes -toplevel StrmRep.ui ConcRep.ui GenericArray.ui GenericArraySlice.ui \
                             ArrayPatch_sig.ui ArrayPatch.ui AbstRep.ui IPv4Packet.sml

TCPConsts.uo : TCPConsts.sml
	$(MOSMLC) -imptypes -toplevel TCPConsts.sml

Ffi.ui : Ffi.sig
	$(MOSMLC) -imptypes Ffi.sig

Ffi.uo : Ffi.sml
	$(MOSMLC) -imptypes Ffi.sml

Jit.ui : Jit.sig Ffi.ui
	$(MOSMLC) -imptypes Jit.sig

Jit.uo : Jit.sml Jit.ui
	$(MOSMLC) -imptypes Jit.sml

PrologLexer.sml : PrologLexer.lex

PrologParser.sml PrologParser.sig: PrologParser.grm
	$(MOSMLYACC) PrologParser.grm

PrologParser.ui : PrologParser.sig
	$(MOSMLC) -liberal PrologParser.sig

PrologParser.uo : PrologParser.ui PrologParser.sml
	$(MOSMLC) PrologParser.sml

PrologLexer.uo : PrologParser.uo PrologLexer.sml 
	$(MOSMLC) PrologLexer.sml

Prolog.ui : Prolog.sig
	$(MOSMLC) Prolog.sig

AbsSyn.uo AbsSyn.ui : AbsSyn.sml
	$(MOSMLC) -toplevel AbsSyn.sml

PrologSyntax.uo : PrologLexer.uo PrologParser.uo CSyntax.uo GrammarSyntax.uo PrologSyntax.sml
	$(MOSMLC) PrologSyntax.sml

Prolog.uo : Prolog.ui PrologSyntax.uo Prolog.sml
	$(MOSMLC) Prolog.sml

CPPInfoLexer.sml : CPPInfoLexer.lex

CPPInfoParser.sml CPPInfoParser.sig: CPPInfoParser.grm
	$(MOSMLYACC) CPPInfoParser.grm

CPPInfoParser.ui : CPPInfoParser.sig
	$(MOSMLC) -liberal CPPInfoParser.sig

GrammarLexer.sml : GrammarLexer.lex

GrammarParser.sml GrammarParser.sig: GrammarParser.grm
	$(MOSMLYACC) GrammarParser.grm

GrammarParser.ui : GrammarParser.sig
	$(MOSMLC) -liberal GrammarParser.sig

CParser.ui : CParser.sig
	$(MOSMLC) -liberal CParser.sig

CPPLexer.sml : CPPLexer.lex

CParser.sml CParser.sig: CParser.grm
	$(MOSMLYACC) CParser.grm

RewriteLexer.sml : RewriteLexer.lex

RewriteParser.sml RewriteParser.sig: RewriteParser.grm
	$(MOSMLYACC) RewriteParser.grm

RewriteParser.ui : RewriteParser.sig
	$(MOSMLC) -liberal RewriteParser.sig

RewriteParser.uo : RewriteParser.sml RewriteParser.ui 
	$(MOSMLC) RewriteParser.sml

RewriteMain.uo : PrologLexer.uo PrologParser.uo RewriteMain.sml
	$(MOSMLC) RewriteMain.sml

Rewrite.uo : Rewrite.sml RewriteParser.uo GrammarSyntax.uo
	$(MOSMLC) Rewrite.sml

GrammarSyntax.uo : AbsSyn.ui GrammarSyntax.sml
	$(MOSMLC) AbsSyn.ui GrammarSyntax.sml

CSyntax.uo : CSyntax.sml
	$(MOSMLC) CSyntax.sml

Location2.uo : Location2.sml Location2.ui
	$(MOSMLC) Location2.sml

PrimEncSig.ui : Jit.uo PrimEncSig.sml
	$(MOSMLC) -toplevel PrimEncSig.sml

PrimEnc.uo : PrimEncSig.sml PrimEnc.sml
	$(MOSMLC) -toplevel PrimEncSig.sml PrimEnc.sml

SpeedTest.uo : SpeedTest.sml
	$(MOSMLC) SpeedTest.sml

Globals.uo : Globals.sml
	$(MOSMLC) -toplevel Globals.sml

WordPrim.uo : WordPrim.sml
	$(MOSMLC) -toplevel WordPrim.sml

WordPrim.ui : WordPrim.uo

Rot13.uo : Rot13.sml
	$(MOSMLC) -toplevel Rot13.sml

BitSet.uo : BitSet.sml
	$(MOSMLC) -toplevel BitSet.sml

WordN.uo : WordPrim.ui GenericWord.ui WordN.sml
	$(MOSMLC) -toplevel WordPrim.ui GenericWord.ui WordN.sml

WordFormat.uo WordFormat.ui : WordFormat.sml GenericWord.ui
	$(MOSMLC) -toplevel GenericWord.ui WordFormat.sml

CompTestWord.uo CompTestWord.ui : Globals.sml GenericWord.ui WordFormat.sml CompTestWord.sml
	$(MOSMLC) -toplevel Globals.sml WordFormat.sml GenericWord.ui  CompTestWord.sml

TestWord.uo : Globals.sml CompTestWord.sml WordFormat.sml GenericWord.ui TestWord.sml
	$(MOSMLC) -toplevel WordPrim.sml Globals.sml WordFormat.sml \
                            GenericWord.ui CompTestWord.sml TestWord.sml

WordEncSig.ui : Jit.uo PrimEncSig.ui WordEncSig.sml  
	$(MOSMLC) -toplevel  WordEncSig.sml

WordOpsSig.ui : Jit.ui WordOpsSig.sml
	$(MOSMLC) -toplevel  WordOpsSig.sml

WordOps.uo : Jit.uo WordOpsSig.sml WordOps.sml
	$(MOSMLC) -toplevel WordOpsSig.sml WordOps.sml

VectorWordEnc.uo : WordPrim.sml WordEncSig.sml PrimEncSig.sml VectorWordEnc.sml
	$(MOSMLC) -toplevel WordPrim.sml PrimEncSig.sml WordEncSig.sml VectorWordEnc.sml

VectorSliceWordEnc.uo : WordPrim.ui WordEncSig.ui PrimEncSig.ui VectorSliceWordEnc.sml
	$(MOSMLC) -toplevel WordPrim.sml PrimEncSig.sml WordEncSig.sml VectorSliceWordEnc.sml

ArraySliceWordEnc.uo : WordPrim.sml WordEncSig.sml PrimEncSig.sml ArraySliceWordEnc.sml
	$(MOSMLC) -toplevel WordPrim.sml PrimEncSig.sml WordEncSig.sml ArraySliceWordEnc.sml

AbstractMachineWord.uo : WordPrim.ui WordEncSig.ui PrimEncSig.ui WordOpsSig.ui \
                    WordOps.uo AbstractMachineWord.sml 
	$(MOSMLC) -toplevel WordEncSig.ui WordOpsSig.ui WordPrim.ui AbstractMachineWord.sml

MachineWord.uo : MachineWord.sml WordPrim.sml
	$(MOSMLC) -toplevel WordPrim.sml MachineWord.sml

test:
	mosml testffi.sml

clean:
	rm -f *.o
	rm -f *.so
	rm -f *.ui
	rm -f *.uo
	rm -f *Parser.sml
	rm -f *Parser.sig
	rm -f *Parser.output
	rm -f *Lexer.sml
	rm -f Makefile.bak

depend: GrammarParser.sml GrammarLexer.sml RewriteParser.sml \
           RewriteLexer.sml CPPLexer.sml CParser.sml CPPInfoParser.sml \
           CPPInfoLexer.sml
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
PrologLexer.uo: PrologParser.ui 
Ffi.uo: Ffi.ui 
ArraySliceWordEnc.uo: Jit.ui PrimEnc.uo 
CPPInfoParser.ui: CSyntax.uo 
testregex.uo: Scanners.ui Printers.ui 
PrologSyntax.uo: GrammarSyntax.uo PrologParser.ui PrologLexer.uo 
PrologParser.uo: PrologParser.ui GrammarSyntax.uo 
testmcword.uo: TestWord.uo Jit.ui Ffi.ui 
CPPInfoLexer.uo: CPPInfoParser.ui 
Evil.uo: Evil.ui 
testhugewords.uo: Ffi.ui WordFormat.uo VectorWordEnc.uo 
teststatstr.uo: SigSet.uo MappedStruct.uo ITimer.uo ValRepr.uo PSelect.uo \
    MemDebug.uo FDSet.uo SigAction.uo SigHandler.uo 
MemDebug.uo: ValRepr.uo Lightning32.ui 
RealRepr.uo: RealRepr.ui 
Rewrite.uo: Location2.ui RewriteLexer.uo RewriteParser.ui GrammarSyntax.uo 
CParser.uo: CParser.ui CSyntax.uo 
testmmap.uo: Shm.uo MappedWord8ArraySlice.ui MMap.uo Fcntl.ui ValRepr.uo 
Shm.uo: MappedStruct.uo Fcntl.ui SysErr.uo FDSet.uo Lightning32.ui 
StaticWord8ArraySlice.ui: StaticWord8Array.ui 
Jit.uo: Jit.ui Ffi.ui 
Hasht.uo: Hasht.ui 
testsha2.uo: StaticWord8Array.ui Lightning32.ui StaticBuffer.ui 
SigSet.uo: MappedStruct.uo MappedWord8Array.ui MappedWord8ArraySlice.ui \
    ValRepr.uo SysErr.uo Lightning32.ui 
MachineWord.uo: Jit.ui Ffi.ui 
AbstractMachineWord.uo: Jit.ui Ffi.ui 
testrewrite.uo: Jit.ui Ffi.ui GrammarSyntax.uo RewriteMain.uo CSyntax.uo \
    Hasht.ui Rewrite.uo 
TestWord.uo: CompTestWord.uo Globals.uo SpeedTest.ui 
GrammarLexer.uo: GrammarParser.ui 
SplitFifo.uo: ValRepr.uo MemDebug.uo MappedNativeRegister.ui 
VectorWordEnc.uo: Jit.ui PrimEnc.uo 
Prolog.uo: Prolog.ui PrologSyntax.uo 
testfdset.uo: FDSet.uo StaticStruct.uo 
CSyntax.uo: GrammarSyntax.uo Hasht.ui Rewrite.uo 
MappedWord8ArraySlice.ui: MappedWord8Array.ui 
Wpp.uo: Wpp.ui 
Printers.uo: Printers.ui 
testmappedarray.uo: MappedWord8Array.ui MappedWord8ArraySlice.ui ValRepr.uo \
    MemDebug.uo Lightning32.ui 
StaticBuffer.uo: StaticBuffer.ui StaticWord8Array.ui 
Word8ArrayPatch.uo: Word8ArrayPatch.ui 
Lightning64.uo: Lightning64.ui 
MappedNativeRegister.ui: MappedWord8ArraySlice.ui 
PrologParser.ui: GrammarSyntax.uo 
testffi.uo: Jit.ui Ffi.ui 
StaticWord8ArraySlice.uo: StaticWord8ArraySlice.ui StaticWord8Array.ui 
testobj.uo: MappedWord8Array.ui MappedWord8ArraySlice.ui PolyRedBlackMap.uo \
    MemDebug.uo ObjRepr.uo MappedNativeRegister.ui 
StaticWord8Array.uo: StaticWord8Array.ui 
MappedNativeRegister.uo: MappedNativeRegister.ui MappedWord8Array.ui \
    MappedWord8ArraySlice.ui ValRepr.uo 
Lightning_head.uo: Lightning_head.ui 
testprolog.uo: Prolog.ui 
AbsObjRepr.uo: ValRepr.uo 
PolyRedBlackMap.uo: AbsObjRepr.uo 
RewriteParser.ui: GrammarSyntax.uo 
testguile.uo: Jit.ui Ffi.ui 
SpeedTest.uo: SpeedTest.ui 
testcmd.uo: SigSet.uo MappedWord8Array.ui MappedWord8ArraySlice.ui MMap.uo \
    MappedFifo.uo ShmMap.uo Fcntl.ui ValRepr.uo SysErr.uo SigAction.uo \
    SigHandler.uo 
FDSet.uo: MappedStruct.uo 
MappedRegister.uo: MappedRegister.ui MappedWord8Array.ui \
    MappedWord8ArraySlice.ui ValRepr.uo 
testreg.uo: MappedRegister.ui MappedWord8Array.ui MappedWord8ArraySlice.ui 
Scanners.uo: Scanners.ui 
GrammarParser.ui: GrammarSyntax.uo 
MappedStruct.uo: MappedWord8Array.ui MappedWord8ArraySlice.ui WordRepr.uo \
    ValRepr.uo RealRepr.ui 
CParser.ui: CSyntax.uo 
MMap.uo: MappedWord8Array.ui ValRepr.uo SysErr.uo FDSet.uo StaticStruct.uo \
    Lightning32.ui 
testcmd_loader.uo: SigSet.uo MappedWord8Array.ui MappedWord8ArraySlice.ui \
    MMap.uo MappedFifo.uo ShmMap.uo Fcntl.ui ValRepr.uo PSelect.uo FDSet.uo \
    SigAction.uo SigHandler.uo 
Lightning_foot.uo: Lightning_foot.ui 
RewriteMain.uo: CParser.ui Location2.ui CPPInfoParser.ui CPPLexer.uo \
    CSyntax.uo GrammarLexer.uo GrammarParser.ui CPPInfoLexer.uo 
ConcRep.uo: StaticWord8Array.ui StaticWord8ArraySlice.ui 
MappedRegister.ui: MappedWord8ArraySlice.ui 
tdpeprint.uo: Scanners.ui 
testrbobj.uo: ValRepr.uo MemDebug.uo ObjRepr.uo 
ExampleStruct.uo: IPv4Consts.uo ConcRep.uo StaticWord8ArraySlice.ui \
    StaticBuffer.ui 
CPPInfoParser.uo: CPPInfoParser.ui CSyntax.uo 
PrimEncSig.uo: Jit.ui PrimEnc.uo 
StaticStruct.uo: WordRepr.uo StaticWord8Array.ui StaticWord8ArraySlice.ui \
    StaticBuffer.ui RealRepr.ui 
IPv4Packet.uo: ConcRep.uo 
RewriteParser.uo: RewriteParser.ui GrammarSyntax.uo 
CPPLexer.uo: CParser.ui CSyntax.uo Hasht.ui 
BinaryFileFormat.uo: Jit.ui 
MappedWord8ArraySlice.uo: MappedWord8ArraySlice.ui MappedWord8Array.ui 
teststrms.uo: ConcRep.uo Evil.ui AbstRep.uo StrmRep.uo Scanners.ui \
    StaticWord8ArraySlice.ui Printers.ui StaticBuffer.ui 
MappedWord8Array.uo: MappedWord8Array.ui ValRepr.uo Lightning32.ui 
testlgtng.uo: Lightning32.ui 
WordEncSig.uo: Jit.ui 
PSelect.uo: SigSet.uo MappedStruct.uo SysErr.uo FDSet.uo Lightning32.ui 
Lightning32.uo: Lightning32.ui 
SigHandler.uo: SigSet.uo MappedStruct.uo MappedWord8Array.ui \
    MappedWord8ArraySlice.ui ValRepr.uo SysErr.uo Lightning32.ui \
    SigAction.uo MappedNativeRegister.ui 
Jit.ui: Ffi.ui 
RewriteLexer.uo: RewriteParser.ui 
DuplexChannel.uo: ObjRepr.uo 
RedBlackMap.uo: ObjRepr.uo 
GrammarParser.uo: GrammarParser.ui GrammarSyntax.uo 
StrmRep.uo: ConcRep.uo 
Fcntl.uo: Fcntl.ui 
VectorSliceWordEnc.uo: Jit.ui PrimEnc.uo 
WordOps.uo: Jit.ui 
ObjRepr.uo: AbsObjRepr.uo ValRepr.uo 
PrimEnc.uo: Jit.ui Ffi.ui 
AbstRep.uo: ConcRep.uo AbstRep.uo StrmRep.uo 
Location2.uo: Location2.ui 
ShmMap.uo: Shm.uo MMap.uo ValRepr.uo 
MappedFifo.uo: MappedWord8Array.ui MappedWord8ArraySlice.ui ValRepr.uo \
    MappedNativeRegister.ui 
ITimer.uo: MappedStruct.uo MappedWord8Array.ui MappedWord8ArraySlice.ui \
    WordRepr.uo ValRepr.uo SysErr.uo Lightning32.ui 
StaticBuffer.ui: StaticWord8Array.ui 
testsigset.uo: SigSet.uo StaticStruct.uo StaticBuffer.ui 
SigAction.uo: SigSet.uo MappedStruct.uo ValRepr.uo SysErr.uo Lightning32.ui 
