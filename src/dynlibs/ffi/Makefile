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

MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

all: libmffi.so Ffi.uo Jit.uo CParser.uo	\
    CPPLexer.uo GrammarParser.uo GrammarLexer.uo GrammarSyntax.uo	\
    CSyntax.uo RewriteLexer.uo RewriteParser.uo	\
    RewriteMain.uo Rewrite.uo CPPInfoParser.uo CPPInfoLexer.uo		\
    Location2.uo Hasht.uo 

install:
	cp libmffi.so $(LIBDIR)
	cp Ffi.ui Ffi.uo Jit.uo Jit.ui $(LIBDIR)

ffi.o: ffi.c mffi.h mem.h types.h
	$(CC) $(CFLAGS) -c -o ffi.o ffi.c

jit: jit.c mffi.h mem.h types.h
	$(CC) $(CFLAGS)  -DTEST_PROGRAM -llightning -o jit jit.c

jit.o: jit.c mffi.h mem.h types.h
	$(CC) $(CFLAGS) -c -o jit.o jit.c

svec.o: svec.c mffi.h mem.h
	$(CC) $(CFLAGS) -c -o svec.o svec.c

mem.o: mem.c mffi.h
	$(CC) $(CFLAGS) -c -o mem.o mem.c

types.o: types.c mffi.h mem.h types.h 
	$(CC) $(CFLAGS) -c -o types.o types.c

libmffi.so: svec.o jit.o mem.o ffi.o types.o
	$(DYNLD) -o libmffi.so -rpath=$(FFILIBDIR):$(LIGHTNINGLIBDIR) -lffi -llightning mem.o jit.o svec.o ffi.o types.o

Ffi.ui : Ffi.sig
	$(MOSMLC) -imptypes Ffi.sig

Ffi.uo : Ffi.sml
	$(MOSMLC) -imptypes Ffi.sml

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

RewriteMain.uo : RewriteMain.sml
	$(MOSMLC) RewriteMain.sml

Rewrite.uo : Rewrite.sml RewriteParser.uo GrammarSyntax.uo
	$(MOSMLC) Rewrite.sml

GrammarSyntax.uo : GrammarSyntax.sml
	$(MOSMLC) GrammarSyntax.sml

CSyntax.uo : CSyntax.sml
	$(MOSMLC) CSyntax.sml

Location2.uo : Location2.sml Location2.ui
	$(MOSMLC) Location2.sml

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
Ffi.uo: Ffi.ui 
CPPInfoParser.ui: CSyntax.uo 
CPPInfoLexer.uo: CPPInfoParser.ui 
Rewrite.uo: Location2.ui RewriteLexer.uo RewriteParser.ui GrammarSyntax.uo 
CParser.uo: CParser.ui CSyntax.uo 
Jit.uo: Jit.ui Ffi.ui 
Hasht.uo: Hasht.ui 
testrewrite.uo: GrammarSyntax.uo RewriteMain.uo CSyntax.uo Hasht.ui \
    Rewrite.uo 
GrammarLexer.uo: GrammarParser.ui 
CSyntax.uo: GrammarSyntax.uo Hasht.ui Rewrite.uo 
testffi.uo: Jit.ui Ffi.ui 
RewriteParser.ui: GrammarSyntax.uo 
GrammarParser.ui: GrammarSyntax.uo 
CParser.ui: CSyntax.uo 
RewriteMain.uo: CParser.ui Location2.ui CPPInfoParser.ui CPPLexer.uo \
    CSyntax.uo GrammarLexer.uo GrammarParser.ui CPPInfoLexer.uo 
CPPInfoParser.uo: CPPInfoParser.ui CSyntax.uo 
RewriteParser.uo: RewriteParser.ui GrammarSyntax.uo 
CPPLexer.uo: CParser.ui CSyntax.uo Hasht.ui 
RewriteLexer.uo: RewriteParser.ui 
GrammarParser.uo: GrammarParser.ui GrammarSyntax.uo 
Location2.uo: Location2.ui 
