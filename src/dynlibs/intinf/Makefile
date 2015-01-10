# Makefile for dynamic library interface to GNU multiprecision package GMP

# The runtime system must be compiled with support for dynamic libraries.
# The GNU GMP multiprecision package must be installed.

# Where to find GMP header file and compiled library

#GMPDIR=${HOME}/c/gmp-2.0.2
GMPDIR=/home/ian3/usr
GMPINCDIR=$(GMPDIR)/include
GMPLIBDIR=$(GMPDIR)/lib

include ../../Makefile.inc

OPTS=-fno-defer-pop
ROOTDIR:=../..
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I$(CAMLRT) -I${GMPINCDIR}
#CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I$(INCDIR) -I ${GMPDIR}

MOSMLTOOLS=camlrunm $(TOOLDIR)

all: libmgmp.so IntInf.uo

install:
	$(INSTALL_DATA) libmgmp.so $(DESTDIR)$(LIBDIR)
	$(INSTALL_DATA) IntInf.sig $(DESTDIR)$(LIBDIR)
	$(INSTALL_DATA) IntInf.ui $(DESTDIR)$(LIBDIR)
	$(INSTALL_DATA) IntInf.uo $(DESTDIR)$(LIBDIR)

intinf.o: intinf.c
	$(CC) $(CFLAGS) -c -o intinf.o intinf.c

sha256.o : sha256.c
	$(CC) -o sha256.o -I ~/build/linux-3.16.3/include -I./ -c sha256.c 

libmgmp.so: intinf.o sha256.o
	$(DYNLD) -o libmgmp.so intinf.o sha256.o -L$(GMPLIBDIR) -lgmp -lc

test:
	echo $(CURDIR)
	mosml testintinf.sml

clean:
	rm -f *.o
	rm -f *.so
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak

depend: 
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
IntInf.uo: IntInf.ui 
