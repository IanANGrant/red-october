# Makefile for Moscow ML dynamic libraries: the crypt example

include ../../Makefile.inc

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I$(INCDIR)
BASELIBS=-lm -ldl

all: libmcrypt.so 
	@echo "Now execute 'mosml testcrypt.sml' to test the C function"

crypt.o: crypt.c
	$(CC) $(CFLAGS) -c -o crypt.o crypt.c

libmcrypt.so: crypt.o
	$(DYNLD) -lcrypt -o libmcrypt.so crypt.o
#	$(DYNLD)         -o libmcrypt.so crypt.o

install:
	cp libmcrypt.so $(LIBDIR)

test:
	mosml testcrypt.sml

clean:
	rm -f *.o
	rm -f *.so


### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
