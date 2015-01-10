# Unix Makefile for Moscow ML library

# Library is compiled with -imptypes, even when used with value polymorphism
COMPFLAGS=-imptypes

include ../Makefile.inc

# Make with the supplied compiler

all: Array.uo Array2.uo ArraySlice.uo Arraysort.uo \
     BasicIO.uo Binaryset.uo Binarymap.uo BinIO.uo Bool.uo Buffer.uo Byte.uo \
     Callback.uo Char.uo CharArray.uo CharArraySlice.uo \
     CharVector.uo CharVectorSlice.uo CommandLine.uo \
     Date.uo Dynarray.uo Dynlib.uo \
     FileSys.uo Gdbm.uo Gdimage.uo Hashset.uo Help.uo \
     Int.uo Intmap.uo Intset.uo IO.uo \
     Lexing.uo List.uo ListPair.uo Listsort.uo Location.uo \
     Math.uo Misc.uo Mosml.uo Mosmlcgi.uo Mosmlcookie.uo Msp.uo Mysql.uo \
     NJ93.uo Nonstdio.uo \
     Obj.uo Option.uo OS.uo \
     Parsing.uo Path.uo Polygdbm.uo Polyhash.uo Postgres.uo PP.uo Process.uo \
     Random.uo Rbset.uo Real.uo Redblackmap.uo Regex.uo \
     Signal.uo SML90.uo Socket.uo Splaymap.uo Splayset.uo Splaytree.uo \
     Strbase.uo String.uo StringCvt.uo Substring.uo Susp.uo \
     TextIO.uo Buffered.uo Time.uo Timer.uo \
     Unix.uo Vector.uo VectorSlice.uo \
     Weak.uo Word.uo Word8.uo Word8Array.uo Word8ArraySlice.uo \
     Word8Vector.uo Word8VectorSlice.uo UTF8.uo

# Make with the current compiler
current:
	make clean
	make all MOSMLC=../camlrunm\ ../compiler/mosmlcmp\ -stdlib\ .\ -P\ none

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak
	rm -f Array.sml FileSys.sml Help.sml Int.sml Mosml.sml
	rm -f Path.sml Process.sml Strbase.sml 
	rm -f Vector.sml Word.sml Word8Array.sml Word8Vector.sml Weak.sml

install:
	test -d $(DESTDIR)$(LIBDIR) || mkdir -p $(DESTDIR)$(LIBDIR)
	${INSTALL_DATA} README $(DESTDIR)$(LIBDIR)
	${INSTALL_DATA} *.ui $(DESTDIR)$(LIBDIR)
	${INSTALL_DATA} *.uo $(DESTDIR)$(LIBDIR)
	${INSTALL_DATA} *.sig $(DESTDIR)$(LIBDIR)
	${INSTALL_DATA} General.fke $(DESTDIR)$(LIBDIR)/General.sig
	${INSTALL_DATA} Meta.fke $(DESTDIR)$(LIBDIR)/Meta.sig
#	rm -f $(DESTDIR)$(LIBDIR)/camlrunm
#	ln -s $(BINDIR)/camlrunm $(LIBDIR)/camlrunm

depend: Array.sml FileSys.sml Help.sml Int.sml Mosml.sml Path.sml \
    Process.sml Strbase.sml Vector.sml Word.sml Word8Array.sml \
    Word8Vector.sml Weak.sml
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLCUT) < Makefile.bak > Makefile
	$(MOSMLDEP) >> Makefile

# mosmldep does not see that OS depends on FileSys, Path, Process:

OS.ui: FileSys.ui Path.ui Process.ui

### DO NOT DELETE THIS LINE
Word8.ui: Word.ui StringCvt.ui 
Callback.uo: Callback.ui Polyhash.ui 
Math.uo: Math.ui 
Array.ui: Vector.ui 
Vector.uo: Vector.ui List.ui 
OS.uo: OS.ui 
VectorSlice.ui: Vector.ui 
Word8ArraySlice.ui: Word8Array.ui Word8.ui Word8Vector.ui \
    Word8VectorSlice.ui 
Polyhash.uo: Polyhash.ui Array.ui 
Socket.ui: Signal.ui Word8Array.ui Word8Vector.ui Time.ui 
Location.ui: BasicIO.ui Lexing.ui 
Bool.ui: StringCvt.ui 
VectorSlice.uo: VectorSlice.ui Vector.ui 
Array2.ui: Vector.ui 
Lexing.uo: Lexing.ui CharArray.ui Obj.uo 
Path.uo: Path.ui String.ui CharVector.ui List.ui Substring.ui 
Date.ui: StringCvt.ui Time.ui 
Splayset.uo: Splayset.ui List.ui Splaytree.ui 
Signal.ui: Word.ui 
UTF8.ui: Word.ui String.ui Char.ui 
Dynlib.uo: Dynlib.ui List.ui 
Mosmlcgi.uo: Mosmlcgi.ui String.ui StringCvt.ui List.ui Option.ui \
    Process.ui Substring.ui Splaymap.ui TextIO.ui Int.ui Char.ui 
Regex.uo: Regex.ui Word.ui String.ui Dynlib.ui List.ui Vector.ui \
    Substring.ui 
CharArraySlice.uo: CharArraySlice.ui CharVector.ui CharArray.ui \
    Word8ArraySlice.ui CharVectorSlice.ui 
Misc.uo: Misc.ui String.ui List.ui Option.ui Vector.ui Array.ui TextIO.ui \
    Char.ui 
Timer.ui: Time.ui 
ArraySlice.uo: ArraySlice.ui Vector.ui Array.ui VectorSlice.ui 
Rbset.uo: Rbset.ui List.ui Int.ui 
Help.uo: Help.ui String.ui StringCvt.ui List.ui BasicIO.ui Vector.ui \
    TextIO.ui Char.ui 
Process.uo: Process.ui List.ui BasicIO.ui Time.ui 
Binarymap.uo: Binarymap.ui 
String.uo: String.ui List.ui Strbase.ui Char.ui 
Nonstdio.ui: BasicIO.ui CharArray.ui Char.ui 
CommandLine.uo: CommandLine.ui Vector.ui 
Word8Array.ui: Word8.ui Word8Vector.ui 
Signal.uo: Signal.ui Word.ui 
Bool.uo: Bool.ui StringCvt.ui Char.ui 
FileSys.ui: Time.ui 
Nonstdio.uo: Nonstdio.ui BasicIO.ui CharArray.ui 
List.uo: List.ui 
Mosmlcookie.uo: Mosmlcookie.ui String.ui List.ui Date.ui Option.ui \
    Process.ui Substring.ui Bool.ui 
Arraysort.uo: Arraysort.ui Array.ui 
Word8.uo: Word8.ui Word.ui String.ui StringCvt.ui Char.ui 
Unix.uo: Unix.ui BinIO.ui Signal.ui Word.ui Dynlib.ui Option.ui Vector.ui \
    Socket.ui OS.ui TextIO.ui Obj.uo 
Misc.ui: Array.ui 
Word8VectorSlice.ui: Word8.ui Word8Vector.ui 
String.ui: Char.ui 
Word8Vector.ui: Word8.ui 
FileSys.uo: FileSys.ui Path.ui BinIO.ui List.ui Time.ui 
CharArraySlice.ui: CharVector.ui CharArray.ui CharVectorSlice.ui 
SML90.uo: SML90.ui String.ui BasicIO.ui 
CharVector.ui: Char.ui 
PP.uo: PP.ui String.ui UTF8.ui List.ui Vector.ui Array.ui TextIO.ui 
Parsing.ui: Lexing.ui Vector.ui Obj.uo 
Intmap.uo: Intmap.ui 
BasicIO.uo: BasicIO.ui 
Option.uo: Option.ui 
Int.ui: StringCvt.ui 
Time.ui: StringCvt.ui 
Gdbm.uo: Gdbm.ui Dynlib.ui List.ui 
Int.uo: Int.ui String.ui StringCvt.ui Char.ui 
Mosml.ui: Word8Vector.ui 
CharArray.uo: CharArray.ui CharVector.ui Word8Array.ui Char.ui 
BinIO.uo: BinIO.ui String.ui Word8.ui TextIO.ui Word8Vector.ui Char.ui 
Polygdbm.ui: Gdbm.ui 
Splaymap.uo: Splaymap.ui Splaytree.ui 
Word8Array.uo: Word8Array.ui List.ui Word8.ui Word8Vector.ui 
TextIO.ui: StringCvt.ui Char.ui 
Mosml.uo: Mosml.ui Timer.ui FileSys.ui Path.ui BinIO.ui String.ui List.ui \
    Vector.ui Word8.ui Process.ui Byte.ui TextIO.ui Word8Vector.ui Time.ui 
Postgres.uo: Postgres.ui String.ui Real.ui Dynlib.ui StringCvt.ui Msp.ui \
    List.ui Date.ui Option.ui Word8Array.ui Vector.ui Substring.ui Int.ui \
    Bool.ui 
Listsort.uo: Listsort.ui List.ui 
Gdimage.uo: Gdimage.ui Dynlib.ui Vector.ui 
Socket.uo: Socket.ui Signal.ui Word.ui Dynlib.ui Word8Array.ui Vector.ui \
    Word8Vector.ui Time.ui Word8VectorSlice.ui 
Splaytree.uo: Splaytree.ui 
Byte.ui: String.ui Word8Array.ui Word8.ui Word8ArraySlice.ui Substring.ui \
    Word8Vector.ui Char.ui Word8VectorSlice.ui 
Date.uo: Date.ui String.ui Real.ui StringCvt.ui Option.ui Vector.ui Int.ui \
    Time.ui Char.ui 
Location.uo: Location.ui CharVector.ui Parsing.ui BasicIO.ui Nonstdio.ui \
    Lexing.ui 
Timer.uo: Timer.ui Time.ui 
CharArray.ui: CharVector.ui Char.ui 
Array.uo: Array.ui List.ui Vector.ui 
Char.uo: Char.ui Strbase.ui 
Word.uo: Word.ui String.ui StringCvt.ui Char.ui 
ArraySlice.ui: Vector.ui Array.ui VectorSlice.ui 
Susp.uo: Susp.ui 
Mosmlcookie.ui: Date.ui 
Buffered.uo: Buffered.ui CharArray.ui TextIO.ui Obj.uo 
CharVector.uo: CharVector.ui String.ui Word8Vector.ui Char.ui 
Unix.ui: BinIO.ui Signal.ui Socket.ui OS.ui TextIO.ui 
Arraysort.ui: Array.ui 
CharVectorSlice.ui: CharVector.ui Substring.ui Char.ui 
Random.uo: Random.ui 
NJ93.uo: NJ93.ui String.ui List.ui BasicIO.ui TextIO.ui 
Real.ui: StringCvt.ui 
Word8Vector.uo: Word8Vector.ui String.ui List.ui Word8.ui 
Process.ui: Time.ui 
Word8ArraySlice.uo: Word8ArraySlice.ui Word8Array.ui Word8.ui \
    Word8Vector.ui Word8VectorSlice.ui 
Buffered.ui: CharArray.ui TextIO.ui 
Hashset.uo: Hashset.ui List.ui Array.ui 
Msp.uo: Msp.ui String.ui StringCvt.ui List.ui Option.ui Vector.ui TextIO.ui \
    Int.ui Mosmlcgi.ui Char.ui 
UTF8.uo: UTF8.ui String.ui Word.ui CharVector.ui StringCvt.ui Word8.ui \
    Int.ui Word8Vector.ui Char.ui 
StringCvt.uo: StringCvt.ui 
Polygdbm.uo: Polygdbm.ui List.ui Gdbm.ui 
Mysql.ui: Msp.ui Date.ui Word8Array.ui Vector.ui 
Weak.uo: Weak.ui Dynlib.ui 
ListPair.uo: ListPair.ui List.ui 
Substring.uo: Substring.ui String.ui Strbase.ui 
AppleScript.uo: AppleScript.ui 
Real.uo: Real.ui StringCvt.ui Char.ui 
Lexing.ui: CharArray.ui Obj.uo 
BinIO.ui: Word8.ui Word8Vector.ui 
Buffer.uo: Buffer.ui String.ui Substring.ui 
Strbase.uo: Strbase.ui List.ui 
Dynarray.uo: Dynarray.ui Array.ui 
Byte.uo: Byte.ui String.ui Word8.ui Word8ArraySlice.ui Substring.ui \
    Word8Vector.ui Char.ui Word8VectorSlice.ui 
Postgres.ui: Msp.ui Date.ui Word8Array.ui Vector.ui 
Parsing.uo: Parsing.ui Lexing.ui Vector.ui Obj.uo 
Mysql.uo: Mysql.ui String.ui Real.ui Dynlib.ui StringCvt.ui Msp.ui List.ui \
    Date.ui Option.ui Word8Array.ui Vector.ui Substring.ui Int.ui 
Word8VectorSlice.uo: Word8VectorSlice.ui Word8.ui Word8Vector.ui 
Binaryset.uo: Binaryset.ui List.ui 
Array2.uo: Array2.ui List.ui Vector.ui Array.ui VectorSlice.ui \
    ArraySlice.ui 
Intset.uo: Intset.ui List.ui 
Time.uo: Time.ui Real.ui StringCvt.ui Char.ui 
Redblackmap.uo: Redblackmap.ui 
TextIO.uo: TextIO.ui String.ui Char.ui 
CharVectorSlice.uo: CharVectorSlice.ui CharVector.ui Substring.ui Char.ui \
    Word8VectorSlice.ui 
Word.ui: StringCvt.ui 
