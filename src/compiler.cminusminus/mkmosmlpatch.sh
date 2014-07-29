#! /bin/sh
rpath=../../../mosml/src/compiler

for i in *.{sig,mlp,sml} ; do
   if test -e $rpath/$i ; then
      diff -c $i $rpath/$i ;
   fi ;
done
