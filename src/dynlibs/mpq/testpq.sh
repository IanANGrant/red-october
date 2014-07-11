#! /bin/sh

PGPATH=/home/ian3/usr/bin
PGCTL=$PGPATH/pg_ctl
PSQL=$PGPATH/psql

MOSML=mosml

export PGDATA=/tmp/testmosmlpg
$PGCTL stop > /dev/null 2>&1
if test -x $PGDATA ; then rm -rf $PGDATA ; fi
$PGCTL initdb &&
$PGCTL -l $PGDATA/logfile start &&
sleep 2 &&
$PSQL template1 -c "CREATE DATABASE $USERNAME" &&
$MOSML testpsql.sml &&
$PGCTL stop

