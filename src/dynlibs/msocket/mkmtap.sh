#! /bin/sh
sudo -v
test -f ./mtap && sudo chown $USER ./mtap
cc -o mtap -DLINUX -lrt mtap.c &&
sudo chown root ./mtap &&
sudo chmod +s ./mtap
