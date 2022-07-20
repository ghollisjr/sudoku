#!/bin/bash
scp -i ~/.ssh/id_rsa_raspberrypi \
    *.lisp \
    *.cgi \
    *.asd \
    *.txt \
    Makefile \
    ghollisjr@raspberrypi:/home/ghollisjr/myprogs/LISP/sudoku/
./install-sudoku.sh
