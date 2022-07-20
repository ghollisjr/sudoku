#!/bin/bash
scp -i ~/.ssh/id_rsa_homevid \
    *.lisp \
    *.asd \
    *.txt \
    Makefile \
    pi@homevid:/home/pi/myprogs/LISP/sudoku/
./install-sudoku.sh
