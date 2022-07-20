#!/bin/bash
ssh -i ~/.ssh/id_rsa_homevid pi@homevid <<EOF
cd /home/pi/myprogs/LISP/sudoku/ &&
    make clean &&
    make &&
    sudo make install &&
    sudo cp -v *.cgi /usr/lib/cgi-bin/
EOF
