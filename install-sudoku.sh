#!/bin/bash
ssh -i ~/.ssh/id_rsa_raspberrypi ghollisjr@raspberrypi <<EOF
cd /home/ghollisjr/myprogs/LISP/sudoku/ &&
    make clean &&
    make &&
    sudo make install &&
    sudo cp -v *.cgi /usr/lib/cgi-bin/
EOF
