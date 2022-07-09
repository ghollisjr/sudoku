I found another project that attempted to port Peter Norvig's Sudoku
solver to Common Lisp (https://github.com/dimitri/sudoku), and while
the skeleton of the code was good, the solver had a bug wherein this
puzzle would lead to undetected contradictions while claiming it
solved the puzzle:

  4 . . | 9 . . | 3 . .
  . . 2 | 1 . . | . . 4
  5 3 . | . . . | . . .
  ------+-------+------
  . . . | . . . | . . .
  . . 4 | . . 9 | . 6 .
  . . 7 | 8 . . | . . 2
  ------+-------+------
  . 7 5 | . . 6 | 2 . .
  . . 9 | . . 7 | . . 8
  . . . | . . 5 | . . 3

This is my own from-scratch implementation of a Sudoku solver that was
inspired by Norvig and Dimitri, but with an added CGI web app script
to create a simple Sudoku solver web page provided you have your
environment set up with the CGI core already built and installed.

The usage is fairly straightforward:

(in-package :sudoku)
(write-sudoku
  (solve-from-string
    "
  4 . . | 9 . . | 3 . .
  . . 2 | 1 . . | . . 4
  5 3 . | . . . | . . .
  ------+-------+------
  . . . | . . . | . . .
  . . 4 | . . 9 | . 6 .
  . . 7 | 8 . . | . . 2
  ------+-------+------
  . 7 5 | . . 6 | 2 . .
  . . 9 | . . 7 | . . 8
  . . . | . . 5 | . . 3"))

would solve the above example and print it to *standard-output*.
