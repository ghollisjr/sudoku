;;;; sudoku.asd

(asdf:defsystem #:sudoku
  :serial t
  :depends-on (#:cl-ana.array-utils)
  :components ((:file "sudoku")))

