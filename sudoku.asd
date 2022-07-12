;;;; sudoku.asd

(asdf:defsystem #:sudoku
  :serial t
  :depends-on (#:cl-generator #:cl-ana.array-utils)
  :components ((:file "sudoku")))
