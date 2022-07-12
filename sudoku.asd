;;;; sudoku.asd

(asdf:defsystem #:sudoku
  :serial t
  :depends-on (#:cl-generator)
  :components ((:file "sudoku")))
