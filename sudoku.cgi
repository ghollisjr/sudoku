#!/usr/bin/env -S sbcl --core /usr/local/lib/sbcl-cores/sudoku.core --script
(unless (member :script *features*)
  (ql:quickload '(:cgi :scripting :sudoku)))
(in-package :cgi)
(use-package :sudoku)
(import 'sudoku::*n-values*)

(defun style ()
  "Returns style for sudoku with variation due to *n-values*"
  (let* ((rt (floor (sqrt *n-values*))))
    (concatenate
     'string
     "
table.sudoku {
    margin:1em auto;
    border:1px solid;
}
.sudoku td {
    height:30px;
    width:30px;
    border:1px solid;
    text-align:center;
}
.sudoku input[type=\"text\"] {
    padding: 0px;
    margin: 0px;
    border: none;
    text-align:center;
    height:30px;
    width:30px;
}
.sudoku td:nth-child("
     (princ-to-string rt)
     "n-"
     (princ-to-string (1- rt))
     ") {
    border-left:solid;
}
.sudoku td:nth-child("
     (princ-to-string rt)
     "n) {
    border-right:solid;
}
.sudoku tr:nth-child("
     (princ-to-string rt)
     "n-"
     (princ-to-string (1- rt))
     ") td {
    border-top:solid;
}
.sudoku tr:nth-child("
     (princ-to-string rt)
     "n) td {
    border-bottom:solid;
}
")))

(defun puzzle->html (puzzle
                     &key
                       form-p)
  "Returns HTML element for puzzle as a table"
  (let* ((grid
           (slot-value puzzle
                       'sudoku::values)))
    (flet ((form-name (row col)
             (format nil "~a~a"
                     row
                     col)))
      (if form-p
          `(table (:class "sudoku")
                  ,@(loop
                      for r below *n-values*
                      collecting
                      `(tr ()
                           ,@(loop
                               for c below *n-values*
                               collecting
                               `(td ()
                                    (input (:type #"text"
                                            :name ,(quote-string (form-name r c))
                                            :value
                                            ,(if (zerop (aref grid r c))
                                                 #""
                                                 (aref grid r c)))))))))
          `(table (:class "sudoku")
                  ,@(loop
                      for r below *n-values*
                      collecting
                      `(tr ()
                           ,@(loop
                               for c below *n-values*
                               collecting
                               `(td ()
                                    ,(if (zerop (aref grid r c))
                                         ""
                                         (aref grid r c)))))))))))

(defun test ()
  (make-html
   (puzzle->html
    (read-sudoku-from-string "85...24..72......9..4.........1.7..23.5...9...4...........8..7..17..........36.4.")
    :form-p t)))

(defun head ()
  "Common header"
  `(head ()
         (title () "Common Lisp Sudoku Solver")
         (style () ,(style))))

(defun input-page ()
  (http-response-header)
  (let* ((puzzle
           (read-sudoku-from-string
            (make-string (* *n-values* *n-values*) :initial-element #\.)))
         (table (puzzle->html puzzle :form-p t)))
    (format
     t
     "~a~%"
     (make-html
      `(html ()
             ,(head)
             (body ()
                   (h1 (:style #"text-align: center")
                       "Common Lisp Sudoku Solver")
                   (form (:method #"POST")
                         (input (:type #"hidden" :name #"n"
                                 :value ,(quote-string
                                          (princ-to-string *n-values*))))
                         (table (:style #"margin-left:auto;margin-right:auto")
                                (tr ()
                                    (td ()
                                        ,table))
                                (tr ()
                                    (td ()
                                        (input (:type #"submit"
                                                :name #"submit"
                                                :value #"Solve"))))))
                   (table (:style #"margin-left:auto;margin-right:auto")
                          (tr ()
                              (td ()
                                  "Puzzle size:")
                              ,@(let ((result nil))
                                  (dolist (n (list 4 9 16)
                                             (nreverse result))
                                    (let ((link
                                            (quote-string
                                             (format nil
                                                     "sudoku.cgi?n=~a" n))))
                                      (push `(td ()
                                                 (a (:href ,link)
                                                    ,(format nil
                                                             "~ax~a"
                                                             n n)))
                                            result))))))))))))

(defparameter *debug* NIL)

(defun results-page (queries)
  (http-response-header)
  (let* ((string (make-string (* *n-values* *n-values*) :initial-element #\.))
         puzzle)
    (handler-case
        (let ((*print-base* (1+ *n-values*)))
          (dotimes (r *n-values*)
            (dotimes (c *n-values*)
              (let* ((index (+ (* r *n-values*) c))
                     (query (gethash (format nil "~a~a" r c)
                                     queries))
                     (value
                       (if (and query
                                (plusp (length query)))
                           (parse-integer query
                                          :start 0
                                          :end 1
                                          :radix *print-base*
                                          :junk-allowed t)
                           NIL)))
                (when (and value
                           (plusp value))
                  (setf (aref string index)
                        (character (format nil "~a" value))))))))
      (error (err) (format t "~a~%" err)))
    ;; debug
    (when *debug*
      (format t "~a~%" (str:replace-all (string #\newline)
                                        "<br>"
                                        (write-sudoku-to-string
                                         (read-sudoku-from-string string)))))
    ;; end debug
    (setf puzzle
          (solve
           (read-sudoku-from-string string)))
    ;; debug
    (when *debug*
      (format t "<br><br>~a~%" (str:replace-all (string #\newline)
                                                "<br>"
                                                (write-sudoku-to-string puzzle))))
    ;; end debug
    (if puzzle
        (format t
                "~a~%"
                (make-html
                 `(html ()
                        ,(head)
                        (body ()
                              (h1 (:style #"text-align: center") "Solution")
                              (table (:style #"margin-left:auto;margin-right:auto")
                                     (tr ()
                                         (td ()
                                             ,(puzzle->html puzzle)))
                                     (tr ()
                                         (td ()
                                             (a (:href ,(quote-string
                                                         (format
                                                          nil
                                                          "sudoku.cgi?n=~a"
                                                          *n-values*)))
                                                "Return to puzzle entry"))))))))
        (format t
                "~a~%"
                (make-html
                 `(html ()
                        ,(head)
                        (body ()
                              (p ()
                                 "Error solving puzzle")
                              (a (:href ,(quote-string
                                          (format
                                           nil
                                           "sudoku.cgi?n=~a"
                                           *n-values*)))
                                 "Return to puzzle entry"))))))))

(defun get-n-values (queries)
  "Get n-values from GET request and set *n-values* appropriately"
  ;; handle dimensionality
  (let* ((get queries)
         (dim nil))
    (when (and get (gethash "n" get))
      (setf dim (read-from-string (safe-string (gethash "n" get))
                                  nil nil))
      (unless (find dim (list 4 9 16))
        (setf dim nil))
      (when dim
        (setf *n-values* dim)))))

(defun main ()
  (let* ((post (parse-post-query)))
    (get-n-values (or post (parse-get-query)))
    (if post
        (results-page post)
        (input-page))))

(when (member :script *features*)
  (main))
