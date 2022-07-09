#!/usr/bin/env -S sbcl --core /usr/local/lib/sbcl-cores/sudoku.core --script
(unless (member :script *features*)
  (ql:quickload '(:cgi :scripting :sudoku)))
(in-package :cgi)
(use-package :sudoku)

(defparameter *style*
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
.sudoku td:nth-child(3n-2) {
    border-left:solid;
}
.sudoku td:nth-child(3n) {
    border-right:solid;
}
.sudoku tr:nth-child(3n-2) td {
    border-top:solid;
}
.sudoku tr:nth-child(3n) td {
    border-bottom:solid;
}
"
  "Style settings for sudoku web page")

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
                      for r below 9
                      collecting
                      `(tr ()
                           ,@(loop
                               for c below 9
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
                      for r below 9
                      collecting
                      `(tr ()
                           ,@(loop
                               for c below 9
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

(defparameter *head*
  `(head ()
         (title () "Common Lisp Sudoku Solver")
         (style () ,*style*))
  "Common header")

(defun input-page ()
  (http-response-header)
  (let* ((puzzle
           (read-sudoku-from-string
            (make-string 81 :initial-element #\.)))
         (table (puzzle->html puzzle :form-p t)))
    (format t
            "~a~%"
            (make-html
             `(html ()
                    ,*head*
                    (body ()
                          (h1 (:style #"text-align: center")
                              "Common Lisp Sudoku Solver")
                          (form (:action #"sudoku.cgi"
                                 :method #"POST")
                                (table (:style #"margin-left:auto;margin-right:auto")
                                       (tr ()
                                           (td ()
                                               ,table))
                                       (tr ()
                                           (td ()
                                               (input (:type #"submit"
                                                       :name #"submit"
                                                       :value #"Solve"))))))))))))

(defparameter *debug* NIL)

(defun results-page (queries)
  (http-response-header)
  (let* ((string (make-string 81 :initial-element #\.))
         puzzle)
    (handler-case
        (dotimes (r 9)
          (dotimes (c 9)
            (let* ((index (+ (* r 9) c))
                   (query (gethash (format nil "~a~a" r c)
                                   queries))
                   (value
                     (if (and query
                              (plusp (length query)))
                         (parse-integer query
                                        :start 0
                                        :end 1
                                        :radix 10 ; paranoia
                                        :junk-allowed t)
                         NIL)))
              ;; there are no single-digit integers greater than 9 in base 10.
              (when (and value
                         (plusp value))
                (setf (aref string index)
                      (character (format nil "~a" value)))))))
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
                        ,*head*
                        (body ()
                              (h1 (:style #"text-align: center") "Solution")
                              (table (:style #"margin-left:auto;margin-right:auto")
                                     (tr ()
                                         (td ()
                                             ,(puzzle->html puzzle)))
                                     (tr ()
                                         (td ()
                                             (a (:href #"sudoku.cgi")
                                                "Return to puzzle entry"))))))))
        (format t
                "~a~%"
                (make-html
                 `(html ()
                        ,*head*
                        (body ()
                              (p ()
                                 "Error solving puzzle")
                              (a (:href #"sudoku.cgi")
                                 "Return to puzzle entry"))))))))

(defun main ()
  (let* ((queries
           (parse-post-query)))
    (if queries
        (results-page queries)
        (input-page))))

(when (member :script *features*)
  (main))
