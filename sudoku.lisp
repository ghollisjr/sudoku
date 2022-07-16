(defpackage #:sudoku
  (:use :cl)
  (:use :cl-ana.array-utils)
  (:export
   :sudoku
   :solve
   :solve-from-string
   :check
   :read-sudoku
   :read-sudoku-from-string
   :write-sudoku
   :write-sudoku-to-string))

(in-package :sudoku)

(declaim (optimize (debug 1)
                   (speed 3)
                   (safety 1)
                   (space 3)
                   (compilation-speed 1)))

;;; Parameters

(defparameter *n-values* 9
  "Number of values for sudoku cells.")

(defparameter *check-p* T
  "Check initial puzzle for contradictions.  Only disable for
debugging.")

;;; Utility Macros
(defmacro awhen (test &body body)
  "Binds the resulting value of test to 'it and executes when statement"
  `(let ((it ,test))
     (when it
       ,@body)))

;; neighbor walking
(defmacro do-neighbors ((nrow ncol row col &optional result) &body body)
  "Iterates over all neighbor index values for the input values i & j,
excluding i & j"
  (let* ((bc (gensym "blockcol"))
         (br (gensym "blockrow"))
         (r (gensym "row"))
         (c (gensym "col"))
         (rn (gensym "nr")))
    `(let ((,rn (floor (sqrt *n-values*)))
           (,r ,row)
           (,c ,col))
       ;; column
       (let ((,ncol ,c))
         (dotimes (,nrow *n-values*)
           (unless (= ,nrow ,r)
             ,@body)))
       ;; row
       (let ((,nrow ,r))
         (dotimes (,ncol *n-values*)
           (unless (= ,ncol ,c)
             ,@body)))
       ;; block
       (let* ((,br (* ,rn (floor ,r ,rn)))
              (,bc (* ,rn (floor ,c ,rn))))
         (dotimes (,nrow ,rn)
           (dotimes (,ncol ,rn)
             (let ((,ncol (+ ,bc ,ncol))
                   (,nrow (+ ,br ,nrow)))
               (unless (or (= ,ncol ,c)
                           (= ,nrow ,r))
                 ,@body)))))
       ,result)))

;; unit walking
(defmacro do-units ((unit &optional result) &body body)
  "Binds unit to the list of values for each unit and executes body,
returning optional result"
  (let ((row (gensym "row"))
        (col (gensym "col"))
        (tmp (gensym "tmp"))
        (tmp2 (gensym "tmp2"))
        (rn (gensym "rn")))
    `(let ((,rn (floor (sqrt *n-values*))))
       ;; rows
       (dotimes (,row *n-values*)
         (let ((,tmp NIL))
           (dotimes (,col *n-values*)
             (push (list ,row ,col) ,tmp))
           (let ((,unit ,tmp))
             ,@body)))
       ;; cols
       (dotimes (,col *n-values*)
         (let ((,tmp NIL))
           (dotimes (,row *n-values*)
             (push (list ,col ,row) ,tmp))
           (let ((,unit ,tmp))
             ,@body)))
       ;; blocks
       (dotimes (,col ,rn)
         (dotimes (,row ,rn)
           (let ((,tmp NIL))
             (dotimes (,tmp2 *n-values*)
               (push (list (+ (* ,col ,rn) (floor ,tmp2 ,rn))
                           (+ (* ,row ,rn) (mod ,tmp2 ,rn)))
                     ,tmp))
             (let ((,unit ,tmp))
               ,@body))))
       ,result)))

;; intvec walking
(defmacro do-intvec ((var intvec &optional result) &body body)
  "Iterate over all values in the intvec"
  (let* ((v (gensym "intvec"))
         (index (gensym "index"))
         (val (gensym "val")))
    `(let* ((,v ,intvec)
            (,var NIL))
       (do ((,index 0 (1+ ,index)))
           ((zerop ,v) ,result)
         (symbol-macrolet ((,val (mod ,v 2)))
           (when (plusp ,val)
             (setf ,var (1+ ,index))
             ,@body))
         (setf ,v (floor ,v 2))))))

;;; Integer bitvector API
(declaim (inline INTVEC-INSERT INTVEC-REMOVE
                 INTVEC-MEMBER INTVEC-LENGTH
                 INTVEC-SINGLETON-VALUE INTVEC-EMPTY-P
                 INTVEC->LIST LIST->INTVEC MAKE-FULL-INTVEC))

(defun intvec-insert (intvec value)
  "Returns new intvec with value inserted"
  (logior intvec
          (ash 1 (1- value))))

(defun intvec-remove (intvec value)
  "Returns new intvec with value removed"
  (logxor intvec
          (logand intvec
                  (ash 1 (1- value)))))

(defun intvec-member (intvec value)
  "Returns boolean denoting whether or not value is in intvec"
  (logbitp (1- value) intvec))

(defun intvec-length (intvec)
  (logcount intvec))

(defun intvec-singleton-value (intvec)
  "Returns the only value in the intvec if there's only one, otherwise
returns NIL."
  (when (= 1 (logcount intvec))
    (integer-length intvec)))

(defun intvec-empty-p (intvec)
  "Returns T if intvec has no items, NIL otherwise"
  (zerop (logcount intvec)))

(defun intvec->list (intvec)
  (let* ((result nil))
    (do-intvec (i intvec result)
      (push i result))))

(defun list->intvec (list)
  (reduce #'intvec-insert
          list
          :initial-value 0))

(defun make-full-intvec ()
  "Returns completely filled intvec for current *n-values*"
  (1- (ash 1 *n-values*)))

;;; Utility functions
(defun copy-sudoku-array (array &optional (initial-element 0))
  (let* ((dims (array-dimensions array))
         (type (array-element-type array))
         (result (make-array dims
                             :element-type type
                             :initial-element initial-element)))
    (dotimes (i *n-values*)
      (dotimes (j *n-values*)
        (setf (aref result i j)
              (aref array i j))))
    result))

;; sudoku puzzle type
(defclass sudoku ()
  ((values
    :initarg :values
    :initform (make-array (list *n-values* *n-values*)
                          :initial-element 0
                          :element-type (list 'integer 0 *n-values*))
    :documentation "values on grid that have been set")
   (possible
    :initarg :possible
    :initform NIL
    :documentation "intvec grid for remaining possible values")))

;; Initial constraint propagation
;;
;; Only run this if possible isn't supplied
(when *check-p*
  (defmethod initialize-instance :after ((s sudoku) &key &allow-other-keys)
    (with-slots (values possible) s
      (unless possible
        (setf possible
              (make-array (list *n-values* *n-values*)
                          :initial-element (make-full-intvec)
                          :element-type (list 'integer 0 (make-full-intvec))))
        (dotimes (i *n-values*)
          (dotimes (j *n-values*)
            (let ((value (aref values i j)))
              (when (plusp value)
                (unless (intvec-member (aref possible i j)
                                       value)
                  (error "Contradiction"))
                (setf (aref possible i j)
                      (intvec-insert 0 value))
                (do-neighbors (nr nc i j)
                  (setf (aref possible nr nc)
                        (intvec-remove (aref possible nr nc)
                                       value))
                  (when (zerop (aref possible nr nc))
                    (error "Contradiction")))))))))))

(defun copy-sudoku (s)
  (with-slots (values possible) s
    (make-instance 'sudoku
                   :values (copy-sudoku-array values)
                   :possible (copy-sudoku-array possible (make-full-intvec)))))

;; debugging function
(defun copy-sudoku-unit (s i j)
  "Only copies data from specific unit containing (i j)"
  (let* ((result
           (make-instance 'sudoku
                          :values
                          (make-array (list *n-values* *n-values*)
                                      :initial-element 0)
                          :possible
                          (make-array (list *n-values* *n-values*)
                                      :initial-element (make-full-intvec)))))
    (with-slots (values) result
      (setf (aref values i j)
            (aref (slot-value s 'values) i j))
      (do-neighbors (ni nj i j)
        (setf (aref values ni nj)
              (aref (slot-value s 'values) ni nj))))
    result))

;; Sudoku puzzle I/O
(defun read-sudoku (&optional
                      (stream *standard-input*)
                      (eof-error-p t)
                      (eof-value nil)
                      (recursive-p nil))
  "Reads sudoku puzzle from stream and returns object.  Digits
characters and periods are allowed as data, all other values are
ignored.  Note that Common Lisp's radix-aware I/O is used, so only
*n-values* up to 36 are supported (i.e. 4, 9, 16, 25 are the possible
reasonable *n-values*)."
  (do (;; constants
       (radix (1+ *n-values*))
       ;; loop variables
       (values (make-array (list *n-values* *n-values*)
                           :initial-element 0
                           :element-type (list 'integer 0 *n-values*)))
       (index 0))
      ((= index (* *n-values* *n-values*))
       (make-instance 'sudoku :values values))
    (let ((c (read-char stream eof-error-p eof-value recursive-p))
          (i (floor index *n-values*))
          (j (mod index *n-values*)))
      (when (not c)
        (return-from read-sudoku NIL))
      (when (or (digit-char-p c radix)
                (char= c #\.))
        (incf index)
        (when (char= c #\.)
          (setf c #\0))
        (let ((val (parse-integer (string c)
                                  :radix radix)))
          (setf (aref values i j) val))))))

(defun read-sudoku-from-string (string)
  (with-input-from-string (s string)
    (read-sudoku s)))

(defun write-sudoku (sudoku &optional (stream *standard-output*))
  "Prints puzzle to stream in human readable way."
  (let* ((*print-base* (1+ *n-values*))
         (rn (floor (sqrt *n-values*)))
         (line-width (+ *n-values* ; entries
                        (- *n-values* 1) ; spaces
                        (- rn 1)))) ; block divisions
    (declare (ignore line-width))
    (labels ((division ()
               (dotimes (i *n-values*)
                 (princ (if (and (plusp i)
                                 (zerop (mod i rn)))
                            "+---"
                            (if (< i (1- *n-values*)) "--" "-"))
                        stream))
               (terpri stream)))
      (dotimes (i *n-values*)
        (when (and (plusp i)
                   (zerop (mod i rn)))
          (division))
        (dotimes (j *n-values*)
          (when (and (plusp j)
                     (zerop (mod j rn)))
            (princ "| " stream))
          (with-slots (values) sudoku
            (let ((v (aref values i j)))
              (princ (if (plusp v) v ".") stream)
              (princ " " stream))))
        (terpri stream)))))

(defun write-sudoku-to-string (sudoku)
  (with-output-to-string (s)
    (write-sudoku sudoku s)))

;; solver
(defun sudoku-solved-p (puzzle)
  "Returns NIL if the puzzle is unsolved, otherwise returns the puzzle"
  (with-slots (values) puzzle
    (dotimes (i *n-values*)
      (dotimes (j *n-values*)
        (unless (plusp (aref values i j))
          (return-from sudoku-solved-p NIL)))))
  puzzle)

(defun assign (puzzle i j value)
  (with-slots (values possible) puzzle
    (cond
      ;; redundant assignments
      ((= (aref values i j) value)
       puzzle)
      ;; contradictions
      ((plusp (aref values i j))
       NIL)
      ((not (intvec-member (aref possible i j) value))
       NIL)
      ;; recursion
      (t
       (let ((singletons NIL))
         (labels ((eliminate (i j value)
                    ;; Remove value from (i j)'s possible values.  If it
                    ;; results in a singleton, call assign on that value.
                    ;; If it results in an empty intvec, that's a
                    ;; contradiction so return NIL.  Otherwise return the
                    ;; puzzle after successful modification.
                    ;;
                    ;; Pushes singletons into the singletons list when
                    ;; encountered.
                    (symbol-macrolet ((pos (aref possible i j)))
                      (setf pos
                            (intvec-remove pos
                                           value))
                      (when (zerop pos)
                        (return-from eliminate NIL))
                      (awhen (intvec-singleton-value pos)
                        (push (list i j it) singletons))
                      T)))
           (setf (aref values i j) value
                 (aref possible i j) (intvec-insert 0 value))
           (do-neighbors (ni nj i j)
             (when (not (plusp (aref values ni nj)))
               (unless (eliminate ni nj value)
                 (return-from assign NIL))))
           (dolist (s singletons)
             (destructuring-bind (i j v) s
               (unless (assign puzzle i j v)
                 (return-from assign NIL))))))
       puzzle))))

(defun solve (puzzle)
  (with-slots (values possible) puzzle
    (if (sudoku-solved-p puzzle)
        puzzle
        (destructuring-bind (i j nvalues)
            (reduce (lambda (x y)
                      (if (< (third x) (third y)) x y))
                    (loop
                      for i below *n-values*
                      appending
                      (loop
                        for j below *n-values*
                        for nvalues = (intvec-length (aref possible i j))
                        when (and (not (plusp (aref values i j)))
                                  (not (= nvalues 1)))
                          collecting
                          (list i j nvalues))))
          (unless (zerop nvalues)
            (do-intvec (value (aref possible i j))
              (let* ((new (assign (copy-sudoku puzzle) i j value)))
                (awhen (and new
                            (solve new))
                  (return-from solve it)))))))))

(defun check (puzzle)
  "Checks puzzle to see if it's really a solution"
  (with-slots (values possible) puzzle
    (and
     ;; all values are set
     (let ((result T))
       (dotimes (i *n-values* result)
         (dotimes (j *n-values*)
           (when (not (plusp (aref values i j)))
             (setf result NIL)))))
     ;; no units have a contradiction
     (let ((result T))
       (do-units (unit result)
         (let ((contained 0))
           (dolist (u unit)
             (destructuring-bind (i j) u
               (setf contained
                     (intvec-insert contained
                                    (aref values i j)))))
           (when (not (= (intvec-length contained) *n-values*))
             (setf result NIL))))))))

(defun solve-from-string (string)
  (with-input-from-string (s string)
    (solve (read-sudoku s))))

(defparameter *hard-test*
  "4 0 0 | 9 0 0 | 3 0 0
  0 0 2 | 1 0 0 | 0 0 4
  5 3 0 | 0 0 0 | 0 0 0
  ------+-------+------
  0 0 0 | 0 0 0 | 0 0 0
  0 0 4 | 0 0 9 | 0 6 0
  0 0 7 | 8 0 0 | 0 0 2
  ------+-------+------
  0 7 5 | 0 0 6 | 2 0 0
  0 0 9 | 0 0 7 | 0 0 8
  0 0 0 | 0 0 5 | 0 0 3"
  "This was the problem that stumped the other guy's solver.  Looks like
mine solves it.")

;;;; Test suite
(defmacro timed-test (acc &body body)
  "Pushes run time value into acc"
  (let* ((start (gensym "start"))
         (result (gensym "result")))
    `(let ((,start (get-internal-real-time))
           (,result (progn ,@body)))
       (push (* 1e-6 (- (get-internal-real-time) ,start)) ,acc)
       ,result)))

(defun run-tests (name path)
  (let* ((path (asdf:system-relative-pathname
                :sudoku
                path))
         (n 0)
         (success 0)
         (times NIL))
    (with-open-file (f path :direction :input)
      (do ((puzzle T))
          ((null puzzle) nil)
        (setf puzzle
              (read-sudoku f nil nil))
        (when puzzle
          (incf n)
          (setf puzzle
                (timed-test times
                  (solve puzzle)))
          (when (check puzzle)
            (incf success)))))
    (format t "~a: Pass ~a/~a~%"
            name
            success n)
    (let ((min (first times))
          (max (first times))
          (mean 0.0))
      (dolist (time times)
        (when (< time min)
          (setf min time))
        (when (> time max)
          (setf max time))
        (incf mean time))
      (setf mean (/ mean n))
      (format t "Average time: ~a~%Max time: ~a~%Min time: ~a~%"
              mean max min))))

(defun run-tests-debug (name path)
  (let* ((path (asdf:system-relative-pathname
                :sudoku
                path))
         (n 0)
         (success 0)
         (times NIL))
    (with-open-file (f path :direction :input)
      (do ((puzzle T))
          ((null puzzle) nil)
        (setf puzzle
              (read-sudoku f nil nil))
        (when puzzle
          (incf n)
          (setf puzzle
                (timed-test times
                  (solve puzzle)))
          (when (check puzzle)
            (incf success)))))
    (format t "~a: Pass ~a/~a~%"
            name
            success n)
    (let ((min (first times))
          (max (first times))
          (mean 0.0))
      (dolist (time times)
        (when (< time min)
          (setf min time))
        (when (> time max)
          (setf max time))
        (incf mean time))
      (setf mean (/ mean n))
      (format t "Average time: ~a~%Max time: ~a~%Min time: ~a~%"
              mean max min)
      times)))

(defun easy-tests ()
  (run-tests "Easy tests" "data/easy50.txt"))

(defun hard-tests ()
  (run-tests "Hard tests" "data/hardest.txt"))

(defun top95-tests ()
  (run-tests "Top 95 tests" "data/top95.txt"))

;;;; 25x25 tests
(defparameter *25x25-raw-data*
  (list 0 2 0 0 0 3 14 0 8 0 0 0 0 0 0 0 0 13 4 24 0 7 1 0 0
        0 10 17 0 0 0 6 18 0 0 22 16 0 12 0 0 0 0 1 0 0 0 13 19 0
        0 15 24 13 7 0 0 0 4 0 10 0 0 3 14 0 18 0 0 0 0 22 2 6 0
        0 0 1 21 0 0 15 0 22 0 0 19 13 0 0 0 8 0 0 0 0 16 18 20 0
        0 5 0 0 20 7 25 19 0 0 0 21 17 18 2 10 12 22 9 15 11 0 0 0 0
        11 0 0 0 22 8 0 24 7 1 5 0 0 0 13 16 17 25 23 2 4 0 6 0 19
        16 9 12 0 17 0 19 22 0 0 0 0 18 21 0 0 20 6 13 0 7 0 0 23 11
        0 0 6 0 21 9 16 0 3 0 0 22 20 19 0 0 0 0 15 8 25 0 0 0 0
        0 0 23 5 0 2 0 0 11 17 8 0 0 0 16 12 9 0 0 21 0 3 10 0 0
        0 0 0 0 0 6 0 0 12 0 9 1 25 0 3 0 11 0 0 7 0 0 21 0 0
        0 0 9 0 0 23 0 5 17 4 16 0 11 0 22 18 2 0 21 13 0 0 7 0 0
        4 6 0 0 5 0 0 2 0 0 0 18 21 24 0 0 19 3 0 12 23 0 0 17 0
        0 0 0 12 11 0 7 3 0 24 17 20 15 13 19 1 0 5 8 0 6 9 0 0 0
        0 22 0 0 14 19 0 6 16 0 0 8 9 7 0 0 0 24 0 0 3 0 0 1 18
        0 0 21 0 0 25 13 0 20 8 12 0 14 0 10 9 16 15 0 6 0 0 4 0 0
        0 0 25 0 0 24 0 0 18 0 4 0 3 10 5 0 1 0 0 14 0 0 0 0 0
        0 0 5 3 0 17 0 0 23 7 13 0 0 0 18 19 21 0 0 22 0 11 12 0 0
        0 0 0 0 18 10 8 0 0 0 0 25 23 2 0 0 5 0 16 11 9 0 3 0 0
        17 20 0 0 2 0 22 16 6 0 0 7 12 0 0 0 0 9 3 0 18 0 23 24 25
        6 0 4 0 16 1 11 12 25 3 19 0 0 0 21 17 23 8 0 18 2 0 0 0 14
        0 0 0 0 4 14 24 11 19 23 21 17 16 8 0 0 0 1 2 9 13 0 0 5 0
        0 1 14 23 0 0 0 0 9 0 0 0 19 5 0 0 24 0 12 0 0 8 17 0 0
        0 16 11 8 0 0 0 0 1 0 6 4 0 0 23 0 15 0 0 0 14 12 9 10 0
        0 21 3 0 0 0 17 0 0 0 0 15 0 25 20 0 0 4 10 0 0 0 16 11 0
        0 0 20 2 0 16 5 8 0 0 0 0 0 0 0  0 6 0 19 25 0 0 0 3 0))

(defparameter *25x25-puzzle*
  (let* ((*print-base* 26))
    (with-output-to-string (s)
      (dolist (x *25x25-raw-data*)
        (format s "~a" x)))))

;; utilities
(defun indexify (list)
  "Return alist mapping index to data (useful for sorting without losing
original index)"
  (mapcar (let ((i 0))
            (lambda (x)
              (cons (prog1 i (incf i)) x)))
          list))
