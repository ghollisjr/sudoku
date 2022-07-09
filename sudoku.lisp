(defpackage #:sudoku
  (:use :cl)
  (:export
   :sudoku
   :solve
   :solve-from-string
   :check
   :read-sudoku
   :write-sudoku))

(in-package :sudoku)
           
(declaim (optimize (debug 3)
                   (speed 0)
                   (safety 3)
                   (space 0)
                   (compilation-speed 0)))

;; Anaphoric macros
(defmacro aif (test then &optional else)
  "Binds the resulting value of test to 'it and executes if statement as usual"
  `(let ((it ,test))
     (if it ,then ,@(when else `(,else)))))

(defmacro awhen (test &body body)
  "Binds the resulting value of test to 'it and executes when statement"
  `(let ((it ,test))
     (when it
       ,@body)))

(defmacro aunless (test &body body)
  "Binds the resulting value of test to 'it and executes unless statement"
  `(let ((it ,test))
     (unless it
       ,@body)))

(defparameter *n-values* 9
  "Number of values for sudoku cells.")

;; Integer bitvector API
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

;; utilities
(defun copy-sudoku-array (array)
  (let* ((dims (array-dimensions array))
         (type (array-element-type array))
         (result (make-array dims
                             :element-type type
                             :initial-element 0)))
    (dotimes (i *n-values*)
      (dotimes (j *n-values*)
        (setf (aref result i j)
              (aref array i j))))
    result))

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
(defmethod initialize-instance :after ((s sudoku) &key &allow-other-keys)
  (with-slots (values possible) s
    (unless possible
      (setf possible
            (make-array (list *n-values* *n-values*)
                        :initial-element (make-full-intvec)
                        :element-type (list 'integer (make-full-intvec))))
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
                  (error "Contradiction"))))))))))

(defun copy-sudoku (s)
  (with-slots (values possible) s
    (make-instance 'sudoku
                   :values (copy-sudoku-array values)
                   :possible (copy-sudoku-array possible))))

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
      (when (or (digit-char-p c radix)
                (char= c #\.))
        (incf index)
        (when (char= c #\.)
          (setf c #\0))
        (let ((val (parse-integer (string c)
                                  :radix radix)))
          (setf (aref values i j) val))))))

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
