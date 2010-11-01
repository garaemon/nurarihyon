;;================================================
;; syntax.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :nurarihyon)
;; syntax
(defvar *original-readtable* nil "variable to store the original read table")

(defun %enable-nurarihyon-reader-syntax ()
  "internal function called from enable-nurarihyon-reader-syntax."
  (unless *original-readtable*          ;not enabled
    (setq *original-readtable* *readtable*)
    (setq *readtable* (copy-readtable))
    ;; for [vec 0]
    (set-macro-character #\[ #'aref-reader-open)
    (set-macro-character #\] (get-macro-character #\)))
    ;; for #d(1 2 3)
    (set-dispatch-macro-character #\# #\d 'double-array-open)
    ;; for #%(1 + 2)
    (set-dispatch-macro-character #\# #\% 'read-infix-sexp))
  t)

(defun aref-reader-open (stream char)
  "called after reading the character [.
this function convert \"[hoge a b]\" to (aerf hoge a b)."
  (declare (ignore char))
  (let ((word-list (read-delimited-list #\] stream t)))
    (cons 'aref word-list)))

(defun double-array-open (stream n char)
  "called after reading the character d.
this function convert #d(1 2 3) to (double-vector 1 2 3) or
#d((1 2 3) (4 5 6)) to (double-matrix ((1 2 3) (4 5 6))."
  (declare (ignore n char))
  (let ((in-list (read stream)))
    (if (= (list-rank in-list) 1)       ;check a vector or matrix
        (cons 'double-vector in-list)
      ;; here, in-list is a list of list of number
      (cons 'double-matrix (mapcar #'(lambda (x) (cons 'list x)) in-list)))))

(defun read-infix-sexp (stream n char)
  (declare (ignore n char))
  (let ((sexp (read stream)))
    (infix->prefix sexp)))

(defun infix->prefix/split$ (arg &optional (buf nil) (result nil))
  "this function separete arg by the symbol `$'"
  ;; first, just separate arg by $.
  ;; (1 $ 2 $ 3) -> ((3) (2) (1))
  ;; (1 + 2 $ 3) -> ((3) (1 + 2))
  ;; (1 + 2 $ 3) -> (+ 2 $ 3) (1)           ()
  ;;             -> (2 $ 3)   (1 +)         ()
  ;;             -> ($ 3)     (1 + 2)       ()
  ;;             -> (3)       ()            ((1 + 2))
  ;;             -> ()        (3)           ((1 + 2))
  ;;             ->                         ((1 + 2) (3))
  (cond ((null arg) (append result (list buf)))
        ((and (symbolp (car arg))
              (eq (symbol->keyword (car arg)) :$))
         (infix->prefix/split$ (cdr arg) nil (append result (list buf))))
        (t
         (infix->prefix/split$ (cdr arg) (append buf (list (car arg)))
                               result))))

(defun infix->prefix/function-call (a b c)
  "For example,
a := sin
b := (1)
c := another s-expression..."
  ;; its deficult to estimate the number of arguments of b.
  ;; so we utilize another syntax `$' for separate arguments.
  (let ((function-sexp
         (cons a (mapcar #'infix->prefix (infix->prefix/split$ b)))))
    (if c
        ;; if there is c, we need to resolve c to operator and its args.
        (destructuring-bind (operator &rest args) c
          (list operator function-sexp (infix->prefix args)))
      function-sexp)))

(defun %infix->prefix (sexp)
  (destructuring-bind (a &optional b &rest c) sexp ;(a b . c)
    (cond ((and (not (null b)) (listp b))  ;when b is list
           ;; here, we check sexp like (sin(x) ...)
           (infix->prefix/function-call a b c))
          ((and a b c)
           (let ((bsym (symbol->keyword b)))
             (case bsym
               (:@
                ;; @ works as aref a @ (1 2) -> (aref a 1 2)
                ;; a @ 1 -> (aref a 1)
                ;; here we need to think aboud (cdr c) too...
                (destructuring-bind (index &rest args) c
                  (let ((this-section
                         (if (listp index)
                             (append (list 'aref a) index)
                           (append (list 'aref a) (list index)))))
                    (if args
                        (destructuring-bind (operator &rest op-args) args
                          (list (infix->prefix operator) ;no need?
                                this-section
                                (infix->prefix op-args)))
                      this-section))))
               (t
                (list (infix->prefix b)
                      (infix->prefix a) (infix->prefix c))))))
          ((and b (null c)) ; no c, it means function appling like sin(x)
           (list (infix->prefix a) (infix->prefix b)))
          ((and (null b) (null c)) (infix->prefix a))))) ;only a

(defun infix->prefix (sexp)
  "This function converts an infix s-expression to a prefix s-expression."
  (cond
   ((and (symbolp sexp)
         (or (eq (symbol->keyword sexp) :<-)
             (eq (symbol->keyword sexp) :=)))
    'setf)                              ;setf alias
   ((listp sexp) (%infix->prefix sexp)) ;we need to convert
   (t sexp)))                           ;may be literal

(defun %disable-nurarihyon-reader-syntax ()
  (when *original-readtable*
    (setq *readtable* *original-readtable*
          *original-readtable* nil))
  t)

(defmacro enable-nurarihyon-reader-syntax ()
  "enable nurarihyon reader macro. the old read table will
be stored *original-readtable*.

nurarihyon support followin syntax:
 1. array accessor
 you can use [] syntax instead of aref.
  example:
    [foo 1] => (aref foo 1)
    [bar 1 2] => (aref bar 1 2)

 2. double array literal.
 you can use #d syntax for double-float matrix.
  example:
    #d(1 2 3) => #(1.0d0 2.0d0 3.0d0)
    #d((1 2 3) (4 5 6)) => #2A((1.0d0 2.0d0 3.0d0) (4.0d0 5.0d0 6.0d0))

 3. infix syntax
 you can use infix style to describe math formulas within #% macro character.
 in infyx syntax, you need to use $ character to separate the arguments passed
 into functions.
  example:
    #%(1 + 2) => (+ 1 2) => 3
    #%(1 + sin(2.0)) => 1.9092975
    #%(add(3 $ 2) - 4) => 1 where add = (lambda (a b) (+ a b))
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-nurarihyon-reader-syntax)))

(defmacro disable-nurarihyon-reader-syntax ()
  "disable nurarihyon reader macro. this function just set *readtable*
with *original-readtable* and reset *original-readtable*."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%disable-nurarihyon-reader-syntax)))
