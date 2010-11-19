;;================================================
;; util.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(in-package :nurarihyon)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *nurarihyon-optimization* nil))

;; used in syntax.lisp
(defun symbol->keyword (sym)
  "convert a symbol to a keyword.

 example::

  (sybol-keyword 'hoge) => :hoge"
  (intern (string sym) :keyword))

;; used in syntax.lisp
(defun list-rank (list)
  "returns the rank of lst.

 example::

  (list-rank nil) => 0
  (list-rank '(1 2 3)) => 1
  (list-rank '((1 2 3))) => 2"
  (if (atom list)
      0
      (max (1+ (list-rank (car list)))
           (list-rank (cdr list)))))

(defun optimize-function-name (funcname)
  "take a symbol, FUNCNAME, and return the name for optimized function."
  (intern (concatenate 'string "$" (string funcname))))

(defmacro define-nhfun (funcname args documentation &rest form)
  "DEFINE-NHFUN defines two functions and export them:

one is the function which has FUCNAME as its own name and compiled with
(optimize (safety 3) (debug 3) (space 0)). this is compiled with
*NURARIHYON-OPTIMIZATION* NIL.

another is the function which has $FUCNAME as its own name and compiled with
(optimize (safety 3) (debug 1) (space 0) (safety 0)). this is compiled with
*NURARIHYON-OPTIMIZATION* T."
  (let ((fast-funcname (optimize-function-name funcname)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (export (list ',funcname ',fast-funcname))
       (setq *nurarihyon-optimization* nil) ;let cannot be used, why?
       (defun ,funcname ,args
         ,documentation
         (declare (optimize (safety 3) (debug 3) (space 0)))
         ,@form)
       (setq *nurarihyon-optimization* t) ;let cannot be used, why?
       (defun ,fast-funcname ,args
         ,documentation
         (declare (optimize (speed 3) (safety 0)
                            (debug 1) (space 0)))
         ,@form))))

(defmacro declaim-inline-nhfun (funcname)
  "declaim FUCNAME and $FUNCNAME as inline functions."
  (let ((fast-funcname (optimize-function-name funcname)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (declaim (inline ,funcname))
       (declaim (inline ,fast-funcname)))))

