;;================================================
;; syntax.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(in-package :nurarihyon)
;; syntax
(defvar *original-readtable* nil)
(defun %enable-aref-reader-syntax ()
  (unless *original-readtable*          ;not enabled
    (setq *original-readtable* *readtable*)
    (setq *readtable* (copy-readtable))
    ;; for [vec 0]
    (set-macro-character #\[ #'aref-reader-open)
    (set-macro-character #\] (get-macro-character #\)))
    ;; for #d(1 2 3)
    (set-dispatch-macro-character #\# #\d 'double-array-open)
    )
  t)

(defun aref-reader-open (stream char)
  ;; call after read [
  ;; this function convert [hoge a b] => (aref hoge a b)
  (declare (ignore char))
  (let ((word-list (read-delimited-list #\] stream t)))
    (cons 'aref word-list)))

(defun double-array-open (stream n char)
  (declare (ignore n char))
  ;; #d(....)
  (let ((in-list (read stream)))
    (if (= (chimi:list-rank in-list) 1)
        (cons 'double-vector in-list)
        ;; here, in-list is a list of list of number
        (cons 'double-matrix (mapcar #'(lambda (x)
                                         (cons 'list x))
                                     in-list)))))

(defun %disable-aref-reader-syntax ()
  (when *original-readtable*
    (setq *readtable* *original-readtable*
          *original-readtable* nil))
  t)

(defmacro enable-aref-reader-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-aref-reader-syntax)))

(defmacro disable-aref-reader-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%disable-aref-reader-syntax)))
