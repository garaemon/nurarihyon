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
    (set-macro-character #\[ #'aref-reader-open)
    (set-macro-character #\] (get-macro-character #\))))
  t)

(defun aref-reader-open (stream char)
  ;; call after read [
  ;; this function convert [hoge a b] => (aref hoge a b)
  (declare (ignore char))
  (let ((word-list (read-delimited-list #\] stream t)))
    (cons 'aref word-list)))

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
