;;================================================
;; syntax.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(in-package :nurarihyon)
;; syntax
(defvar *original-readtable* nil)
(defun %enable-nurarihyon-reader-syntax ()
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

(defun read-infix-sexp (stream n char)
  (declare (ignore n char))
  (let ((sexp (read stream)))
    (infix->prefix sexp)))

(defun infix->prefix (sexp)
  (cond
    ((symbolp sexp)
     sexp)                              ;just return
    ((listp sexp)                       ;we need to convert
     ;; (1 + 2) (1 + sin(3)) (a = hoge(fuga)) ...
     (let ((1arg (car sexp))
           (2arg (cadr sexp))
	   (3arg (cddr sexp)))
       (cond ((symbolp 2arg)
              (let ((key (chimi:symbol->keyword 2arg)))
                (case key
                  ((:= :<-)
                   (cons (cons 'setf (infix->prefix 1arg))
                           (infix->prefix 3arg)))
                  (:->
                   (cons (cons 'setf (infix->prefix 3arg))
                           (infix->prefix 1arg)))
                  (t
                   (cons (cons 2arg (infix->prefix 1arg))
                           (infix->prefix 3arg)))
                  ))))))
    (t sexp))                            ;literal
  )
    

(defun %disable-nurarihyon-reader-syntax ()
  (when *original-readtable*
    (setq *readtable* *original-readtable*
          *original-readtable* nil))
  t)

(defmacro enable-nurarihyon-reader-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-nurarihyon-reader-syntax)))

(defmacro disable-nurarihyon-reader-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%disable-nurarihyon-reader-syntax)))
