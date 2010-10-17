;;================================================
;; util.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(in-package :nurarihyon)

(defun symbol->keyword (sym)
  (intern (string sym) :keyword))

(defun list-rank (list)
  "returns the rank of lst.

   ;;; (list-rank nil) -> 0
   ;;; (list-rank '(1 2 3)) -> 1
   ;;; (list-rank '((1 2 3))) -> 2"
  (if (atom list)
      0
      (max (1+ (list-rank (car list)))
           (list-rank (cdr list)))))
