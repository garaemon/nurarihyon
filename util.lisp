;;================================================
;; util.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(in-package :nurarihyon)

(defun symbol->keyword (sym)
  (intern (string sym) :keyword))
