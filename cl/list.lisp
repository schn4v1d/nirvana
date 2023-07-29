(in-package common-lisp)

(export '(append listp list))

(defun listp (object)
  (or (consp object) (null object)))

(defun list (&rest objects)
  objects)

(defun append-inner (list next)
  (cond ((atom list)
          (cond ((null next) nil)
                ((atom (car next)) (car next))
                (t (append-inner (car next) (cdr next)))))
        (t (cons (car list) (append-inner (cdr list) next)))))

(defun append (&rest lists)
  (cond ((null lists) nil)
        ((atom (car lists)) (car lists))
        (t (append-inner (car lists) (cdr lists)))))