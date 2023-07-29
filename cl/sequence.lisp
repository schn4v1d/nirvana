(in-package common-lisp)

(export '(reverse nreverse))

(defun reverse-list (list)
  (cond ((null list)
          list)
        ;; one-element
        ((null (cdr list)) (cons (car list) nil))
        ;; two-element
        ((null (cddr list)) (cons (cadr list) (cons (car list) nil)))
        ;; three+-element
        (t (append
             (reverse-list (cddr list))
             (cons (cadr list) nil)
             (cons (car list) nil)))))

(defun reverse (sequence)
  (cond ((listp sequence) (reverse-list sequence))
        (t (error "todo"))))

(defun nreverse (sequence)
  ;; TODO not wrong but inefficient
  (reverse sequence))