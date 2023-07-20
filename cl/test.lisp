(in-package common-lisp-user)

(export '(not identity))

(defun not (x)
  (if x nil t))

(defun identity (object)
  object)

(nirvana-builtins:%defmacro 'lambda
           #'(lambda (args env)
               `#'(lambda ,(car args) ,@(cdr args))))

(dolist (v '(1 2 3 4) 'end)
  (print v))