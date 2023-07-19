(in-package common-lisp-user)

(export '(not identity))

(defun not (x)
  (if x nil t))

(defun identity (object)
  object)

(nirvana-builtins:%defmacro 'lambda
           #'(lambda (args env)
               `#'(lambda ,(car args) ,@(cdr args))))

 (block nil
   (let ((x 5))
     (unwind-protect (return-from nil)
       (print x))))