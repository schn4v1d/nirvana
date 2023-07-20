(in-package common-lisp-user)

(export '(not identity))

(defun not (x)
  (if x nil t))

(defun identity (object)
  object)

(nirvana-builtins:%defmacro 'lambda
           #'(lambda (args env)
               `#'(lambda ,(car args) ,@(cdr args))))

(let (val)
  (tagbody
    (setq val 2)
    (go lp)
    (setq val 4)
   lp
    (setq val 3))
  val)