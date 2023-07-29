(in-package common-lisp-user)

(nirvana-builtins:%defmacro
 'lambda
 #'(lambda (args env)
     `#'(lambda ,(car args) ,@(cdr args))))

(dolist (x '(1 2 3))
  (print x))