(in-package common-lisp-user)

(nirvana-builtins:%defmacro
 'lambda
 #'(lambda (args env)
     `#'(lambda ,(car args) ,@(cdr args))))

(multiple-value-call #'list (values 1 2 3) 4 5)