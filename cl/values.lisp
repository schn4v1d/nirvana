(in-package common-lisp)

(export '(multiple-value-list))

(nirvana-builtins:%defmacro
 'multiple-value-list
 #'(lambda (form env)
     `(multiple-value-call #'list (second form))))

(nirvana-builtins:%defmacro
 'multiple-value-setq
 #'(lambda (form env)
     (let ((vars (second form))
           (form (third form))
           (results '#:results))
       `(let* ((,results (multiple-value-list ,form))
                   )))))