(in-package common-lisp)

(nirvana-builtins:%defmacro
 'cond
 #'(lambda (form env)
     (if (null (cdr form))
         nil
         (let ((test-form (caadr form))
               (forms (cdadr form))
               (rest (cddr form)))
           `(if ,test-form
                (progn ,@forms)
                (cond ,@rest))))))

(nirvana-builtins:%defmacro
 'or
 #'(lambda (form env)
     (if (null (cdr form))
         nil
         (let ((res '#:res)
               (cond (cadr form))
               (rest (cddr form)))
           `(let ((,res ,cond))
              (if ,res
                  ,res
                  (or ,@rest)))))))

(nirvana-builtins:%defmacro
 'and
 #'(lambda (form env)
     (if (null (cdr form))
         t
         (let ((res '#:res)
               (cond (cadr form))
               (rest (cddr form)))
           `(let ((,res ,cond))
              (if ,res
                  (and ,@rest)
                  nil))))))