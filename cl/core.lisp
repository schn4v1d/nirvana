(setq *package* (nirvana-builtins:%find-package 'common-lisp))

(nirvana-builtins:%export '(in-package export defun) *package*)

(nirvana-builtins:%defmacro 'defun
  #'(lambda (args env)
      (let ((name (nirvana-builtins:%car args))
            (arglist (nirvana-builtins:%car (nirvana-builtins:%cdr args)))
            (body (nirvana-builtins:%cdr (nirvana-builtins:%cdr args))))
        `(nirvana-builtins:%defun ',name #'(lambda ,arglist (block ,name ,@body))))))

(defun export (symbols &optional (package *package*))
  (nirvana-builtins:%export symbols package))

(nirvana-builtins:%defmacro 'in-package
  #'(lambda (args env)
      `(setq *package* (nirvana-builtins:%find-package ,(string (nirvana-builtins:%car args))))))