(setq *package* (find-package 'common-lisp))

(export 'in-package)

(%defmacro 'in-package
  #'(lambda (args env)
      `(setq *package* (find-package ,(string (car args))))))