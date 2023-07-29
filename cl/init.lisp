(setq *package* (nirvana-builtins:%find-package 'common-lisp))

(nirvana-builtins:%export
 '(defun in-package export not identity funcall load)
 *package*)

(nirvana-builtins:%defmacro
 'defun
 #'(lambda (form env)
     (let* ((args (nirvana-builtins:%cdr form))
            (name (nirvana-builtins:%car args))
            (arglist (nirvana-builtins:%car (nirvana-builtins:%cdr args)))
            (body (nirvana-builtins:%cdr (nirvana-builtins:%cdr args))))
       `(nirvana-builtins:%defun ',name #'(lambda ,arglist (block ,name ,@body))))))

(defun export (symbols &optional (package *package*))
  (nirvana-builtins:%export symbols package))

(nirvana-builtins:%defmacro 'in-package
                            #'(lambda (form env)
                                `(setq *package* (nirvana-builtins:%find-package ,(string (nirvana-builtins:%car
                                                                                           (nirvana-builtins:%cdr form)))))))

(defun not (x)
  (if x nil t))

(defun identity (object)
  object)

(defun funcall (function &rest args)
  (nirvana-builtins:%funcall function args))

(defun load (pathname)
  (nirvana-builtins:%load pathname))

(load "cons.lisp")
(load "logic.lisp")
(load "list.lisp")
(load "sequence.lisp")
(load "iteration.lisp")
(load "values.lisp")
(load "defmacro.lisp")
(load "setf.lisp")