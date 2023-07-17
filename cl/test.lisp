(setq *package* (find-package 'common-lisp))

(defun endp (list)
  (null list))

(setq *package* (find-package 'common-lisp-user))

(endp nil)
(endp ())
(endp '(1))
(endp (cdr '(1)))