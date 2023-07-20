(in-package common-lisp)

(export 'dolist)

(nirvana-builtins:%defmacro
 'dolist
 #'(lambda (args env)
     (let* ((info-args (car args))
            (body (cdr args))
            (var (first info-args))
            (list-form (second info-args))
            (result-form (third info-args))
            (list '#:list)
            (tag-next '#:next)
            (tag-step '#:step)
            (tag-exit '#:exit))
       `(let ((,list ,list-form)
              ,var)
          (block nil
            (tagbody
              (go ,tag-step)
              ,tag-next
              (setq ,list (cdr ,list))
              ,tag-step
              (setq ,var (car ,list))
              (if (null ,list)
                  (go ,tag-exit))
              ,@body
              (go ,tag-next)
              ,tag-exit
              ,(if result-form
                   `(return-from nil ,result-form))))))))