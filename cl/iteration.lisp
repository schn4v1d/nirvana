(in-package common-lisp)

(export '(psetq do do* dotimes dolist))

(nirvana-builtins:%defmacro
 'psetq
 #'(lambda (form env)
     (let* ((args (cdr form))
            values
            assignments)
       (tagbody
         begin
         (if (null args)
             (go end))
         (let ((val '#:val))
           (setq values (cons `(,val ,(second args)) values))
           (setq assignments (cons val (cons (first args) assignments))))
         (setq args (cddr args))
         (go begin)
         end)
       (setq values (nreverse values) assignments (nreverse assignments))
       `(let ,values
          (setq ,@assignments)))))

(defun do-expander (form env)
  (let* ((args (cdr form))
         (var-forms (car args))
         (end-forms (cadr args))
         (body (cddr args))
         var-inits
         var-steps)
    (tagbody
      begin
      (if (null var-forms)
          (go end))
      (let ((var-form (car var-forms)))
        (cond ((atom var-form) (setq var-inits (cons var-form var-inits)))
              ((null (cdr var-form)) (setq var-inits (cons var-form
                                                           var-inits)))
              ((null (cddr var-form)) (setq var-inits (cons var-form
                                                            var-inits)))
              (t (setq var-inits (cons `(,(first var-form) ,(second var-form))
                                       var-inits))
                 (setq var-steps (cons (third var-form)
                                       (cons (first var-form) var-steps))))))
      (setq var-forms (cdr var-forms))
      (go begin)
      end)
    (setq var-inits (nreverse var-inits) var-steps (nreverse var-steps))
    (let ((end-test-form (car end-forms))
          (result-forms (cdr end-forms))
          (body-tag '#:begin)
          (step-tag '#:step)
          (end-tag '#:end))
      `(block nil
         (,(cond ((eq (car form) 'do) 'let)
                 ((eq (car form) 'do*) 'let*)) ,var-inits
           (tagbody
             (go ,body-tag)
             ,step-tag
             ,(cond ((eq (car form) 'do) `(psetq ,@var-steps))
                    ((eq (car form) 'do*) `(setq ,@var-steps)))
             ,body-tag
             (if ,end-test-form
                 (go ,end-tag))
             ,@body
             (go ,step-tag)
             ,end-tag
             (return-from nil (progn ,@result-forms))))))))

(nirvana-builtins:%defmacro
 'do
 #'do-expander)

(nirvana-builtins:%defmacro
 'do*
 #'do-expander)

(nirvana-builtins:%defmacro
 'dotimes
 #'(lambda (form env)
     (let* ((args (cdr form))
            (var (first (car args)))
            (count-form (second (car args)))
            (result-form (third (car args)))
            (body (cdr args))
            (count '#:count))
       `(let ((,count ,count-form))
          (do ((,var 0 (+ 1 ,var)))
              ((eq ,var ,count) ,result-form)
            ,@body)))))

(nirvana-builtins:%defmacro
 'dolist
 #'(lambda (form env)
     (let* ((args (cdr form))
            (var (first (car args)))
            (list-form (second (car args)))
            (result-form (third (car args)))
            (body (cdr args))
            (list '#:list))
       `(do* ((,list ,list-form (cdr ,list))
              (,var (car ,list) (car ,list)))
            ((null ,list) ,result-form)
          ,@body))))

(defun nth (n list)
  (dotimes (i n)
    (setq list (cdr list)))
  (car list))

(defun make-list (size)
  (let (result)
    (dotimes (i size)
      (setq result (cons nil result)))
    result))

(defun mapcar (function &rest lists)
  (when (null lists)
      (error "invalid argument count 1 for mapcar"))
  (let (result)
    (do ())))