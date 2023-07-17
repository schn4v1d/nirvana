(setq *package* (find-package 'common-lisp))
(export '(endp caar cadr cdar cddr caaar caadr cadar caddr cdaar
          cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
          cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar
          cddadr cdddar cddddr first second third fourth fifth
          sixth seventh eighth ninth tenth not identity))

(defun caar (x)
  (car (car x)))

(defun cadr (x)
  (car (cdr x)))

(defun cdar (x)
  (cdr (car x)))

(defun cddr (x)
  (cdr (cdr x)))

(defun caaar (x)
  (car (caar x)))

(defun caadr (x)
  (car (cadr x)))

(defun cadar (x)
  (car (cdar x)))

(defun caddr (x)
  (car (cddr x)))

(defun cdaar (x)
  (cdr (caar x)))

(defun cdadr (x)
  (cdr (cadr x)))

(defun cddar (x)
  (cdr (cdar x)))

(defun cdddr (x)
  (cdr (cddr x)))

(defun caaaar (x)
  (car (caaar x)))

(defun caaadr (x)
  (car (caadr x)))

(defun caadar (x)
  (car (cadar x)))

(defun caaddr (x)
  (car (caddr x)))

(defun cadaar (x)
  (car (cdaar x)))

(defun cadadr (x)
  (car (cdadr x)))

(defun caddar (x)
  (car (cddar x)))

(defun cadddr (x)
  (car (cdddr x)))

(defun cdaaar (x)
  (cdr (caaar x)))

(defun cdaadr (x)
  (cdr (caadr x)))

(defun cdadar (x)
  (cdr (cadar x)))

(defun cdaddr (x)
  (cdr (caddr x)))

(defun cddaar (x)
  (cdr (cdaar x)))

(defun cddadr (x)
  (cdr (cdadr x)))

(defun cdddar (x)
  (cdr (cddar x)))

(defun cddddr (x)
  (cdr (cdddr x)))

(defun first (list)
  (car list))

(defun second (list)
  (car (cdr list)))

(defun third (list)
  (car (cddr list)))

(defun fourth (list)
  (car (cdddr list)))

(defun fifth (list)
  (car (cddddr list)))

(defun sixth (list)
  (car (cdr (cddddr list))))

(defun seventh (list)
  (car (cddr (cddddr list))))

(defun eighth (list)
  (car (cdddr (cddddr list))))

(defun ninth (list)
  (car (cddddr (cddddr list))))

(defun tenth (list)
  (car (cdr (cddddr (cddddr list)))))

(defun endp (list)
  (null list))

(defun not (x)
  (if x nil t))

(defun identity (object)
  object)

(setq *package* (find-package 'common-lisp-user))

(not nil)
(not '())
(not (integerp 'sss))
(not (integerp 1))
(not 'apple)