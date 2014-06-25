(in-package :utils)

(defmacro defconstant (sym val &optional doc)
  "Make sure that VAL is only evaluated once."
  `(cl:defconstant ,sym (if (boundp ',sym) (symbol-value ',sym) ,val)
     ,@(when doc (list doc))))

(defmacro when-bind ((var expr) &body body)
  "Bind the evaluation of EXPR to VAR and evaluate BODY if it's non nil."
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro aif (test then &optional else)
  "Behaves exactly like IF except that the result of the evaluation of
TEST is bound to the variable IT."
  (let ((it (intern "IT")))
    `(let ((,it ,test))
       (declare (ignorable ,it))
       (if ,it ,then ,else))))
