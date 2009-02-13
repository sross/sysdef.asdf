(eval-when (:compile-toplevel :load-toplevel :execute)
  (mb:load :tryil))

(defpackage :sysdef.asdf.test (:use :cl :sysdef.asdf :tryil))

(in-package :sysdef.asdf.test)


(defclass my-test-op (operation) ())

(defvar *added* nil)

(defmethod perform ((op my-test-op) (system system))
  (push (list (class-name (class-of op)) (sysdef:name-of system))
        *added*))

(define-test :wrap-execute-test ()
  "Ensure that perform wraps execute"
  (let ((*added* nil))
    (oos 'my-test-op :sysdef.asdf)
    (assert-equal '((my-test-op :sysdef.asdf) (my-test-op :asdf) (my-test-op :alexandria)) *added*)))


(princ (run-tests))





