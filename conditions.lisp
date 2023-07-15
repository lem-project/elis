(defpackage :elis/conditions
  (:use :cl)
  (:export :elis-error
           :parse-input-error))
(in-package :elis/conditions)

(define-condition elis-error (error)
  ((message :initarg :message))
  (:report (lambda (c s)
             (with-slots (message) c
               (format s "ELIS Error: ~A" message)))))

(define-condition parse-input-error (elis-error)
  ())
