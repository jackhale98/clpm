;;;; src/core/errors.lisp
;;;; Error condition definitions

(in-package #:claps)

(define-condition claps-error (error)
  ((message :initarg :message :reader error-message))
  (:documentation "Base error condition for CLAPS")
  (:report (lambda (condition stream)
             (format stream "CLAPS Error: ~A"
                     (error-message condition)))))

;; Backwards compatibility alias
(define-condition project-juggler-error (claps-error)
  ()
  (:documentation "Backwards compatibility alias for claps-error"))

(define-condition reference-error (claps-error)
  ((reference :initarg :reference :reader error-reference))
  (:documentation "Error resolving a reference")
  (:report (lambda (condition stream)
             (format stream "Cannot resolve reference: ~A~%~A"
                     (error-reference condition)
                     (error-message condition)))))

(define-condition circular-dependency-error (claps-error)
  ((cycles :initarg :cycles :reader error-cycles))
  (:documentation "Circular dependency detected")
  (:report (lambda (condition stream)
             (format stream "Circular dependency detected:~%~{~A~^~%~}"
                     (error-cycles condition)))))

(define-condition scheduling-error (claps-error)
  ()
  (:documentation "Error during scheduling"))

(define-condition validation-error (claps-error)
  ()
  (:documentation "Validation error"))
