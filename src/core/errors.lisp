;;;; src/core/errors.lisp
;;;; Error condition definitions

(in-package #:project-juggler)

(define-condition project-juggler-error (error)
  ((message :initarg :message :reader error-message))
  (:documentation "Base error condition for Project Juggler")
  (:report (lambda (condition stream)
             (format stream "Project Juggler Error: ~A"
                     (error-message condition)))))

(define-condition reference-error (project-juggler-error)
  ((reference :initarg :reference :reader error-reference))
  (:documentation "Error resolving a reference")
  (:report (lambda (condition stream)
             (format stream "Cannot resolve reference: ~A~%~A"
                     (error-reference condition)
                     (error-message condition)))))

(define-condition circular-dependency-error (project-juggler-error)
  ((cycles :initarg :cycles :reader error-cycles))
  (:documentation "Circular dependency detected")
  (:report (lambda (condition stream)
             (format stream "Circular dependency detected:~%~{~A~^~%~}"
                     (error-cycles condition)))))

(define-condition scheduling-error (project-juggler-error)
  ()
  (:documentation "Error during scheduling"))

(define-condition validation-error (project-juggler-error)
  ()
  (:documentation "Validation error"))
