;;;; src/core/protocols.lisp
;;;; Generic function protocols

(in-package #:project-juggler)

;;; Temporal type protocols

(defgeneric duration-in-seconds (duration)
  (:documentation "Convert duration to seconds"))

(defgeneric duration-in-minutes (duration)
  (:documentation "Convert duration to minutes"))

(defgeneric duration-in-hours (duration)
  (:documentation "Convert duration to hours"))

(defgeneric duration-in-days (duration)
  (:documentation "Convert duration to days"))

(defgeneric duration-in-weeks (duration)
  (:documentation "Convert duration to weeks"))

;;; Task protocols

(defgeneric task-ready-p (task)
  (:documentation "Check if task is ready for scheduling"))

(defgeneric schedule-task (task scenario)
  (:documentation "Schedule a single task"))

;;; Validation protocols

(defgeneric validate-project (project)
  (:documentation "Validate project"))

(defgeneric finalize-project (project)
  (:documentation "Finalize project - resolve references and validate"))

;;; Reference resolution protocols

(defgeneric resolve-task-reference (reference &optional project)
  (:documentation "Resolve a task reference to actual task object"))

(defgeneric resolve-resource-reference (reference &optional project)
  (:documentation "Resolve a resource reference to actual resource object"))

(defgeneric register-task (task &optional namespace)
  (:documentation "Register task in namespace and project"))

(defgeneric register-resource (resource &optional namespace)
  (:documentation "Register resource in namespace and project"))
