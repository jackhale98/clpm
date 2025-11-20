;;;; src/dsl/defresource.lisp
;;;; defresource macro implementation

(in-package #:project-juggler)

(defmacro defresource (id name &body body)
  "Define a resource.

   Usage:
     (defresource dev1 \"Developer 1\"
       :efficiency 1.2
       :rate 150.0)

   Keywords:
     :efficiency  - Efficiency factor (default 1.0)
     :rate        - Hourly/daily rate
     :limits      - Working time limits

   Body can contain nested resource definitions for teams/groups."

  ;; Parse keyword arguments and body forms
  ;; Walk through body, consuming keyword-value pairs until we hit a non-keyword
  (let ((efficiency-expr nil)
        (rate-expr nil)
        (limits-expr nil)
        (forms nil)
        (remaining body))

    (loop while (and remaining (keywordp (first remaining)))
          do (let ((keyword (first remaining))
                   (value (second remaining)))
               (case keyword
                 (:efficiency (setf efficiency-expr value))
                 (:rate (setf rate-expr value))
                 (:limits (setf limits-expr value))
                 (t (warn "Unknown keyword in defresource: ~A" keyword)))
               (setf remaining (cddr remaining))))

    ;; Remaining elements are body forms
    (setf forms remaining)

    `(let ((resource (make-instance 'resource
                                   :id ',id
                                   :name ,name
                                   :project *current-project*
                                   ,@(when efficiency-expr `(:efficiency ,efficiency-expr))
                                   ,@(when rate-expr `(:rate ,rate-expr))
                                   ,@(when limits-expr `(:limits ,limits-expr)))))

       ;; Register resource
       (register-resource resource)

       ;; Execute body forms (for nested resources)
       ,@forms

       ;; Return the resource
       resource)))
