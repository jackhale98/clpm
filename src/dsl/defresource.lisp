;;;; src/dsl/defresource.lisp
;;;; defresource macro implementation

(in-package #:project-juggler)

;;; ============================================================================
;;; Leave Specification Parsing
;;; ============================================================================

(defun parse-leave-spec (spec)
  "Parse a leave specification into a leave object.

   Supported formats:
     (vacation (date 2024 7 1) (date 2024 7 14))
     (sick (date 2024 3 15) (date 2024 3 17))
     (holiday (date 2024 12 25) (date 2024 12 25))
     (training (date 2024 9 1) (date 2024 9 5) \"Conference\")

   Leave types: vacation, sick, holiday, training, other"
  (let ((leave-type (first spec))
        (start-expr (second spec))
        (end-expr (third spec))
        (description (fourth spec)))
    (make-instance 'leave
                  :type (intern (string-upcase (string leave-type)) :keyword)
                  :start (eval start-expr)
                  :end (eval end-expr)
                  :description description)))

(defun parse-leaves-list (leaves-specs)
  "Parse a list of leave specifications."
  (mapcar #'parse-leave-spec leaves-specs))

;;; ============================================================================
;;; defresource Macro
;;; ============================================================================

(defmacro defresource (id name &body body)
  "Define a resource.

   Usage:
     (defresource dev1 \"Developer 1\"
       :efficiency 1.2
       :rate 150.0
       :daily-limit (duration 8 :hours)
       :weekly-limit (duration 40 :hours)
       :leaves ((vacation (date 2024 7 1) (date 2024 7 14))
                (holiday (date 2024 12 25) (date 2024 12 25)))
       :calendar my-calendar)

   Keywords:
     :efficiency   - Efficiency factor (default 1.0)
     :rate         - Hourly/daily rate
     :limits       - Working time limits (deprecated, use daily-limit/weekly-limit)
     :daily-limit  - Maximum hours per day (duration)
     :weekly-limit - Maximum hours per week (duration)
     :leaves       - List of leave periods
     :calendar     - Custom calendar for this resource

   Body can contain nested resource definitions for teams/groups."

  ;; Parse keyword arguments and body forms
  ;; Walk through body, consuming keyword-value pairs until we hit a non-keyword
  (let ((efficiency-expr nil)
        (rate-expr nil)
        (limits-expr nil)
        (daily-limit-expr nil)
        (weekly-limit-expr nil)
        (leaves-expr nil)
        (calendar-expr nil)
        (forms nil)
        (remaining body))

    (loop while (and remaining (keywordp (first remaining)))
          do (let ((keyword (first remaining))
                   (value (second remaining)))
               (case keyword
                 (:efficiency (setf efficiency-expr value))
                 (:rate (setf rate-expr value))
                 (:limits (setf limits-expr value))
                 (:daily-limit (setf daily-limit-expr value))
                 (:weekly-limit (setf weekly-limit-expr value))
                 (:leaves (setf leaves-expr value))
                 (:calendar (setf calendar-expr value))
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
                                   ,@(when limits-expr `(:limits ,limits-expr))
                                   ,@(when daily-limit-expr `(:daily-limit ,daily-limit-expr))
                                   ,@(when weekly-limit-expr `(:weekly-limit ,weekly-limit-expr))
                                   ,@(when leaves-expr `(:leaves (parse-leaves-list ',leaves-expr)))
                                   ,@(when calendar-expr `(:calendar ,calendar-expr)))))

       ;; Register resource
       (register-resource resource)

       ;; Execute body forms (for nested resources)
       ,@forms

       ;; Return the resource
       resource)))
