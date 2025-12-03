;;;; src/dsl/deftask.lisp
;;;; deftask macro implementation

(in-package #:project-juggler)

;;; ============================================================================
;;; Dependency Specification Parsing
;;; ============================================================================

(defun parse-dependency-spec (spec)
  "Parse a dependency specification into (target-ref type lag) values.

   Supported formats:
     task1                              -> (task1 :fs nil)
     (task1)                            -> (task1 :fs nil)
     (task1 :type :ss)                  -> (task1 :ss nil)
     (task1 :type :fs :lag dur)         -> (task1 :fs dur)
     (task1 :lag dur)                   -> (task1 :fs dur)

   Types: :fs (finish-to-start), :ss (start-to-start),
          :ff (finish-to-finish), :sf (start-to-finish)"
  (cond
    ;; Simple symbol: task1
    ((symbolp spec)
     (list spec :fs nil))

    ;; List form: (task1 ...) or just (task1)
    ((listp spec)
     (let ((target-ref (first spec))
           (dep-type :fs)
           (lag nil)
           (rest (rest spec)))
       ;; Parse keyword arguments
       (loop while rest do
         (let ((key (first rest))
               (val (second rest)))
           (case key
             (:type (setf dep-type
                          (case val
                            ((:fs :finish-start :finish-to-start) :fs)
                            ((:ss :start-start :start-to-start) :ss)
                            ((:ff :finish-finish :finish-to-finish) :ff)
                            ((:sf :start-finish :start-to-finish) :sf)
                            (t (warn "Unknown dependency type ~A, defaulting to :fs" val)
                               :fs))))
             (:lag (setf lag val))
             (t (warn "Unknown dependency keyword ~A" key))))
         (setf rest (cddr rest)))
       (list target-ref dep-type lag)))

    (t
     (error "Invalid dependency specification: ~A" spec))))

;;; ============================================================================
;;; Allocation Specification Parsing
;;; ============================================================================

(defun parse-allocation-spec (spec)
  "Parse an allocation specification into (resource-ref percent) values.

   Supported formats:
     dev1                       -> (dev1 100)
     (dev1)                     -> (dev1 100)
     (dev1 :percent 50)         -> (dev1 50)

   Returns a list: (resource-ref percent)"
  (cond
    ;; Simple symbol: dev1
    ((symbolp spec)
     (list spec 100))

    ;; List form: (dev1 ...) or just (dev1)
    ((listp spec)
     (let ((resource-ref (first spec))
           (percent 100)
           (rest (rest spec)))
       ;; Parse keyword arguments
       (loop while rest do
         (let ((key (first rest))
               (val (second rest)))
           (case key
             (:percent (setf percent val))
             (t (warn "Unknown allocation keyword ~A" key))))
         (setf rest (cddr rest)))
       (list resource-ref percent)))

    (t
     (error "Invalid allocation specification: ~A" spec))))

(defun parse-allocations-list (specs)
  "Parse a list of allocation specifications.
   Returns (resource-refs resource-percents) where:
   - resource-refs is a list of resource symbols
   - resource-percents is an alist of (resource-ref . percent)"
  (let ((resource-refs nil)
        (resource-percents nil))
    (dolist (spec specs)
      (let ((parsed (parse-allocation-spec spec)))
        (push (first parsed) resource-refs)
        (push (cons (first parsed) (second parsed)) resource-percents)))
    (list (nreverse resource-refs) (nreverse resource-percents))))

;;; ============================================================================
;;; PERT Estimate Parsing
;;; ============================================================================

(defun parse-estimate-spec (spec)
  "Parse a PERT estimate specification into a pert-estimate object.

   Supported formats:
     (:optimistic dur :likely dur :pessimistic dur)
     (:optimistic dur :pessimistic dur)  ; likely defaults to midpoint

   Returns a pert-estimate object."
  (let ((optimistic nil)
        (likely nil)
        (pessimistic nil)
        (rest spec))
    (loop while rest do
      (let ((key (first rest))
            (val (second rest)))
        (case key
          (:optimistic (setf optimistic (eval val)))
          (:likely (setf likely (eval val)))
          (:pessimistic (setf pessimistic (eval val)))
          (t (warn "Unknown estimate keyword ~A" key))))
      (setf rest (cddr rest)))
    ;; If likely not provided, use midpoint
    (unless likely
      (when (and optimistic pessimistic)
        (setf likely (duration (/ (+ (duration-in-days optimistic)
                                     (duration-in-days pessimistic))
                                  2)
                               :days))))
    (make-instance 'pert-estimate
                  :optimistic optimistic
                  :likely likely
                  :pessimistic pessimistic)))

;;; ============================================================================
;;; Constraint Specification Parsing
;;; ============================================================================

(defun parse-constraint-spec (spec)
  "Parse a constraint specification into a task-constraint object.

   Supported formats:
     (:snet date)   ; Start No Earlier Than
     (:snlt date)   ; Start No Later Than
     (:mso date)    ; Must Start On
     (:fnet date)   ; Finish No Earlier Than
     (:fnlt date)   ; Finish No Later Than
     (:mfo date)    ; Must Finish On

   Returns a task-constraint object."
  (let ((ctype (first spec))
        (cdate (eval (second spec))))
    (make-task-constraint ctype cdate)))

;;; ============================================================================
;;; Recurring Specification Parsing
;;; ============================================================================

(defun parse-recurring-spec (spec)
  "Parse a recurring specification into a recurring-definition object.

   Supported formats:
     (:daily :start date :end date)
     (:weekly :day :friday :start date :end date)
     (:workdays :start date :end date)
     (:monthly :day 1 :start date :end date)
     (:days (:monday :wednesday) :start date :end date)
     (:daily :start date :end date :except (date1 date2))

   Returns a recurring-definition object."
  (let ((frequency (first spec))
        (start-date nil)
        (end-date nil)
        (day nil)
        (days nil)
        (exceptions nil)
        (rest (rest spec)))
    ;; Handle :days specially - first arg is list of days
    (when (eq frequency :days)
      (setf days (first rest))
      (setf rest (rest rest)))
    ;; Parse keyword arguments
    (loop while rest do
      (let ((key (first rest))
            (val (second rest)))
        (case key
          (:start (setf start-date (eval val)))
          (:end (setf end-date (eval val)))
          (:day (setf day val))
          (:except (setf exceptions (mapcar #'eval val)))
          (t (warn "Unknown recurring keyword ~A" key))))
      (setf rest (cddr rest)))
    (make-instance 'recurring-definition
                  :frequency frequency
                  :start-date start-date
                  :end-date end-date
                  :day day
                  :days days
                  :exceptions exceptions)))

(defmacro deftask (id name &body body)
  "Define a task.

   Usage:
     (deftask task1 \"Task 1\"
       :effort (duration 40 :hours)
       :priority 700
       :depends-on (task0)
       :allocate (dev1 dev2))

   Extended dependency syntax:
     :depends-on (task0)                           ; Simple FS dependency
     :depends-on ((task0 :type :ss))               ; Start-to-Start
     :depends-on ((task0 :type :ff))               ; Finish-to-Finish
     :depends-on ((task0 :type :sf))               ; Start-to-Finish
     :depends-on ((task0 :lag (duration 2 :days))) ; FS with 2-day lag
     :depends-on ((task0 :type :ss :lag (duration -1 :days)))  ; SS with 1-day lead

   Keywords:
     :effort      - Effort duration
     :duration    - Calendar duration
     :start       - Fixed start date
     :end         - Fixed end date
     :priority    - Priority (default 500)
     :milestone   - Boolean, is this a milestone?
     :complete    - Completion percentage (0-100)
     :depends-on  - List of task IDs or dependency specs
     :allocate    - List of resource IDs to allocate
     :fixed-cost  - Fixed cost (non-labor costs like licenses, equipment)
     :estimate    - PERT three-point estimate plist
     :start-constraint - Start date constraint (:snet, :snlt, :mso)
     :finish-constraint - Finish date constraint (:fnet, :fnlt, :mfo)
     :recurring   - Recurring task definition

   Body can contain subtask definitions."

  ;; Parse keyword arguments and body forms
  ;; Walk through body, consuming keyword-value pairs until we hit a non-keyword
  (let ((effort-expr nil)
        (duration-expr nil)
        (start-expr nil)
        (end-expr nil)
        (priority-expr nil)
        (milestone-expr nil)
        (complete-expr nil)
        (fixed-cost-expr nil)
        (depends-on-list nil)
        (allocate-list nil)
        (estimate-expr nil)
        (start-constraint-expr nil)
        (finish-constraint-expr nil)
        (recurring-expr nil)
        (forms nil)
        (remaining body))

    (loop while (and remaining (keywordp (first remaining)))
          do (let ((keyword (first remaining))
                   (value (second remaining)))
               (case keyword
                 (:effort (setf effort-expr value))
                 (:duration (setf duration-expr value))
                 (:start (setf start-expr value))
                 (:end (setf end-expr value))
                 (:priority (setf priority-expr value))
                 (:milestone (setf milestone-expr value))
                 (:complete (setf complete-expr value))
                 (:fixed-cost (setf fixed-cost-expr value))
                 (:depends-on (setf depends-on-list value))
                 (:allocate (setf allocate-list value))
                 (:estimate (setf estimate-expr value))
                 (:start-constraint (setf start-constraint-expr value))
                 (:finish-constraint (setf finish-constraint-expr value))
                 (:recurring (setf recurring-expr value))
                 (t (warn "Unknown keyword in deftask: ~A" keyword)))
               (setf remaining (cddr remaining))))

    ;; Remaining elements are body forms
    (setf forms remaining)

    `(let* ((parent-task *current-task*)
            (task (make-instance 'task
                                :id ',id
                                :name ,name
                                :project *current-project*
                                :parent parent-task
                                ,@(when effort-expr `(:effort ,effort-expr))
                                ,@(when duration-expr `(:duration ,duration-expr))
                                ,@(when start-expr `(:start ,start-expr))
                                ,@(when end-expr `(:end ,end-expr))
                                ,@(when priority-expr `(:priority ,priority-expr))
                                ,@(when milestone-expr `(:milestone ,milestone-expr))
                                ,@(when complete-expr `(:complete ,complete-expr))
                                ,@(when fixed-cost-expr `(:fixed-cost ,fixed-cost-expr))
                                ,@(when estimate-expr `(:estimate (parse-estimate-spec ',estimate-expr)))
                                ,@(when start-constraint-expr `(:start-constraint (parse-constraint-spec ',start-constraint-expr)))
                                ,@(when finish-constraint-expr `(:finish-constraint (parse-constraint-spec ',finish-constraint-expr)))
                                ,@(when recurring-expr `(:recurring (parse-recurring-spec ',recurring-expr))))))

       ;; Register task
       (register-task task)

       ;; Add to parent's subtasks if we have a parent
       (when parent-task
         (push task (task-subtasks parent-task)))

       ;; Create dependencies with extended syntax support
       ,@(when depends-on-list
           `((dolist (dep-spec ',depends-on-list)
               (let* ((parsed (parse-dependency-spec dep-spec))
                      (target-ref (first parsed))
                      (dep-type (second parsed))
                      (lag-expr (third parsed))
                      ;; Evaluate lag expression if present
                      (lag (when lag-expr (eval lag-expr)))
                      (dep (make-instance 'dependency
                                         :source task
                                         :target-ref target-ref
                                         :type dep-type
                                         :gap lag)))
                 (push dep (task-dependencies task))))))

       ;; Create allocations with percent support
       ,@(when allocate-list
           `((let* ((parsed (parse-allocations-list ',allocate-list))
                    (resource-refs (first parsed))
                    (resource-percents (second parsed))
                    (alloc (make-instance 'allocation
                                         :task task
                                         :resource-refs resource-refs
                                         :resource-percents resource-percents)))
               (push alloc (task-allocations task)))))

       ;; Execute body with this task as current
       (let ((*current-task* task))
         ,@forms)

       ;; Return the task
       task)))
