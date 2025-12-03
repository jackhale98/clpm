;;;; src/constraints/constraints.lisp
;;;; Task constraints and recurring task functionality

(in-package #:project-juggler)

;;; ============================================================================
;;; Constraint Application in Scheduling
;;; ============================================================================

(defun apply-start-constraint (task proposed-start)
  "Apply start constraint to a proposed start date.
   Returns the constrained start date."
  (let ((constraint (task-start-constraint task)))
    (if (null constraint)
        proposed-start
        (let ((constraint-date (constraint-date constraint))
              (ctype (constraint-type constraint)))
          (case ctype
            (:snet ; Start No Earlier Than
             (if (date< proposed-start constraint-date)
                 constraint-date
                 proposed-start))
            (:snlt ; Start No Later Than
             (if (date> proposed-start constraint-date)
                 constraint-date
                 proposed-start))
            (:mso ; Must Start On
             constraint-date)
            (otherwise proposed-start))))))

(defun apply-finish-constraint (task proposed-end duration)
  "Apply finish constraint, adjusting start if needed.
   Returns (values adjusted-start adjusted-end)."
  (let ((constraint (task-finish-constraint task)))
    (if (null constraint)
        (values nil proposed-end)
        (let ((constraint-date (constraint-date constraint))
              (ctype (constraint-type constraint)))
          (case ctype
            (:fnet ; Finish No Earlier Than
             (if (date< proposed-end constraint-date)
                 ;; Adjust start to ensure finish meets constraint
                 (let ((adjusted-start (date- constraint-date duration)))
                   (values adjusted-start constraint-date))
                 (values nil proposed-end)))
            (:fnlt ; Finish No Later Than
             (if (date> proposed-end constraint-date)
                 ;; Adjust start to meet finish constraint
                 (let ((adjusted-start (date- constraint-date duration)))
                   (values adjusted-start constraint-date))
                 (values nil proposed-end)))
            (:mfo ; Must Finish On
             (let ((adjusted-start (date- constraint-date duration)))
               (values adjusted-start constraint-date)))
            (otherwise (values nil proposed-end)))))))

;;; ============================================================================
;;; Constraint Conflict Detection
;;; ============================================================================

(defclass constraint-conflict ()
  ((task-id :initarg :task-id :reader conflict-task-id)
   (description :initarg :description :reader conflict-description)
   (constraint-type :initarg :constraint-type :reader conflict-constraint-type)
   (expected-date :initarg :expected-date :reader conflict-expected-date)
   (actual-date :initarg :actual-date :reader conflict-actual-date))
  (:documentation "A constraint conflict"))

(defun detect-constraint-conflicts (project)
  "Detect all constraint conflicts in the project.
   Returns a list of constraint-conflict objects."
  (let ((conflicts nil))
    (maphash (lambda (id task)
               (declare (ignore id))
               ;; Check start constraint conflicts
               (let ((start-constraint (task-start-constraint task)))
                 (when start-constraint
                   (let ((ctype (constraint-type start-constraint))
                         (cdate (constraint-date start-constraint))
                         (actual-start (task-start task)))
                     (when actual-start
                       (case ctype
                         (:snet
                          (when (date< actual-start cdate)
                            (push (make-instance 'constraint-conflict
                                                 :task-id (task-id task)
                                                 :description "Task starts before SNET date"
                                                 :constraint-type :snet
                                                 :expected-date cdate
                                                 :actual-date actual-start)
                                  conflicts)))
                         (:snlt
                          (when (date> actual-start cdate)
                            (push (make-instance 'constraint-conflict
                                                 :task-id (task-id task)
                                                 :description "Task starts after SNLT date"
                                                 :constraint-type :snlt
                                                 :expected-date cdate
                                                 :actual-date actual-start)
                                  conflicts)))
                         (:mso
                          (unless (date= actual-start cdate)
                            (push (make-instance 'constraint-conflict
                                                 :task-id (task-id task)
                                                 :description "Task does not start on MSO date"
                                                 :constraint-type :mso
                                                 :expected-date cdate
                                                 :actual-date actual-start)
                                  conflicts))))))))
               ;; Check finish constraint conflicts
               (let ((finish-constraint (task-finish-constraint task)))
                 (when finish-constraint
                   (let ((ctype (constraint-type finish-constraint))
                         (cdate (constraint-date finish-constraint))
                         (actual-end (task-end task)))
                     (when actual-end
                       (case ctype
                         (:fnet
                          (when (date< actual-end cdate)
                            (push (make-instance 'constraint-conflict
                                                 :task-id (task-id task)
                                                 :description "Task finishes before FNET date"
                                                 :constraint-type :fnet
                                                 :expected-date cdate
                                                 :actual-date actual-end)
                                  conflicts)))
                         (:fnlt
                          (when (date> actual-end cdate)
                            (push (make-instance 'constraint-conflict
                                                 :task-id (task-id task)
                                                 :description "Task finishes after FNLT date"
                                                 :constraint-type :fnlt
                                                 :expected-date cdate
                                                 :actual-date actual-end)
                                  conflicts)))
                         (:mfo
                          (unless (date= actual-end cdate)
                            (push (make-instance 'constraint-conflict
                                                 :task-id (task-id task)
                                                 :description "Task does not finish on MFO date"
                                                 :constraint-type :mfo
                                                 :expected-date cdate
                                                 :actual-date actual-end)
                                  conflicts))))))))
               ;; Check constraint vs dependency conflicts
               ;; If task has dependencies and constraint starts before dependency ends
               (when (and (task-start task) (task-dependencies task))
                 (dolist (dep (task-dependencies task))
                   (let* ((pred-task (or (dependency-target dep)
                                        (gethash (dependency-target-ref dep) (project-tasks project))))
                          (pred-end (when pred-task (task-end pred-task))))
                     (when (and pred-end (date< (task-start task) pred-end))
                       (push (make-instance 'constraint-conflict
                                           :task-id (task-id task)
                                           :description "Task start violates dependency (starts before predecessor ends)"
                                           :constraint-type :dependency
                                           :expected-date pred-end
                                           :actual-date (task-start task))
                             conflicts))))))
             (project-tasks project))
    conflicts))

;;; ============================================================================
;;; Recurring Task Expansion
;;; ============================================================================

(defun day-of-week-number (day)
  "Convert day keyword to number (0=Sunday, 1=Monday, etc.)."
  (case day
    (:sunday 0)
    (:monday 1)
    (:tuesday 2)
    (:wednesday 3)
    (:thursday 4)
    (:friday 5)
    (:saturday 6)
    (otherwise 0)))

(defun date-day-number (d)
  "Get day of week as number (0=Sunday) for a date."
  (let ((ts (date-timestamp d)))
    (local-time:timestamp-day-of-week ts)))

(defun workday-p (d)
  "Check if date is a workday (Monday-Friday)."
  (let ((dow (date-day-number d)))
    (and (>= dow 1) (<= dow 5))))

(defun date-match-day-p (d day)
  "Check if date matches the given day of week."
  (= (date-day-number d) (day-of-week-number day)))

(defun date-match-month-day-p (d day-number)
  "Check if date is the given day of month."
  (= (date-day d) day-number))

(defun exception-date-p (d exceptions)
  "Check if date is in the exception list."
  (some (lambda (ex) (date= d ex)) exceptions))

(defun expand-recurring-tasks (project)
  "Expand all recurring tasks in the project.
   Returns a plist of task-id -> list of instance dates."
  (let ((result nil))
    (maphash (lambda (id task)
               (let ((recur (task-recurring task)))
                 (when recur
                   (setf (getf result id)
                         (expand-recurring-definition recur)))))
             (project-tasks project))
    result))

(defun expand-recurring-definition (recur)
  "Expand a recurring definition into a list of dates."
  (let ((frequency (recurring-frequency recur))
        (start-date (recurring-start-date recur))
        (end-date (recurring-end-date recur))
        (exceptions (recurring-exceptions recur))
        (instances nil))
    (case frequency
      (:daily
       (loop for d = start-date then (date+ d (duration 1 :days))
             while (date<= d end-date)
             unless (exception-date-p d exceptions)
               do (push d instances)))
      (:workdays
       (loop for d = start-date then (date+ d (duration 1 :days))
             while (date<= d end-date)
             when (and (workday-p d)
                       (not (exception-date-p d exceptions)))
               do (push d instances)))
      (:weekly
       (let ((target-day (recurring-day recur)))
         (loop for d = start-date then (date+ d (duration 1 :days))
               while (date<= d end-date)
               when (and (date-match-day-p d target-day)
                         (not (exception-date-p d exceptions)))
                 do (push d instances))))
      (:monthly
       (let ((target-day (recurring-day recur)))
         (loop for d = start-date then (date+ d (duration 1 :days))
               while (date<= d end-date)
               when (and (date-match-month-day-p d target-day)
                         (not (exception-date-p d exceptions)))
                 do (push d instances))))
      (:days
       (let ((target-days (recurring-days recur)))
         (loop for d = start-date then (date+ d (duration 1 :days))
               while (date<= d end-date)
               when (and (some (lambda (day) (date-match-day-p d day)) target-days)
                         (not (exception-date-p d exceptions)))
                 do (push d instances)))))
    (nreverse instances)))

(defun get-recurring-tasks (project)
  "Get all tasks that have recurring definitions."
  (let ((result nil))
    (maphash (lambda (id task)
               (declare (ignore id))
               (when (task-recurring task)
                 (push task result)))
             (project-tasks project))
    result))

