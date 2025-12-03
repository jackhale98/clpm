;;;; src/core/classes.lisp
;;;; Core CLOS classes (to be implemented via TDD)

(in-package #:project-juggler)

;;; Stub class definitions - will be implemented via TDD in Phase 1

(defclass project ()
  ((id :initarg :id :reader project-id)
   (name :initarg :name :reader project-name)
   (start :initarg :start :reader project-start)
   (end :initarg :end :reader project-end)
   (tasks :initform (make-hash-table :test 'eq) :accessor project-tasks)
   (resources :initform (make-hash-table :test 'eq) :accessor project-resources)
   (scenarios :initform nil :accessor project-scenarios)
   (current-scenario :initform :plan :accessor project-current-scenario)
   (baseline :initform nil :accessor project-baseline)
   (reports :initform (make-hash-table :test 'eq) :accessor project-reports)
   ;; Cost tracking
   (budget :initarg :budget :initform nil :accessor project-budget)
   ;; Risk register
   (risk-register :initform nil :accessor project-risk-register)
   ;; Primary baseline name (TaskJuggler-style)
   (primary-baseline :initform nil :accessor project-primary-baseline))
  (:documentation "A project"))

(defun project-p (obj)
  "Check if object is a project"
  (typep obj 'project))

(defclass task ()
  ((id :initarg :id :reader task-id)
   (name :initarg :name :reader task-name)
   (project :initarg :project :reader task-project)
   (parent :initarg :parent :initform nil :accessor task-parent)
   (subtasks :initform nil :accessor task-subtasks)
   (dependencies :initform nil :accessor task-dependencies)
   (allocations :initform nil :accessor task-allocations)
   ;; Base values (used for default/plan scenario)
   (effort :initarg :effort :initform nil :accessor task-effort)
   (duration :initarg :duration :initform nil :accessor task-duration)
   (start :initarg :start :initform nil :accessor task-start)
   (end :initarg :end :initform nil :accessor task-end)
   (priority :initarg :priority :initform 500 :accessor task-priority)
   (milestone :initarg :milestone :initform nil :accessor task-milestone-p)
   (complete :initarg :complete :initform 0 :accessor task-complete)
   (index :initarg :index :initform 0 :accessor task-index)
   (scheduled :initform nil :accessor task-scheduled-p)
   ;; Scenario-specific values: scenario-id -> plist of (:effort :duration :start :end :complete)
   ;; TaskJuggler-style: task stores different values per scenario
   (scenario-values :initform (make-hash-table :test 'eq) :accessor task-scenario-values)
   ;; TaskJuggler heuristic criticalness (for scheduling priority)
   (criticalness :initform 0.0 :accessor task-criticalness)
   (path-criticalness :initform 0.0 :accessor task-path-criticalness)
   ;; CPM critical path slots (calculated after scheduling)
   (early-start :initform nil :accessor task-early-start)
   (early-finish :initform nil :accessor task-early-finish)
   (late-start :initform nil :accessor task-late-start)
   (late-finish :initform nil :accessor task-late-finish)
   (slack :initform nil :accessor task-slack)
   ;; Bookings (actual time tracking)
   (bookings :initform nil :accessor task-bookings)
   ;; Cost tracking
   (fixed-cost :initarg :fixed-cost :initform 0.0 :accessor task-fixed-cost)
   ;; PERT estimation
   (estimate :initarg :estimate :initform nil :accessor task-estimate)
   ;; Constraints
   (start-constraint :initarg :start-constraint :initform nil :accessor task-start-constraint)
   (finish-constraint :initarg :finish-constraint :initform nil :accessor task-finish-constraint)
   ;; Recurring task definition
   (recurring :initarg :recurring :initform nil :accessor task-recurring))
  (:documentation "A task"))

(defun task-p (obj)
  "Check if object is a task"
  (typep obj 'task))

;;; TaskJuggler-style scenario value accessors
;;; These get/set values for a specific scenario, falling back to base values

(defun task-value-for-scenario (task property scenario-id)
  "Get a task property value for a specific scenario.
   Falls back to base value if scenario doesn't override it."
  (let ((scenario-plist (gethash scenario-id (task-scenario-values task))))
    (if scenario-plist
        (let ((val (getf scenario-plist property :not-found)))
          (if (eq val :not-found)
              ;; Fall back to base value
              (ecase property
                (:effort (task-effort task))
                (:duration (task-duration task))
                (:start (task-start task))
                (:end (task-end task))
                (:complete (task-complete task)))
              val))
        ;; No scenario override, use base value
        (ecase property
          (:effort (task-effort task))
          (:duration (task-duration task))
          (:start (task-start task))
          (:end (task-end task))
          (:complete (task-complete task))))))

(defun (setf task-value-for-scenario) (value task property scenario-id)
  "Set a task property value for a specific scenario."
  (let ((scenario-plist (gethash scenario-id (task-scenario-values task))))
    (if scenario-plist
        (setf (getf (gethash scenario-id (task-scenario-values task)) property) value)
        (setf (gethash scenario-id (task-scenario-values task))
              (list property value))))
  value)

(defun task-effort-for-scenario (task scenario-id)
  "Get task effort for a specific scenario."
  (task-value-for-scenario task :effort scenario-id))

(defun task-duration-for-scenario (task scenario-id)
  "Get task duration for a specific scenario."
  (task-value-for-scenario task :duration scenario-id))

(defun task-start-for-scenario (task scenario-id)
  "Get task start for a specific scenario."
  (task-value-for-scenario task :start scenario-id))

(defun task-end-for-scenario (task scenario-id)
  "Get task end for a specific scenario."
  (task-value-for-scenario task :end scenario-id))

(defun task-complete-for-scenario (task scenario-id)
  "Get task completion for a specific scenario."
  (task-value-for-scenario task :complete scenario-id))

(defclass resource ()
  ((id :initarg :id :reader resource-id)
   (name :initarg :name :reader resource-name)
   (project :initarg :project :reader resource-project)
   (parent :initarg :parent :initform nil :accessor resource-parent)
   (efficiency :initarg :efficiency :initform 1.0 :accessor resource-efficiency)
   (rate :initarg :rate :initform nil :accessor resource-rate)
   (limits :initform nil :accessor resource-limits)
   (criticalness :initform 0.0 :accessor resource-criticalness)
   (allocated-effort :initform 0.0 :accessor resource-allocated-effort)
   (available-effort :initform 0.0 :accessor resource-available-effort)
   ;; Bookings (actual time tracking)
   (bookings :initform nil :accessor resource-bookings)
   ;; Availability
   (leaves :initarg :leaves :initform nil :accessor resource-leaves)
   (daily-limit :initarg :daily-limit :initform nil :accessor resource-daily-limit)
   (weekly-limit :initarg :weekly-limit :initform nil :accessor resource-weekly-limit)
   (resource-calendar :initarg :calendar :initform nil :accessor resource-calendar))
  (:documentation "A resource"))

(defun resource-p (obj)
  "Check if object is a resource"
  (typep obj 'resource))

(defclass dependency ()
  ((source :initarg :source :reader dependency-source)
   (target-ref :initarg :target-ref :reader dependency-target-ref)
   (target :initarg :target :initform nil :accessor dependency-target)
   (type :initarg :type :initform :finish-start :reader dependency-type)
   (gap-duration :initarg :gap :initform nil :reader dependency-gap))
  (:documentation "A dependency between tasks"))

(defun dependency-p (obj)
  "Check if object is a dependency"
  (typep obj 'dependency))

(defclass allocation ()
  ((task :initarg :task :reader allocation-task)
   (resource-refs :initarg :resource-refs :reader allocation-resource-refs)
   (resources :initarg :resources :initform nil :accessor allocation-resources)
   (mandatory :initarg :mandatory :initform nil :reader allocation-mandatory-p)
   ;; Resource percentages: alist of (resource-ref . percent)
   (resource-percents :initarg :resource-percents :initform nil :accessor allocation-resource-percents)
   ;; Default percent for simple allocations
   (percent :initarg :percent :initform 100 :accessor allocation-percent))
  (:documentation "Resource allocation to task"))

(defclass leave ()
  ((type :initarg :type :reader leave-type)
   (start :initarg :start :reader leave-start)
   (end :initarg :end :reader leave-end)
   (description :initarg :description :initform nil :reader leave-description))
  (:documentation "Resource leave/unavailability period"))

(defun leave-p (obj)
  "Check if object is a leave"
  (typep obj 'leave))

(defclass leave-conflict ()
  ((resource :initarg :resource :reader conflict-resource)
   (leave :initarg :leave :reader conflict-leave)
   (task :initarg :task :reader conflict-task)
   (overlap-start :initarg :overlap-start :reader conflict-overlap-start)
   (overlap-end :initarg :overlap-end :reader conflict-overlap-end))
  (:documentation "Conflict between task schedule and resource leave"))

(defclass pert-estimate ()
  ((optimistic :initarg :optimistic :reader estimate-optimistic)
   (likely :initarg :likely :accessor estimate-likely)
   (pessimistic :initarg :pessimistic :reader estimate-pessimistic))
  (:documentation "PERT three-point estimate for task duration"))

(defun pert-estimate-p (obj)
  "Check if object is a PERT estimate"
  (typep obj 'pert-estimate))

(defun allocation-p (obj)
  "Check if object is an allocation"
  (typep obj 'allocation))

(defclass namespace ()
  ((name :initarg :name :reader namespace-name)
   (tasks :initform (make-hash-table :test 'eq) :accessor namespace-tasks)
   (resources :initform (make-hash-table :test 'eq) :accessor namespace-resources)
   (source-file :initarg :source-file :initform nil :reader namespace-source-file))
  (:documentation "Namespace for tasks and resources"))

(defclass session ()
  ((project :initarg :project :accessor session-project)
   (current-task :initform nil :accessor session-current-task)
   (changes :initform nil :accessor session-changes)
   (redo-stack :initform nil :accessor session-redo-stack))
  (:documentation "Project session"))

(defclass scenario ()
  ((id :initarg :id :reader scenario-id)
   (name :initarg :name :reader scenario-name)
   (parent :initarg :parent :initform nil :accessor scenario-parent)
   (enabled :initarg :enabled :initform t :accessor scenario-enabled-p))
  (:documentation "Project scenario"))

(defclass booking ()
  ((resource :initarg :resource :reader booking-resource)
   (task :initarg :task :reader booking-task)
   (start :initarg :start :reader booking-start)
   (end :initarg :end :reader booking-end)
   (amount :initarg :amount :reader booking-amount))
  (:documentation "Actual work booking"))

;; Note: working-hours class is defined in scheduling/calendars.lisp

(defclass calendar ()
  ((id :initarg :id :reader calendar-id)
   (working-hours :initarg :working-hours :reader calendar-working-hours)
   (holidays :initform nil :accessor calendar-holidays)
   (timezone :initarg :timezone :initform :utc :reader calendar-timezone))
  (:documentation "Calendar with working time and holidays"))

(defclass report ()
  ((id :initarg :id :reader report-id)
   (title :initarg :title :reader report-title)
   (format :initarg :format :reader report-format)
   (columns :initarg :columns :reader report-columns)
   (filter :initarg :filter :initform nil :reader report-filter)
   (sort-by :initarg :sort-by :initform nil :reader report-sort-by))
  (:documentation "Base report class"))

(defclass task-report (report)
  ()
  (:documentation "Task-focused report"))

(defclass resource-report (report)
  ()
  (:documentation "Resource-focused report"))

(defclass baseline ()
  ((name :initarg :name :reader baseline-name)
   (date :initarg :date :reader baseline-date)
   (tasks :initform (make-hash-table :test 'eq) :accessor baseline-tasks))
  (:documentation "Project baseline for EVM"))

(defclass baseline-task ()
  ((id :initarg :id :reader baseline-task-id)
   (name :initarg :name :reader baseline-task-name)
   (start :initarg :start :reader baseline-task-start)
   (end :initarg :end :reader baseline-task-end)
   (duration :initarg :duration :reader baseline-task-duration)
   (priority :initarg :priority :reader baseline-task-priority))
  (:documentation "Snapshot of task state in baseline"))

(defclass overallocation ()
  ((resource-id :initarg :resource-id :reader overallocation-resource-id)
   (date :initarg :date :reader overallocation-date)
   (load :initarg :load :reader overallocation-load))
  (:documentation "Resource over-allocation information"))

;;; ============================================================================
;;; Task Constraint Classes
;;; ============================================================================

(defclass task-constraint ()
  ((type :initarg :type :reader constraint-type
         :documentation "Constraint type: :snet, :snlt, :fnet, :fnlt, :mso, :mfo")
   (date :initarg :date :reader constraint-date
         :documentation "The date for the constraint"))
  (:documentation "A date constraint on a task"))

(defun task-constraint-p (obj)
  "Check if object is a task constraint."
  (typep obj 'task-constraint))

(defun make-task-constraint (type date)
  "Create a task constraint."
  (make-instance 'task-constraint :type type :date date))

;;; ============================================================================
;;; Recurring Task Classes
;;; ============================================================================

(defclass recurring-definition ()
  ((frequency :initarg :frequency :reader recurring-frequency
              :documentation "Frequency: :daily, :weekly, :workdays, :monthly, :days")
   (start-date :initarg :start-date :reader recurring-start-date
               :documentation "Start date for recurrence")
   (end-date :initarg :end-date :reader recurring-end-date
             :documentation "End date for recurrence")
   (day :initarg :day :initform nil :reader recurring-day
        :documentation "Day of week (for :weekly) or day of month (for :monthly)")
   (days :initarg :days :initform nil :reader recurring-days
         :documentation "List of days (for :days frequency)")
   (exceptions :initarg :exceptions :initform nil :accessor recurring-exceptions
               :documentation "List of exception dates to skip"))
  (:documentation "Definition of a recurring task pattern"))

(defun recurring-definition-p (obj)
  "Check if object is a recurring definition."
  (typep obj 'recurring-definition))
