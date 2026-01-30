;;;; src/dsl/defbooking.lisp
;;;; Declarative booking macros for time tracking

(in-package #:claps)

;;; ============================================================================
;;; Booking DSL
;;; ============================================================================
;;;
;;; Bookings can be defined:
;;; 1. Inline in deftask with :bookings keyword
;;; 2. Separately with defbooking/defbookings macros
;;;
;;; The separate approach allows time entries to be in different files,
;;; keeping project schedules clean and time tracking data organized.
;;;
;;; Example usage:
;;;
;;;   ;; Single booking
;;;   (defbooking
;;;     :task backend-api
;;;     :resource senior-dev
;;;     :date (date 2024 11 18)
;;;     :hours 8)
;;;
;;;   ;; Multiple bookings (batch)
;;;   (defbookings
;;;     (backend-api senior-dev (date 2024 11 18) 8)
;;;     (backend-api senior-dev (date 2024 11 19) 8)
;;;     (backend-api mid-dev (date 2024 11 18) 8))
;;;
;;;   ;; Timesheet style (grouped by resource)
;;;   (deftimesheet senior-dev
;;;     (backend-api (date 2024 11 18) 8)
;;;     (backend-api (date 2024 11 19) 8)
;;;     (frontend (date 2024 11 20) 4))
;;;
;;; ============================================================================

(defmacro defbooking (&key task resource date hours duration)
  "Define a single booking entry.

   Usage:
     (defbooking
       :task backend-api
       :resource senior-dev
       :date (date 2024 11 18)
       :hours 8)

   Or with duration:
     (defbooking
       :task backend-api
       :resource senior-dev
       :date (date 2024 11 18)
       :duration (duration 8 :hours))

   Keywords:
     :task     - Task ID (symbol)
     :resource - Resource ID (symbol)
     :date     - Date of the work
     :hours    - Hours worked (number, converted to duration)
     :duration - Duration worked (alternative to :hours)"
  (let ((dur-expr (or duration
                      (when hours `(duration ,hours :hours)))))
    `(let ((task-obj (gethash ',task (project-tasks *current-project*)))
           (resource-obj (gethash ',resource (project-resources *current-project*))))
       (if (and task-obj resource-obj)
           (add-booking task-obj resource-obj ,date ,dur-expr)
           (warn "defbooking: Could not find task ~A or resource ~A" ',task ',resource)))))

(defmacro defbookings (&body entries)
  "Define multiple booking entries.

   Usage:
     (defbookings
       (backend-api senior-dev (date 2024 11 18) 8)
       (backend-api senior-dev (date 2024 11 19) 8)
       (backend-api mid-dev (date 2024 11 18) 8)
       (frontend senior-dev (date 2024 11 20) 4))

   Each entry is: (task-id resource-id date hours-or-duration)

   This is convenient for bulk time entry, such as weekly timesheets."
  `(progn
     ,@(mapcar (lambda (entry)
                 (let ((task-id (first entry))
                       (resource-id (second entry))
                       (date-expr (third entry))
                       (amount (fourth entry)))
                   `(let ((task-obj (gethash ',task-id (project-tasks *current-project*)))
                          (resource-obj (gethash ',resource-id (project-resources *current-project*))))
                      (if (and task-obj resource-obj)
                          (add-booking task-obj resource-obj ,date-expr
                                      ,(if (numberp amount)
                                           `(duration ,amount :hours)
                                           amount))
                          (warn "defbookings: Could not find task ~A or resource ~A"
                                ',task-id ',resource-id)))))
               entries)))

(defmacro deftimesheet (resource-id &body entries)
  "Define bookings for a single resource (timesheet style).

   Usage:
     (deftimesheet senior-dev
       (backend-api (date 2024 11 18) 8)
       (backend-api (date 2024 11 19) 8)
       (frontend (date 2024 11 20) 4))

   Each entry is: (task-id date hours-or-duration)

   This is convenient for entering a single resource's timesheet."
  `(let ((resource-obj (gethash ',resource-id (project-resources *current-project*))))
     (if resource-obj
         (progn
           ,@(mapcar (lambda (entry)
                       (let ((task-id (first entry))
                             (date-expr (second entry))
                             (amount (third entry)))
                         `(let ((task-obj (gethash ',task-id (project-tasks *current-project*))))
                            (if task-obj
                                (add-booking task-obj resource-obj ,date-expr
                                            ,(if (numberp amount)
                                                 `(duration ,amount :hours)
                                                 amount))
                                (warn "deftimesheet: Could not find task ~A" ',task-id)))))
                     entries))
         (warn "deftimesheet: Could not find resource ~A" ',resource-id))))

;;; ============================================================================
;;; Auto-completion from Bookings
;;; ============================================================================

(defun sync-completion-from-bookings (task)
  "Update task completion percentage based on booked hours vs planned effort.
   Call this after adding bookings to automatically calculate progress."
  (when (task-effort task)
    (let* ((booked (total-booked-hours task))
           (planned (duration-in-hours (task-effort task)))
           (percent (if (plusp planned)
                       (min 100 (round (* 100 (/ booked planned))))
                       0)))
      (setf (task-complete task) percent)
      percent)))

(defun sync-actual-dates-from-bookings (task)
  "Update task actual-start and actual-end from booking dates.
   Sets actual-start to earliest booking, actual-end to latest."
  (let ((bookings (task-bookings task)))
    (when bookings
      (let ((dates (mapcar #'booking-date bookings)))
        (setf (task-actual-start task)
              (reduce (lambda (a b) (if (date< a b) a b)) dates))
        (setf (task-actual-end task)
              (reduce (lambda (a b) (if (date> a b) a b)) dates))))))
