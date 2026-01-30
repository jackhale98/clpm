;;;; examples/declarative-demo/timesheets.lisp
;;;;
;;;; Timesheet entries - can be loaded separately from the project file.
;;;; This keeps the project schedule clean while tracking actual work.
;;;;
;;;; Load this file AFTER the project file has defined tasks and resources.

(in-package :claps)

;;;; =========================================================================
;;;; Week of November 11-15, 2024
;;;; =========================================================================

;; Batch entry style - good for importing from external tools
(defbookings
  ;; Monday Nov 11
  (backend alice (date 2024 11 11) 8)
  (backend bob (date 2024 11 11) 6)
  (frontend bob (date 2024 11 11) 2)

  ;; Tuesday Nov 12
  (backend alice (date 2024 11 12) 8)
  (backend bob (date 2024 11 12) 4)
  (frontend bob (date 2024 11 12) 4))

;;;; =========================================================================
;;;; Week of November 18-22, 2024
;;;; =========================================================================

;; Timesheet style - grouped by resource
(deftimesheet alice
  (backend (date 2024 11 18) 8)
  (backend (date 2024 11 19) 8)
  (backend (date 2024 11 20) 6))

(deftimesheet bob
  (backend (date 2024 11 18) 4)
  (frontend (date 2024 11 18) 4)
  (frontend (date 2024 11 19) 8)
  (frontend (date 2024 11 20) 6))

;;;; =========================================================================
;;;; Individual booking style - good for corrections
;;;; =========================================================================

;; (defbooking
;;   :task frontend
;;   :resource bob
;;   :date (date 2024 11 21)
;;   :hours 8)

;;;; =========================================================================
;;;; Sync completion percentages from bookings
;;;; =========================================================================

;; After loading bookings, sync task completion from actual hours
(maphash (lambda (id task)
           (declare (ignore id))
           (when (task-bookings task)
             (sync-completion-from-bookings task)
             (sync-actual-dates-from-bookings task)))
         (project-tasks *current-project*))
