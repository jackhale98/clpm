;;;; examples/modular-project/timesheets/november.lisp
;;;;
;;;; Time tracking entries for November 2024.
;;;; This file is kept separate from the schedule to avoid bloat.
;;;;
;;;; Benefits of separate timesheet files:
;;;;   - Schedule file stays clean and readable
;;;;   - Time entries can be added without modifying schedule
;;;;   - Easy to organize by month/week/sprint
;;;;   - Different people can update timesheets vs schedule

(in-package :claps)

;;; ============================================================================
;;; Week 1: Nov 4-8, 2024
;;; ============================================================================

;; Note: Task IDs use namespace prefix (backend/database, frontend/design, etc.)

(defbookings
  ;; Backend work
  (backend/database backend-dev (date 2024 11 4) 8)
  (backend/database backend-dev (date 2024 11 5) 8)
  (backend/database backend-dev (date 2024 11 6) 6)

  ;; Frontend design
  (frontend/design ui-designer (date 2024 11 4) 8)
  (frontend/design ui-designer (date 2024 11 5) 8)
  (frontend/design ui-designer (date 2024 11 6) 8)
  (frontend/design ui-designer (date 2024 11 7) 4))

;;; ============================================================================
;;; Week 2: Nov 11-15, 2024
;;; ============================================================================

;; Using timesheet style (grouped by resource)
(deftimesheet backend-dev
  (backend/api (date 2024 11 11) 8)
  (backend/api (date 2024 11 12) 8)
  (backend/api (date 2024 11 13) 6)
  (backend/auth (date 2024 11 14) 8)
  (backend/auth (date 2024 11 15) 8))

(deftimesheet frontend-dev
  (frontend/components (date 2024 11 11) 8)
  (frontend/components (date 2024 11 12) 8)
  (frontend/components (date 2024 11 13) 8)
  (frontend/components (date 2024 11 14) 6)
  (frontend/components (date 2024 11 15) 8))
