;;;; examples/modular-project/resources/team.lisp
;;;;
;;;; Team resource definitions for the SaaS Platform project.
;;;; This file is included from main.lisp
;;;;
;;;; Keeping resources in a separate file makes it easy to:
;;;;   - Update team members without touching the schedule
;;;;   - Share team definitions across multiple projects
;;;;   - Track team changes in version control

(in-package :claps)

;;; ============================================================================
;;; Development Team
;;; ============================================================================

(defresource backend-dev "Backend Developer (Sarah)"
  :efficiency 1.2
  :rate 150.0)

(defresource frontend-dev "Frontend Developer (Mike)"
  :efficiency 1.0
  :rate 120.0)

(defresource fullstack-dev "Full-Stack Developer (Alex)"
  :efficiency 1.1
  :rate 140.0)

;;; ============================================================================
;;; Design Team
;;; ============================================================================

(defresource ui-designer "UI/UX Designer (Emma)"
  :efficiency 1.0
  :rate 100.0)

;;; ============================================================================
;;; QA Team
;;; ============================================================================

(defresource qa-engineer "QA Engineer (Chris)"
  :efficiency 0.9
  :rate 90.0)
