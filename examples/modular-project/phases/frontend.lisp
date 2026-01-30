;;;; examples/modular-project/phases/frontend.lisp
;;;;
;;;; Frontend development phase tasks.
;;;; This file is included with :namespace frontend, so all task IDs
;;;; will be prefixed: ui -> frontend/ui, components -> frontend/components, etc.

(in-package :claps)

;;; ============================================================================
;;; Frontend Phase Tasks
;;; ============================================================================

;; Note: These tasks will get prefixed IDs when included with :namespace frontend

;; Design comes first (no dependencies)
(deftask design "UI/UX Design"
  :effort (duration 40 :hours)
  :allocate (ui-designer)
  :priority 900)

;; Components depends on design
;; Note: Within a namespace, dependencies need the full prefixed name
(deftask components "React Component Library"
  :effort (duration 50 :hours)
  :depends-on (frontend/design)
  :allocate (frontend-dev)
  :priority 800)

;; UI depends on components
(deftask ui "Main Application UI"
  :effort (duration 60 :hours)
  :depends-on (frontend/components)
  :allocate (frontend-dev)
  :priority 750)

;; Styling depends on design
(deftask styling "Styling & Theming"
  :effort (duration 20 :hours)
  :depends-on (frontend/design)
  :allocate (ui-designer)
  :priority 700)

;; Testing depends on ui and styling
(deftask testing "Frontend Testing"
  :effort (duration 25 :hours)
  :depends-on (frontend/ui frontend/styling)
  :allocate (qa-engineer)
  :priority 700)
