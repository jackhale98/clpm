;;;; src/session/session.lisp
;;;; Session management main file

(in-package #:project-juggler)

;;; The session class is defined in core/classes.lisp
;;; Session functionality is implemented in:
;;; - persistence.lisp: load-project-session, save-session
;;; - changes.lisp: change tracking, add/modify/delete-task-to-session
;;; - undo-redo.lisp: undo, redo
