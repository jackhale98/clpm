;;;; src/session/undo-redo.lisp
;;;; Undo/Redo functionality

(in-package #:project-juggler)

;;; ============================================================================
;;; Undo
;;; ============================================================================

(defun undo ()
  "Undo the last change in the current session.

   Returns T if undo was successful, NIL if there are no changes to undo."

  (let ((session *current-session*))
    (unless session
      (error "No current session"))

    (let ((changes (session-changes session)))
      (if (null changes)
          nil  ; No changes to undo
          (let ((last-change (first changes)))
            ;; Remove from changes list
            (setf (session-changes session) (rest changes))

            ;; Add to redo stack
            (push last-change (session-redo-stack session))

            ;; Execute undo action
            (funcall (change-undo-action last-change))

            t)))))

;;; ============================================================================
;;; Redo
;;; ============================================================================

(defun redo ()
  "Redo the last undone change in the current session.

   Returns T if redo was successful, NIL if there are no changes to redo."

  (let ((session *current-session*))
    (unless session
      (error "No current session"))

    (let ((redo-stack (session-redo-stack session)))
      (if (null redo-stack)
          nil  ; No changes to redo
          (let ((last-undone (first redo-stack)))
            ;; Remove from redo stack
            (setf (session-redo-stack session) (rest redo-stack))

            ;; Add back to changes list
            (push last-undone (session-changes session))

            ;; Execute redo action
            (funcall (change-redo-action last-undone))

            t)))))
