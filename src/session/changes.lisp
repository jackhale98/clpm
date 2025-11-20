;;;; src/session/changes.lisp
;;;; Change tracking for undo/redo

(in-package #:project-juggler)

;;; ============================================================================
;;; Change Class
;;; ============================================================================

(defclass change ()
  ((type :initarg :type :reader change-type)
   (undo-action :initarg :undo-action :reader change-undo-action)
   (redo-action :initarg :redo-action :reader change-redo-action)
   (description :initarg :description :reader change-description))
  (:documentation "A tracked change that can be undone/redone"))

(defun make-change (type undo-action redo-action &optional description)
  "Create a new change object"
  (make-instance 'change
                 :type type
                 :undo-action undo-action
                 :redo-action redo-action
                 :description description))

;;; ============================================================================
;;; Change Tracking
;;; ============================================================================

(defun track-change (session change)
  "Add a change to the session's change list.

   Clears the redo stack (standard undo/redo behavior)."
  (push change (session-changes session))
  ;; Clear redo stack - once you make a new change after undo, you can't redo
  (setf (session-redo-stack session) nil))

(defun reset-session-changes (session)
  "Clear all changes from session"
  (setf (session-changes session) nil))

;;; ============================================================================
;;; Session Modification Functions
;;; ============================================================================

(defun add-task-to-session (id name &key duration effort start end priority
                                       milestone depends-on allocate)
  "Add a task to the current session and track the change"
  (let* ((session *current-session*)
         (project (session-project session))
         (task (make-instance 'task
                             :id id
                             :name name
                             :project project
                             :duration duration
                             :effort effort
                             :start start
                             :end end
                             :priority (or priority 500)
                             :milestone milestone)))

    ;; Add task to project
    (setf (gethash id (project-tasks project)) task)

    ;; Handle dependencies
    (when depends-on
      (dolist (dep-ref (if (listp depends-on) depends-on (list depends-on)))
        (let* ((target (gethash dep-ref (project-tasks project)))
               (dep (make-instance 'dependency
                                  :source task
                                  :target-ref dep-ref
                                  :target target)))
          (push dep (task-dependencies task)))))

    ;; Handle allocations
    (when allocate
      (let ((resources (mapcar (lambda (r-id)
                                (gethash r-id (project-resources project)))
                              (if (listp allocate) allocate (list allocate)))))
        (when (every #'identity resources)
          (let ((alloc (make-instance 'allocation
                                     :task task
                                     :resource-refs (if (listp allocate)
                                                       allocate
                                                       (list allocate))
                                     :resources resources)))
            (push alloc (task-allocations task))))))

    ;; Track the change
    (let ((change (make-change :add-task
                              ;; Undo: remove task
                              (lambda ()
                                (remhash id (project-tasks project)))
                              ;; Redo: re-add task
                              (lambda ()
                                (setf (gethash id (project-tasks project)) task))
                              (format nil "Add task ~A" id))))
      (track-change session change))

    task))

(defun modify-task-in-session (task &key priority duration effort start end
                                       name milestone)
  "Modify a task in the current session and track the change"
  (let* ((session *current-session*)
         ;; Capture original values
         (orig-priority (task-priority task))
         (orig-duration (task-duration task))
         (orig-effort (task-effort task))
         (orig-start (task-start task))
         (orig-end (task-end task))
         (orig-name (task-name task))
         (orig-milestone (task-milestone-p task)))

    ;; Apply changes
    (when priority
      (setf (task-priority task) priority))
    (when duration
      (setf (task-duration task) duration))
    (when effort
      (setf (task-effort task) effort))
    (when start
      (setf (task-start task) start))
    (when end
      (setf (task-end task) end))
    (when name
      (setf (slot-value task 'name) name))
    (when (not (null milestone))
      (setf (slot-value task 'milestone) milestone))

    ;; Track the change
    (let ((change (make-change :modify-task
                              ;; Undo: restore original values
                              (lambda ()
                                (setf (task-priority task) orig-priority)
                                (setf (task-duration task) orig-duration)
                                (setf (task-effort task) orig-effort)
                                (setf (task-start task) orig-start)
                                (setf (task-end task) orig-end)
                                (setf (slot-value task 'name) orig-name)
                                (setf (slot-value task 'milestone) orig-milestone))
                              ;; Redo: re-apply changes
                              (lambda ()
                                (when priority
                                  (setf (task-priority task) priority))
                                (when duration
                                  (setf (task-duration task) duration))
                                (when effort
                                  (setf (task-effort task) effort))
                                (when start
                                  (setf (task-start task) start))
                                (when end
                                  (setf (task-end task) end))
                                (when name
                                  (setf (slot-value task 'name) name))
                                (when (not (null milestone))
                                  (setf (slot-value task 'milestone) milestone)))
                              (format nil "Modify task ~A" (task-id task)))))
      (track-change session change))

    task))

(defun delete-task-from-session (task)
  "Delete a task from the current session and track the change"
  (let* ((session *current-session*)
         (project (session-project session))
         (id (task-id task)))

    ;; Remove task from project
    (remhash id (project-tasks project))

    ;; Track the change
    (let ((change (make-change :delete-task
                              ;; Undo: re-add task
                              (lambda ()
                                (setf (gethash id (project-tasks project)) task))
                              ;; Redo: remove task
                              (lambda ()
                                (remhash id (project-tasks project)))
                              (format nil "Delete task ~A" id))))
      (track-change session change))

    nil))
