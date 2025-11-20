;;;; src/dsl/deftask.lisp
;;;; deftask macro implementation

(in-package #:project-juggler)

(defmacro deftask (id name &body body)
  "Define a task.

   Usage:
     (deftask task1 \"Task 1\"
       :effort (duration 40 :hours)
       :priority 700
       :depends-on (task0)
       :allocate (dev1 dev2))

   Keywords:
     :effort      - Effort duration
     :duration    - Calendar duration
     :start       - Fixed start date
     :end         - Fixed end date
     :priority    - Priority (default 500)
     :milestone   - Boolean, is this a milestone?
     :depends-on  - List of task IDs this task depends on
     :allocate    - List of resource IDs to allocate

   Body can contain subtask definitions."

  ;; Parse keyword arguments and body forms
  ;; Walk through body, consuming keyword-value pairs until we hit a non-keyword
  (let ((effort-expr nil)
        (duration-expr nil)
        (start-expr nil)
        (end-expr nil)
        (priority-expr nil)
        (milestone-expr nil)
        (depends-on-list nil)
        (allocate-list nil)
        (forms nil)
        (remaining body))

    (loop while (and remaining (keywordp (first remaining)))
          do (let ((keyword (first remaining))
                   (value (second remaining)))
               (case keyword
                 (:effort (setf effort-expr value))
                 (:duration (setf duration-expr value))
                 (:start (setf start-expr value))
                 (:end (setf end-expr value))
                 (:priority (setf priority-expr value))
                 (:milestone (setf milestone-expr value))
                 (:depends-on (setf depends-on-list value))
                 (:allocate (setf allocate-list value))
                 (t (warn "Unknown keyword in deftask: ~A" keyword)))
               (setf remaining (cddr remaining))))

    ;; Remaining elements are body forms
    (setf forms remaining)

    `(let* ((parent-task *current-task*)
            (task (make-instance 'task
                                :id ',id
                                :name ,name
                                :project *current-project*
                                :parent parent-task
                                ,@(when effort-expr `(:effort ,effort-expr))
                                ,@(when duration-expr `(:duration ,duration-expr))
                                ,@(when start-expr `(:start ,start-expr))
                                ,@(when end-expr `(:end ,end-expr))
                                ,@(when priority-expr `(:priority ,priority-expr))
                                ,@(when milestone-expr `(:milestone ,milestone-expr)))))

       ;; Register task
       (register-task task)

       ;; Add to parent's subtasks if we have a parent
       (when parent-task
         (push task (task-subtasks parent-task)))

       ;; Create dependencies
       ,@(when depends-on-list
           `((dolist (dep-ref ',depends-on-list)
               (let ((dep (make-instance 'dependency
                                        :source task
                                        :target-ref dep-ref)))
                 (push dep (task-dependencies task))))))

       ;; Create allocations
       ,@(when allocate-list
           `((let ((alloc (make-instance 'allocation
                                        :task task
                                        :resource-refs ',allocate-list)))
               (push alloc (task-allocations task)))))

       ;; Execute body with this task as current
       (let ((*current-task* task))
         ,@forms)

       ;; Return the task
       task)))
