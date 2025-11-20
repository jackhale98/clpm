;;;; src/namespace/references.lisp
;;;; Reference resolution implementation

(in-package #:project-juggler)

;;; Task reference resolution

(defmethod resolve-task-reference ((reference symbol) &optional (project *current-project*))
  "Resolve a task reference to actual task object.
   Supports both simple (task-id) and qualified (namespace:task-id) references.

   Resolution strategy:
   1. If qualified (has :), look in specified namespace
   2. If simple, look in current namespace first
   3. Fall back to project-level lookup if needed"

  (destructuring-bind (ns-part name-part) (parse-qualified-symbol reference)
    (cond
      ;; Qualified reference: namespace:task-id
      (ns-part
       (let ((target-ns (find-namespace ns-part)))
         (unless target-ns
           (error 'reference-error
                  :message (format nil "Namespace ~A not found" ns-part)
                  :reference reference))
         (or (gethash name-part (namespace-tasks target-ns))
             (error 'reference-error
                    :message (format nil "Task ~A not found in namespace ~A"
                                   name-part ns-part)
                    :reference reference))))

      ;; Simple reference: task-id
      ;; Try current namespace first
      (t
       (or (when *current-namespace*
             (gethash reference (namespace-tasks *current-namespace*)))
           ;; Fall back to project lookup
           (when project
             (gethash reference (project-tasks project)))
           (error 'reference-error
                  :message (format nil "Task ~A not found" reference)
                  :reference reference))))))

;;; Resource reference resolution

(defmethod resolve-resource-reference ((reference symbol) &optional (project *current-project*))
  "Resolve a resource reference to actual resource object.
   Supports both simple (resource-id) and qualified (namespace:resource-id) references.

   Resolution strategy:
   1. If qualified (has :), look in specified namespace
   2. If simple, look in current namespace first
   3. Fall back to project-level lookup if needed"

  (destructuring-bind (ns-part name-part) (parse-qualified-symbol reference)
    (cond
      ;; Qualified reference: namespace:resource-id
      (ns-part
       (let ((target-ns (find-namespace ns-part)))
         (unless target-ns
           (error 'reference-error
                  :message (format nil "Namespace ~A not found" ns-part)
                  :reference reference))
         (or (gethash name-part (namespace-resources target-ns))
             (error 'reference-error
                    :message (format nil "Resource ~A not found in namespace ~A"
                                   name-part ns-part)
                    :reference reference))))

      ;; Simple reference: resource-id
      ;; Try current namespace first
      (t
       (or (when *current-namespace*
             (gethash reference (namespace-resources *current-namespace*)))
           ;; Fall back to project lookup
           (when project
             (gethash reference (project-resources project)))
           (error 'reference-error
                  :message (format nil "Resource ~A not found" reference)
                  :reference reference))))))

;;; Bulk reference resolution helpers

(defun resolve-task-references (references &optional (project *current-project*))
  "Resolve a list of task references"
  (mapcar (lambda (ref) (resolve-task-reference ref project)) references))

(defun resolve-resource-references (references &optional (project *current-project*))
  "Resolve a list of resource references"
  (mapcar (lambda (ref) (resolve-resource-reference ref project)) references))
