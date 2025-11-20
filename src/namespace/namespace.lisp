;;;; src/namespace/namespace.lisp
;;;; Namespace system implementation

(in-package #:project-juggler)

;;; Namespace class is already defined in src/core/classes.lisp

;;; Utility functions for qualified symbol parsing

(defun parse-qualified-symbol (symbol)
  "Parse a possibly-qualified symbol into (namespace-part name-part).
   Returns (nil name) for unqualified symbols.

   Examples:
     'task1        => (nil task1)
     'infra:servers => (infra servers)
     :infra:servers => (:infra :servers)"
  (let* ((symbol-string (symbol-name symbol))
         (package-marker-pos (position #\: symbol-string)))
    (if package-marker-pos
        ;; Qualified symbol - has namespace prefix
        (let* ((ns-string (subseq symbol-string 0 package-marker-pos))
               (name-string (subseq symbol-string (1+ package-marker-pos)))
               ;; Determine if we're dealing with keywords
               (use-keywords (keywordp symbol))
               (ns-symbol (if use-keywords
                              (intern ns-string :keyword)
                              (intern ns-string)))
               (name-symbol (if use-keywords
                                (intern name-string :keyword)
                                (intern name-string))))
          (list ns-symbol name-symbol))
        ;; Unqualified symbol - no namespace prefix
        (list nil symbol))))

;;; Task registration

(defmethod register-task ((task task) &optional (namespace *current-namespace*))
  "Register task in namespace and project"
  (let ((id (task-id task)))
    ;; Check for duplicates in namespace
    (when (gethash id (namespace-tasks namespace))
      (cerror "Replace existing task"
              "Task ~A already exists in namespace ~A"
              id (namespace-name namespace)))

    ;; Register in namespace
    (setf (gethash id (namespace-tasks namespace)) task)

    ;; Register in project with qualified name if in named namespace
    (let ((qualified-id (if (namespace-name namespace)
                            ;; Create qualified symbol: namespace:id
                            (intern (format nil "~A:~A"
                                          (namespace-name namespace)
                                          id))
                            id)))
      (when *current-project*
        (setf (gethash qualified-id (project-tasks *current-project*)) task)))

    task))

;;; Resource registration

(defmethod register-resource ((resource resource) &optional (namespace *current-namespace*))
  "Register resource in namespace and project"
  (let ((id (resource-id resource)))
    ;; Check for duplicates in namespace
    (when (gethash id (namespace-resources namespace))
      (cerror "Replace existing resource"
              "Resource ~A already exists in namespace ~A"
              id (namespace-name namespace)))

    ;; Register in namespace
    (setf (gethash id (namespace-resources namespace)) resource)

    ;; Register in project with qualified name if in named namespace
    (let ((qualified-id (if (namespace-name namespace)
                            ;; Create qualified symbol: namespace:id
                            (intern (format nil "~A:~A"
                                          (namespace-name namespace)
                                          id))
                            id)))
      (when *current-project*
        (setf (gethash qualified-id (project-resources *current-project*)) resource)))

    resource))

;;; In-namespace macro

(defmacro in-namespace (name)
  "Set current namespace for subsequent definitions.
   Creates namespace if it doesn't exist."
  `(setf *current-namespace*
         (or (gethash ',name *namespace-registry*)
             (setf (gethash ',name *namespace-registry*)
                   (make-instance 'namespace
                                  :name ',name
                                  :source-file *load-pathname*)))))

;;; Namespace lookup helpers

(defun find-namespace (name)
  "Find namespace by name"
  (gethash name *namespace-registry*))

(defun ensure-namespace (name)
  "Ensure namespace exists, creating if necessary"
  (or (find-namespace name)
      (setf (gethash name *namespace-registry*)
            (make-instance 'namespace :name name))))
