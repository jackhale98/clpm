;;;; src/dsl/defproject.lisp
;;;; defproject macro implementation

(in-package #:project-juggler)

(defmacro defproject (id name &body body)
  "Define a project.

   Usage:
     (defproject my-project \"My Project\"
       :start (date 2024 1 1)
       :end (date 2024 12 31)
       (deftask task1 \"Task 1\")
       (deftask task2 \"Task 2\"))

   Keywords:
     :start     - Project start date (required)
     :end       - Project end date (required)

   The body can contain task and resource definitions."

  ;; Parse keyword arguments and body forms
  ;; Walk through body, consuming keyword-value pairs until we hit a non-keyword
  (let ((start-expr nil)
        (end-expr nil)
        (forms nil)
        (remaining body))

    (loop while (and remaining (keywordp (first remaining)))
          do (let ((keyword (first remaining))
                   (value (second remaining)))
               (case keyword
                 (:start (setf start-expr value))
                 (:end (setf end-expr value))
                 (t (warn "Unknown keyword in defproject: ~A" keyword)))
               (setf remaining (cddr remaining))))

    ;; Remaining elements are body forms
    (setf forms remaining)

    (unless start-expr
      (error "defproject requires :start date"))
    (unless end-expr
      (error "defproject requires :end date"))

    `(progn
       ;; Create and register project
       (let ((project (make-instance 'project
                                    :id ',id
                                    :name ,name
                                    :start ,start-expr
                                    :end ,end-expr)))
         ;; Register in global project registry
         (setf (gethash ',id *project-registry*) project)

         ;; Set as current project
         (setf *current-project* project)

         ;; Create and set default namespace
         (setf *current-namespace*
               (or (gethash nil *namespace-registry*)
                   (setf (gethash nil *namespace-registry*)
                         (make-instance 'namespace :name nil))))

         ;; Execute body forms
         ,@forms

         ;; Return the project
         project))))
