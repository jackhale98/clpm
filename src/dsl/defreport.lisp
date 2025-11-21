;;;; src/dsl/defreport.lisp
;;;; defreport macro implementation

(in-package #:project-juggler)

(defmacro defreport (id title &body body)
  "Define a report within a project.

   Usage:
     (defreport summary \"Project Summary\"
       :type :task
       :format :html
       :columns (:id :name :start :end :duration)
       :filter (lambda (task) (> (task-priority task) 500))
       :sort-by (lambda (a b) (date< (task-start a) (task-start b))))

     (defreport resource-list \"Resource Allocation\"
       :type :resource
       :format :csv
       :columns (:id :name :efficiency :rate :criticalness))

   Keywords:
     :type     - Report type: :task or :resource (default :task)
     :format   - Output format: :html or :csv (default :html)
     :columns  - List of column names to include
     :filter   - Lambda function to filter items (optional)
     :sort-by  - Lambda function for sorting (optional)

   The report is stored in *current-project* and can be generated with:
     (generate-project-report *current-project* 'report-id)

   Or saved to a file:
     (save-project-report *current-project* 'report-id \"output.html\")"

  ;; Parse keyword arguments
  (let ((type-expr :task)
        (format-expr :html)
        (columns-expr nil)
        (filter-expr nil)
        (sort-by-expr nil)
        (remaining body))

    (loop while (and remaining (keywordp (first remaining)))
          do (let ((keyword (first remaining))
                   (value (second remaining)))
               (case keyword
                 (:type (setf type-expr value))
                 (:format (setf format-expr value))
                 (:columns (setf columns-expr value))
                 (:filter (setf filter-expr value))
                 (:sort-by (setf sort-by-expr value))
                 (t (warn "Unknown keyword in defreport: ~A" keyword)))
               (setf remaining (cddr remaining))))

    ;; Validate columns were specified
    (unless columns-expr
      (error "defreport requires :columns keyword"))

    ;; Build report instance creation
    `(let ((report (make-instance ',(ecase type-expr
                                      (:task 'task-report)
                                      (:resource 'resource-report))
                                  :id ',id
                                  :title ,title
                                  :format ,format-expr
                                  :columns ',columns-expr
                                  ,@(when filter-expr `(:filter ,filter-expr))
                                  ,@(when sort-by-expr `(:sort-by ,sort-by-expr)))))

       ;; Register report with project
       (setf (gethash ',id (project-reports *current-project*)) report)

       ;; Return the report
       report)))
