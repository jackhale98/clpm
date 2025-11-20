;;;; tests/reporting/test-reports.lisp
;;;; Tests for Reporting Engine (Phase 8)

(in-package #:project-juggler-tests)

(def-suite reporting-suite
  :in project-juggler-suite
  :description "Reporting engine tests")

(in-suite reporting-suite)

;;; ============================================================================
;;; Report Base Tests
;;; ============================================================================

(test create-task-report
  "Can create a task report"
  (let ((report (make-instance 'task-report
                              :id 'test-report
                              :title "Test Report"
                              :format :html
                              :columns '(:name :start :end :duration))))
    (is (typep report 'task-report))
    (is (eq 'test-report (report-id report)))
    (is (string= "Test Report" (report-title report)))
    (is (eq :html (report-format report)))
    (is (equal '(:name :start :end :duration) (report-columns report)))))

(test create-resource-report
  "Can create a resource report"
  (let ((report (make-instance 'resource-report
                              :id 'resource-report
                              :title "Resource Report"
                              :format :csv
                              :columns '(:name :efficiency :rate))))
    (is (typep report 'resource-report))
    (is (eq :csv (report-format report)))))

;;; ============================================================================
;;; Task Report Generation Tests
;;; ============================================================================

(test generate-html-task-report-basic
  "Generate basic HTML task report"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask t2 "Task 2"
      :depends-on (t1)
      :duration (duration 3 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let* ((report (make-instance 'task-report
                                 :id 'basic-report
                                 :title "Task Report"
                                 :format :html
                                 :columns '(:name :start :end)))
           (output (generate-report report *current-project*)))
      (is (stringp output))
      (is (search "Task Report" output))
      (is (search "Task 1" output))
      (is (search "Task 2" output))
      (is (search "<table" output))
      (is (search "</table>" output)))))

(test generate-csv-task-report-basic
  "Generate basic CSV task report"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let* ((report (make-instance 'task-report
                                 :id 'csv-report
                                 :title "Task Report"
                                 :format :csv
                                 :columns '(:name :start :end)))
           (output (generate-report report *current-project*)))
      (is (stringp output))
      (is (search "Task 1" output))
      (is (search "," output)))))

(test task-report-with-filter
  "Task report with filtering"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days)
      :priority 800)
    (deftask t2 "Task 2"
      :depends-on (t1)
      :duration (duration 3 :days)
      :priority 500)

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let* ((report (make-instance 'task-report
                                 :id 'filtered-report
                                 :title "High Priority Tasks"
                                 :format :html
                                 :columns '(:name :priority)
                                 :filter (lambda (task)
                                          (> (task-priority task) 600))))
           (output (generate-report report *current-project*)))
      (is (search "Task 1" output))
      (is (not (search "Task 2" output))))))

(test task-report-with-sorting
  "Task report with sorting"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days)
      :priority 500)
    (deftask t2 "Task 2"
      :start (date 2024 3 10)
      :duration (duration 3 :days)
      :priority 800)

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let* ((report (make-instance 'task-report
                                 :id 'sorted-report
                                 :title "Tasks by Priority"
                                 :format :html
                                 :columns '(:name :priority)
                                 :sort-by (lambda (a b)
                                           (> (task-priority a)
                                              (task-priority b)))))
           (output (generate-report report *current-project*)))
      (is (stringp output))
      ;; Task 2 should appear before Task 1 due to sorting
      (is (< (search "Task 2" output)
             (search "Task 1" output))))))

;;; ============================================================================
;;; Resource Report Generation Tests
;;; ============================================================================

(test generate-html-resource-report-basic
  "Generate basic HTML resource report"
  (with-test-project
    (defresource dev1 "Developer 1"
      :efficiency 1.0
      :rate 150.0)
    (defresource dev2 "Developer 2"
      :efficiency 0.9
      :rate 140.0)

    (let* ((report (make-instance 'resource-report
                                 :id 'resource-report
                                 :title "Resource Report"
                                 :format :html
                                 :columns '(:name :efficiency :rate)))
           (output (generate-report report *current-project*)))
      (is (stringp output))
      (is (search "Resource Report" output))
      (is (search "Developer 1" output))
      (is (search "Developer 2" output)))))

(test generate-csv-resource-report-basic
  "Generate basic CSV resource report"
  (with-test-project
    (defresource dev1 "Developer 1"
      :efficiency 1.0)

    (let* ((report (make-instance 'resource-report
                                 :id 'csv-report
                                 :title "Resources"
                                 :format :csv
                                 :columns '(:name :efficiency)))
           (output (generate-report report *current-project*)))
      (is (stringp output))
      (is (search "Developer 1" output))
      (is (search "," output)))))

;;; ============================================================================
;;; Column Formatting Tests
;;; ============================================================================

(test format-date-column
  "Date columns formatted correctly"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 15)
      :duration (duration 5 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let* ((report (make-instance 'task-report
                                 :id 'date-report
                                 :title "Date Report"
                                 :format :html
                                 :columns '(:name :start)))
           (output (generate-report report *current-project*)))
      (is (search "2024" output))
      (is (search "03" output))
      (is (search "15" output)))))

(test format-duration-column
  "Duration columns formatted correctly"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let* ((report (make-instance 'task-report
                                 :id 'duration-report
                                 :title "Duration Report"
                                 :format :html
                                 :columns '(:name :duration)))
           (output (generate-report report *current-project*)))
      (is (search "5" output))
      (is (search "days" output)))))

;;; ============================================================================
;;; Gantt Chart Tests
;;; ============================================================================

(test generate-gantt-data
  "Generate Gantt chart data structure"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask t2 "Task 2"
      :depends-on (t1)
      :duration (duration 3 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((gantt-data (generate-gantt-data *current-project*)))
      (is (listp gantt-data))
      (is (= 2 (length gantt-data)))
      ;; Each entry should have task info
      (let ((entry (first gantt-data)))
        (is (getf entry :id))
        (is (getf entry :name))
        (is (getf entry :start))
        (is (getf entry :end))))))

(test gantt-data-ordering
  "Gantt chart data ordered by start date"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 10)
      :duration (duration 5 :days))
    (deftask t2 "Task 2"
      :start (date 2024 3 1)
      :duration (duration 3 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((gantt-data (generate-gantt-data *current-project*)))
      ;; Task 2 should come before Task 1 (starts earlier)
      (is (eq 't2 (getf (first gantt-data) :id)))
      (is (eq 't1 (getf (second gantt-data) :id))))))

(test gantt-data-with-dependencies
  "Gantt chart data includes dependencies"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask t2 "Task 2"
      :depends-on (t1)
      :duration (duration 3 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let* ((gantt-data (generate-gantt-data *current-project*))
           (t2-entry (find 't2 gantt-data :key (lambda (e) (getf e :id)))))
      (is (not (null t2-entry)))
      (is (getf t2-entry :dependencies))
      (is (member 't1 (getf t2-entry :dependencies))))))

;;; ============================================================================
;;; Report Collection Tests
;;; ============================================================================

(test collect-tasks-for-report-all
  "Collect all tasks when no filter specified"
  (with-test-project
    (deftask t1 "Task 1")
    (deftask t2 "Task 2")
    (deftask t3 "Task 3")

    (let* ((report (make-instance 'task-report
                                 :id 'all-tasks
                                 :title "All Tasks"
                                 :format :html
                                 :columns '(:name)))
           (tasks (collect-tasks-for-report report *current-project*)))
      (is (= 3 (length tasks))))))

(test collect-tasks-for-report-filtered
  "Collect only filtered tasks"
  (with-test-project
    (deftask t1 "Task 1" :priority 800)
    (deftask t2 "Task 2" :priority 500)
    (deftask t3 "Task 3" :priority 900)

    (let* ((report (make-instance 'task-report
                                 :id 'high-priority
                                 :title "High Priority"
                                 :format :html
                                 :columns '(:name)
                                 :filter (lambda (task) (> (task-priority task) 600))))
           (tasks (collect-tasks-for-report report *current-project*)))
      (is (= 2 (length tasks)))
      (is (every (lambda (task) (> (task-priority task) 600)) tasks)))))

(test sort-tasks-for-report
  "Sort tasks according to report specification"
  (with-test-project
    (deftask t1 "Task 1" :priority 500)
    (deftask t2 "Task 2" :priority 800)
    (deftask t3 "Task 3" :priority 600)

    (let* ((report (make-instance 'task-report
                                 :id 'sorted
                                 :title "Sorted"
                                 :format :html
                                 :columns '(:name)
                                 :sort-by (lambda (a b)
                                           (> (task-priority a)
                                              (task-priority b)))))
           (tasks (collect-tasks-for-report report *current-project*))
           (sorted (sort-tasks-for-report tasks report)))
      (is (eq 't2 (task-id (first sorted))))
      (is (eq 't3 (task-id (second sorted))))
      (is (eq 't1 (task-id (third sorted)))))))
