;;;; src/cli/output.lisp
;;;; Output formatting for CLAPS CLI

(in-package #:claps/cli)

;;; ============================================================================
;;; Basic Output Functions
;;; ============================================================================

(defvar *quiet* nil
  "When true, suppress informational output")

(defun print-error (format-string &rest args)
  "Print an error message to stderr"
  (format *error-output* "Error: ~?~%" format-string args))

(defun print-warning (format-string &rest args)
  "Print a warning message to stderr"
  (format *error-output* "Warning: ~?~%" format-string args))

(defun print-info (format-string &rest args)
  "Print an informational message (respects --quiet)"
  (unless *quiet*
    (format t "~?~%" format-string args)))

;;; ============================================================================
;;; Error Formatting
;;; ============================================================================

(defun format-lisp-error (condition)
  "Convert a Lisp condition to a user-friendly error message"
  (typecase condition
    (claps:reference-error
     (format nil "Cannot find '~A' - check task/resource names"
             (claps::error-reference condition)))
    (claps:circular-dependency-error
     (format nil "Circular dependency: ~{~A~^ -> ~}"
             (claps::error-cycles condition)))
    (claps:validation-error
     (format nil "Validation error: ~A"
             (claps::error-message condition)))
    (claps:scheduling-error
     (format nil "Cannot schedule: ~A"
             (claps::error-message condition)))
    (file-error
     (format nil "File not found: ~A"
             (file-error-pathname condition)))
    (reader-error
     "Syntax error in project file")
    (t
     (format nil "~A" condition))))

;;; ============================================================================
;;; JSON Output
;;; ============================================================================

(defun escape-json-string (str)
  "Escape a string for JSON output"
  (with-output-to-string (out)
    (loop for char across str
          do (case char
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (write-char char out))))))

(defun to-json-string (obj &key (null-as-array nil))
  "Convert an object to a JSON string.
   If NULL-AS-ARRAY is true, nil converts to [] instead of null."
  (typecase obj
    (null (if null-as-array "[]" "null"))
    ((eql t) "true")
    (string (format nil "\"~A\"" (escape-json-string obj)))
    (symbol (format nil "\"~A\"" (escape-json-string (symbol-name obj))))
    (integer (format nil "~D" obj))
    (float (format nil "~F" obj))
    (ratio (format nil "~F" (float obj)))
    (list
     (if (keywordp (first obj))
         ;; Plist - convert to object
         (with-output-to-string (out)
           (write-char #\{ out)
           (loop for (key val) on obj by #'cddr
                 for first = t then nil
                 do (unless first (write-string "," out))
                    (format out "\"~A\":~A"
                            (string-downcase (symbol-name key))
                            (to-json-string val)))
           (write-char #\} out))
         ;; Regular list - convert to array
         (with-output-to-string (out)
           (write-char #\[ out)
           (loop for item in obj
                 for first = t then nil
                 do (unless first (write-string "," out))
                    (write-string (to-json-string item) out))
           (write-char #\] out))))
    (hash-table
     (with-output-to-string (out)
       (write-char #\{ out)
       (let ((first t))
         (maphash (lambda (key val)
                    (unless first (write-string "," out))
                    (setf first nil)
                    (format out "\"~A\":~A"
                            (escape-json-string (format nil "~A" key))
                            (to-json-string val)))
                  obj))
       (write-char #\} out)))
    (claps:pj-date
     (format nil "\"~A\"" (claps:date-timestamp obj)))
    (claps:duration
     (format nil "~D" (claps:duration-in-days obj)))
    (t
     (format nil "\"~A\"" (escape-json-string (format nil "~A" obj))))))

;;; ============================================================================
;;; Summary Output
;;; ============================================================================

(defun format-summary (project &key milestones json)
  "Format a project summary"
  (let* ((tasks (claps:project-tasks project))
         (resources (claps:project-resources project))
         (task-count (hash-table-count tasks))
         (resource-count (hash-table-count resources))
         (scenarios (claps:list-scenarios project))
         (critical-tasks (claps:critical-path project))
         (critical-count (length critical-tasks))
         (scheduled-count 0)
         (milestone-count 0)
         (total-complete 0)
         (completed-tasks 0))
    ;; Count task statistics
    (maphash (lambda (id task)
               (declare (ignore id))
               (when (claps:task-scheduled-p task)
                 (incf scheduled-count))
               (when (claps:task-milestone-p task)
                 (incf milestone-count))
               (when (claps:task-complete task)
                 (incf total-complete (claps:task-complete task))
                 (when (>= (claps:task-complete task) 100)
                   (incf completed-tasks))))
             tasks)
    (let ((progress (if (plusp task-count)
                        (round (/ total-complete task-count))
                        0)))
      (if json
          ;; JSON output
          (let ((data `(:project ,(claps:project-name project)
                        :start ,(claps:date-timestamp (claps:project-start project))
                        :end ,(claps:date-timestamp (claps:project-end project))
                        :tasks (:total ,task-count
                                :scheduled ,scheduled-count
                                :milestones ,milestone-count
                                :completed ,completed-tasks)
                        :resources ,resource-count
                        :scenarios ,(mapcar #'symbol-name scenarios)
                        :progress ,progress
                        :critical-path (:count ,critical-count
                                        :tasks ,(mapcar (lambda (tsk) (symbol-name (claps:task-id tsk)))
                                                        critical-tasks)))))
            (format t "~A~%" (to-json-string data)))
          ;; Text output
          (progn
            (format t "CLAPS - Project Summary~%")
            (format t "=======================~%")
            (format t "Project: ~A~%" (claps:project-name project))
            (format t "Period:  ~A to ~A~%"
                    (claps:date-timestamp (claps:project-start project))
                    (claps:date-timestamp (claps:project-end project)))
            (format t "~%")
            (format t "Tasks:      ~D total, ~D scheduled, ~D milestones~%"
                    task-count scheduled-count milestone-count)
            (format t "Resources:  ~D defined~%" resource-count)
            (when scenarios
              (format t "Scenarios:  ~{~A~^, ~}~%" scenarios))
            (format t "~%")
            (format t "Progress:   ~D% complete~%" progress)
            (format t "~%")
            (format t "Critical Path: ~D tasks~%" critical-count)
            (when critical-tasks
              (format t "  ~{~A~^ -> ~}~%"
                      (mapcar #'claps:task-name critical-tasks)))
            (when milestones
              (format t "~%")
              (format-milestones project)))))))

;;; ============================================================================
;;; Critical Path Output
;;; ============================================================================

(defun format-critical-path (project &key milestones json)
  "Format critical path information"
  (let ((critical-tasks (claps:critical-path project))
        (all-tasks (claps:project-tasks project)))
    (if json
        ;; JSON output
        (let ((data (mapcar (lambda (task)
                              `(:id ,(symbol-name (claps:task-id task))
                                :name ,(claps:task-name task)
                                :start ,(claps:date-timestamp (claps:task-start task))
                                :end ,(claps:date-timestamp (claps:task-end task))
                                :slack ,(or (claps:task-slack task) 0)
                                :milestone ,(if (claps:task-milestone-p task) t nil)))
                            critical-tasks)))
          (format t "~A~%" (to-json-string data)))
        ;; Text output
        (progn
          (format t "Critical Path~%")
          (format t "=============~%")
          (format t "~D tasks on critical path~%~%" (length critical-tasks))
          (format t "~20A ~12A ~12A ~8A~%" "Task" "Start" "End" "Slack")
          (format t "~20A ~12A ~12A ~8A~%" "----" "-----" "---" "-----")
          (dolist (task critical-tasks)
            (format t "~20A ~12A ~12A ~8A~%"
                    (claps:task-name task)
                    (claps:date-timestamp (claps:task-start task))
                    (claps:date-timestamp (claps:task-end task))
                    (format nil "~D days" (or (claps:task-slack task) 0))))
          (when milestones
            (format t "~%Path: ")
            (loop for task in critical-tasks
                  for first = t then nil
                  do (unless first (format t " -> "))
                     (if (claps:task-milestone-p task)
                         (format t "[M] ~A" (claps:task-name task))
                         (format t "~A" (claps:task-name task))))
            (format t "~%"))))))

;;; ============================================================================
;;; Milestones Output
;;; ============================================================================

(defun format-milestones (project &key json)
  "Format milestone information"
  (let ((milestones nil))
    ;; Collect milestones
    (maphash (lambda (id task)
               (declare (ignore id))
               (when (claps:task-milestone-p task)
                 (push task milestones)))
             (claps:project-tasks project))
    ;; Sort by date
    (setf milestones (sort milestones
                           (lambda (a b)
                             (if (and (claps:task-end a) (claps:task-end b))
                                 (claps:date< (claps:task-end a) (claps:task-end b))
                                 nil))))
    (if json
        ;; JSON output
        (let ((data (mapcar (lambda (task)
                              (let ((status (cond
                                              ((>= (or (claps:task-complete task) 0) 100) "complete")
                                              ((> (or (claps:task-complete task) 0) 0) "in_progress")
                                              (t "pending"))))
                                `(:id ,(symbol-name (claps:task-id task))
                                  :name ,(claps:task-name task)
                                  :date ,(when (claps:task-end task)
                                           (claps:date-timestamp (claps:task-end task)))
                                  :status ,status)))
                            milestones)))
          (format t "~A~%" (to-json-string data)))
        ;; Text output
        (progn
          (format t "Project Milestones~%")
          (format t "==================~%")
          (format t "~12A ~12A ~30A~%" "Date" "Status" "Name")
          (format t "~12A ~12A ~30A~%" "----" "------" "----")
          (dolist (task milestones)
            (let ((status (cond
                            ((>= (or (claps:task-complete task) 0) 100) "Complete")
                            ((> (or (claps:task-complete task) 0) 0) "In Progress")
                            (t "Pending")))
                  (symbol (cond
                            ((>= (or (claps:task-complete task) 0) 100) "OK")
                            ((> (or (claps:task-complete task) 0) 0) "~")
                            (t "o"))))
              (format t "~12A ~12A ~30A~%"
                      (if (claps:task-end task)
                          (claps:date-timestamp (claps:task-end task))
                          "TBD")
                      (format nil "~A ~A" symbol status)
                      (claps:task-name task))))
          (format t "~%Legend: OK=Complete, ~~=In Progress, o=Pending~%")))))

;;; ============================================================================
;;; Resource Output
;;; ============================================================================

(defun format-resources (project &key json)
  "Format resource utilization"
  (let ((resources nil))
    (maphash (lambda (id resource)
               (declare (ignore id))
               (push resource resources))
             (claps:project-resources project))
    (if json
        ;; JSON output
        (let ((data (mapcar (lambda (resource)
                              (let ((allocated (or (claps:resource-allocated-effort resource) 0))
                                    (available (or (claps:resource-available-effort resource) 1)))
                                `(:id ,(symbol-name (claps:resource-id resource))
                                  :name ,(claps:resource-name resource)
                                  :efficiency ,(claps:resource-efficiency resource)
                                  :allocated ,allocated
                                  :available ,available
                                  :load ,(if (plusp available)
                                             (round (* 100 (/ allocated available)))
                                             0))))
                            resources)))
          (format t "~A~%" (to-json-string data)))
        ;; Text output
        (progn
          (format t "Resource Utilization~%")
          (format t "====================~%")
          (format t "~12A ~20A ~12A ~10A ~10A ~6A~%"
                  "ID" "Name" "Efficiency" "Allocated" "Available" "Load")
          (format t "~12A ~20A ~12A ~10A ~10A ~6A~%"
                  "--" "----" "----------" "---------" "---------" "----")
          (let ((total-allocated 0))
            (dolist (resource resources)
              (let* ((allocated (or (claps:resource-allocated-effort resource) 0))
                     (available (or (claps:resource-available-effort resource) 160))
                     (load (if (plusp available)
                               (round (* 100 (/ allocated available)))
                               0)))
                (incf total-allocated allocated)
                (format t "~12A ~20A ~12,1F ~10A ~10A ~5D%~%"
                        (claps:resource-id resource)
                        (claps:resource-name resource)
                        (claps:resource-efficiency resource)
                        (format nil "~Dh" allocated)
                        (format nil "~Dh" available)
                        load)))
            (format t "~%Total: ~D resources, ~Dh allocated~%"
                    (length resources) total-allocated))))))

;;; ============================================================================
;;; Overallocations Output
;;; ============================================================================

(defun format-overallocations (project &key json)
  "Format resource overallocations"
  (let ((overallocations (claps:detect-resource-overallocations project)))
    (if json
        ;; JSON output
        (let ((data (mapcar (lambda (oa)
                              `(:date ,(claps:date-timestamp (claps:overallocation-date oa))
                                :resource ,(symbol-name (claps:overallocation-resource-id oa))
                                :load ,(claps:overallocation-load oa)
                                :tasks ,(mapcar #'symbol-name (claps:overallocation-tasks oa))))
                            overallocations)))
          (format t "~A~%" (to-json-string data)))
        ;; Text output
        (progn
          (format t "Resource Overallocations~%")
          (format t "========================~%")
          (if (null overallocations)
              (format t "No overallocations detected.~%")
              (progn
                (format t "WARNING: ~D overallocation~:P detected~%~%"
                        (length overallocations))
                (format t "~12A ~15A ~8A ~A~%"
                        "Date" "Resource" "Load" "Conflicting Tasks")
                (format t "~12A ~15A ~8A ~A~%"
                        "----" "--------" "----" "-----------------")
                (dolist (oa overallocations)
                  (format t "~12A ~15A ~6D% ~{~A~^, ~}~%"
                          (claps:date-timestamp (claps:overallocation-date oa))
                          (claps:overallocation-resource-id oa)
                          (round (* 100 (claps:overallocation-load oa)))
                          (claps:overallocation-tasks oa)))))))))

;;; ============================================================================
;;; EVM Output
;;; ============================================================================

(defun format-evm (project status-date &key json)
  "Format earned value management metrics"
  (let* ((pv (claps:calculate-planned-value project status-date))
         (ev (claps:calculate-earned-value project))
         (sv (claps:calculate-schedule-variance project status-date))
         (spi (claps:calculate-spi project status-date))
         (status (cond
                   ((> spi 1.0) "Ahead")
                   ((< spi 1.0) "Behind")
                   (t "On Track"))))
    (if json
        ;; JSON output
        (let ((data `(:status-date ,(claps:date-timestamp status-date)
                      :planned-value ,pv
                      :earned-value ,ev
                      :schedule-variance ,sv
                      :spi ,spi
                      :status ,status)))
          (format t "~A~%" (to-json-string data)))
        ;; Text output
        (progn
          (format t "Earned Value Management~%")
          (format t "=======================~%")
          (format t "Status Date: ~A~%~%" (claps:date-timestamp status-date))
          (format t "~25A ~10A ~10A~%" "Metric" "Value" "Status")
          (format t "~25A ~10A ~10A~%" "------" "-----" "------")
          (format t "~25A ~9,1F%~%" "Planned Value (PV)" pv)
          (format t "~25A ~9,1F%~%" "Earned Value (EV)" ev)
          (format t "~25A ~9,1F% ~10A~%" "Schedule Variance (SV)" sv
                  (if (< sv 0) "Behind" ""))
          (format t "~25A ~9,2F ~10A~%" "Schedule Performance (SPI)" spi
                  (cond
                    ((< spi 0.9) "! Critical")
                    ((< spi 1.0) "~ Below target")
                    (t "")))
          (format t "~%Interpretation: Project is ~A (~,1F% ~A schedule)~%"
                  status
                  (abs sv)
                  (if (< sv 0) "behind" "ahead of"))))))

;;; ============================================================================
;;; Simulation Output
;;; ============================================================================

(defun format-simulation (results &key json)
  "Format Monte Carlo simulation results"
  (let* ((mean (claps:simulation-mean results))
         (std-dev (claps:simulation-std-dev results))
         (min-val (claps:simulation-min results))
         (max-val (claps:simulation-max results))
         (p50 (claps:simulation-percentile results 50))
         (p75 (claps:simulation-percentile results 75))
         (p90 (claps:simulation-percentile results 90))
         (trial-count (claps:simulation-trial-count results)))
    (if json
        ;; JSON output
        (let ((data `(:trials ,trial-count
                      :mean ,mean
                      :std-dev ,std-dev
                      :min ,min-val
                      :max ,max-val
                      :percentiles (:p50 ,p50 :p75 ,p75 :p90 ,p90)
                      :probabilities ,(loop for days from (round min-val) to (round max-val) by 10
                                            collect `(:days ,days
                                                      :probability ,(claps:simulation-probability-of-completion
                                                                     results days))))))
          (format t "~A~%" (to-json-string data)))
        ;; Text output
        (progn
          (format t "Monte Carlo Simulation~%")
          (format t "======================~%")
          (format t "Trials: ~:D~%~%" trial-count)
          (format t "Duration Analysis:~%")
          (format t "  Minimum:    ~D days~%" (round min-val))
          (format t "  P50 (50%%):  ~D days~%" (round p50))
          (format t "  P75 (75%%):  ~D days~%" (round p75))
          (format t "  P90 (90%%):  ~D days~%" (round p90))
          (format t "  Maximum:    ~D days~%~%" (round max-val))
          (format t "Probability of completion:~%")
          (let ((base (round min-val)))
            (loop for offset in '(10 20 30 40)
                  for days = (+ base offset)
                  when (<= days (round max-val))
                  do (format t "  By ~D days: ~D%~%"
                             days
                             (round (* 100 (claps:simulation-probability-of-completion
                                            results days))))))))))

;;; ============================================================================
;;; Scenarios Output
;;; ============================================================================

(defun format-scenarios (project &key json)
  "Format scenario listing"
  (let* ((scenarios (claps:list-scenarios project))
         (baseline (claps:baseline-scenario-id project)))
    (if json
        ;; JSON output
        (let ((data (mapcar (lambda (s)
                              (let ((scenario (claps:get-scenario project s)))
                                `(:id ,(symbol-name s)
                                  :name ,(when scenario (claps:scenario-name scenario))
                                  :baseline ,(eq s baseline))))
                            scenarios)))
          (format t "~A~%" (to-json-string data)))
        ;; Text output
        (progn
          (format t "Available Scenarios~%")
          (format t "===================~%")
          (format t "~15A ~30A ~10A~%" "ID" "Name" "Type")
          (format t "~15A ~30A ~10A~%" "--" "----" "----")
          (dolist (s scenarios)
            (let ((scenario (claps:get-scenario project s)))
              (format t "~15A ~30A ~10A~%"
                      s
                      (if scenario (claps:scenario-name scenario) "-")
                      (if (eq s baseline) "baseline" ""))))))))

;;; ============================================================================
;;; Comparison Output
;;; ============================================================================

(defun format-comparison (project scenario-1 scenario-2 &key json)
  "Format scenario comparison"
  (let ((comparison (claps:compare-scenarios project scenario-1 scenario-2)))
    (if json
        ;; JSON output
        (let ((data `(:scenario-1 ,(symbol-name scenario-1)
                      :scenario-2 ,(symbol-name scenario-2)
                      :duration-1 ,(getf comparison :duration-1)
                      :duration-2 ,(getf comparison :duration-2)
                      :effort-1 ,(getf comparison :effort-1)
                      :effort-2 ,(getf comparison :effort-2)
                      :end-1 ,(getf comparison :end-1)
                      :end-2 ,(getf comparison :end-2))))
          (format t "~A~%" (to-json-string data)))
        ;; Text output
        (progn
          (format t "Scenario Comparison~%")
          (format t "===================~%")
          (format t "Comparing: ~A vs ~A~%~%" scenario-1 scenario-2)
          (format t "~20A ~15A ~15A ~15A~%" "Metric" (symbol-name scenario-1) (symbol-name scenario-2) "Difference")
          (format t "~20A ~15A ~15A ~15A~%" "------" "----------" "----------" "----------")
          (let ((dur-1 (getf comparison :duration-1))
                (dur-2 (getf comparison :duration-2)))
            (format t "~20A ~14D ~14D ~+14D~%"
                    "Duration (days)"
                    (or dur-1 0) (or dur-2 0)
                    (- (or dur-2 0) (or dur-1 0))))
          (let ((eff-1 (getf comparison :effort-1))
                (eff-2 (getf comparison :effort-2)))
            (when (and eff-1 eff-2)
              (format t "~20A ~14D ~14D ~+14D~%"
                      "Effort (hours)"
                      eff-1 eff-2 (- eff-2 eff-1))))))))
