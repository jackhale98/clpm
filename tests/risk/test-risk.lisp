;;;; tests/risk/test-risk.lisp
;;;; Tests for risk register and risk management

(in-package #:project-juggler-tests)

(def-suite risk-suite
  :in project-juggler-suite
  :description "Tests for risk register and risk management")

(in-suite risk-suite)

;;; ============================================================================
;;; Risk Class Tests
;;; ============================================================================

(test create-risk-basic
  "Test creating a basic risk"
  (with-test-project
    (let ((risk (create-risk *current-project* 'risk1 "Server failure")))
      (is (not (null risk)))
      (is (eq 'risk1 (risk-id risk)))
      (is (string= "Server failure" (risk-name risk))))))

(test create-risk-with-properties
  "Test creating a risk with all properties"
  (with-test-project
    (let ((risk (create-risk *current-project* 'risk1 "Key developer leaves"
                             :probability 0.3
                             :impact 0.8
                             :category :resource
                             :description "Senior developer might leave mid-project"
                             :owner "PM"
                             :mitigation "Cross-train team members"
                             :contingency "Have contractor backup ready")))
      (is (= 0.3 (risk-probability risk)))
      (is (= 0.8 (risk-impact risk)))
      (is (eq :resource (risk-category risk)))
      (is (string= "Senior developer might leave mid-project" (risk-description risk)))
      (is (string= "PM" (risk-owner risk)))
      (is (string= "Cross-train team members" (risk-mitigation risk)))
      (is (string= "Have contractor backup ready" (risk-contingency risk))))))

(test risk-score-calculation
  "Test risk score (probability * impact) calculation"
  (with-test-project
    (let ((risk (create-risk *current-project* 'risk1 "Risk"
                             :probability 0.5
                             :impact 0.6)))
      ;; Score = 0.5 * 0.6 = 0.3
      (is (= 0.3 (risk-score risk))))))

(test risk-severity-low
  "Test low severity risk classification"
  (with-test-project
    (let ((risk (create-risk *current-project* 'risk1 "Low risk"
                             :probability 0.1
                             :impact 0.2)))
      ;; Score = 0.02, should be low
      (is (eq :low (risk-severity risk))))))

(test risk-severity-medium
  "Test medium severity risk classification"
  (with-test-project
    (let ((risk (create-risk *current-project* 'risk1 "Medium risk"
                             :probability 0.3
                             :impact 0.5)))
      ;; Score = 0.15, should be medium
      (is (eq :medium (risk-severity risk))))))

(test risk-severity-high
  "Test high severity risk classification"
  (with-test-project
    (let ((risk (create-risk *current-project* 'risk1 "High risk"
                             :probability 0.7
                             :impact 0.8)))
      ;; Score = 0.56, should be high
      (is (eq :high (risk-severity risk))))))

(test risk-severity-critical
  "Test critical severity risk classification"
  (with-test-project
    (let ((risk (create-risk *current-project* 'risk1 "Critical risk"
                             :probability 0.9
                             :impact 0.9)))
      ;; Score = 0.81, should be critical
      (is (eq :critical (risk-severity risk))))))

;;; ============================================================================
;;; Risk Status Tests
;;; ============================================================================

(test risk-status-open
  "Test default risk status is open"
  (with-test-project
    (let ((risk (create-risk *current-project* 'risk1 "Risk")))
      (is (eq :open (risk-status risk))))))

(test risk-status-transitions
  "Test risk status transitions"
  (with-test-project
    (let ((risk (create-risk *current-project* 'risk1 "Risk")))
      ;; Initial status
      (is (eq :open (risk-status risk)))
      ;; Mitigated
      (setf (risk-status risk) :mitigated)
      (is (eq :mitigated (risk-status risk)))
      ;; Occurred
      (setf (risk-status risk) :occurred)
      (is (eq :occurred (risk-status risk)))
      ;; Closed
      (setf (risk-status risk) :closed)
      (is (eq :closed (risk-status risk))))))

;;; ============================================================================
;;; Risk-Task Association Tests
;;; ============================================================================

(test risk-associated-tasks
  "Test associating risks with tasks"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Critical task" :duration (duration 5 :days) :allocate (dev1))
    (finalize-project *current-project*)
    (let ((risk (create-risk *current-project* 'risk1 "Scope creep"
                             :tasks '(task1))))
      (is (member 'task1 (risk-tasks risk))))))

(test task-risks
  "Test getting risks for a task"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Critical task" :duration (duration 5 :days) :allocate (dev1))
    (finalize-project *current-project*)
    (create-risk *current-project* 'risk1 "Scope creep" :tasks '(task1))
    (create-risk *current-project* 'risk2 "Tech failure" :tasks '(task1))
    (let ((task-risks (get-task-risks *current-project* 'task1)))
      (is (= 2 (length task-risks))))))

;;; ============================================================================
;;; Risk Register Tests
;;; ============================================================================

(test project-risk-register
  "Test project risk register"
  (with-test-project
    (create-risk *current-project* 'risk1 "Risk 1")
    (create-risk *current-project* 'risk2 "Risk 2")
    (create-risk *current-project* 'risk3 "Risk 3")
    (let ((risks (project-risks *current-project*)))
      (is (= 3 (length risks))))))

(test get-risk-by-id
  "Test retrieving a risk by ID"
  (with-test-project
    (create-risk *current-project* 'risk1 "Risk One")
    (let ((risk (get-risk *current-project* 'risk1)))
      (is (not (null risk)))
      (is (string= "Risk One" (risk-name risk))))))

(test delete-risk
  "Test deleting a risk"
  (with-test-project
    (create-risk *current-project* 'risk1 "Risk to delete")
    (is (= 1 (length (project-risks *current-project*))))
    (delete-risk *current-project* 'risk1)
    (is (= 0 (length (project-risks *current-project*))))))

(test update-risk
  "Test updating risk properties"
  (with-test-project
    (create-risk *current-project* 'risk1 "Risk" :probability 0.3 :impact 0.4)
    (update-risk *current-project* 'risk1 :probability 0.7 :impact 0.9)
    (let ((risk (get-risk *current-project* 'risk1)))
      (is (= 0.7 (risk-probability risk)))
      (is (= 0.9 (risk-impact risk))))))

;;; ============================================================================
;;; Risk Filtering and Sorting Tests
;;; ============================================================================

(test filter-risks-by-status
  "Test filtering risks by status"
  (with-test-project
    (let ((r1 (create-risk *current-project* 'risk1 "Risk 1"))
          (r2 (create-risk *current-project* 'risk2 "Risk 2"))
          (r3 (create-risk *current-project* 'risk3 "Risk 3")))
      (setf (risk-status r2) :closed)
      (setf (risk-status r3) :mitigated)
      (let ((open-risks (filter-risks-by-status *current-project* :open)))
        (is (= 1 (length open-risks)))
        (is (eq 'risk1 (risk-id (first open-risks))))))))

(test filter-risks-by-category
  "Test filtering risks by category"
  (with-test-project
    (create-risk *current-project* 'risk1 "Risk 1" :category :technical)
    (create-risk *current-project* 'risk2 "Risk 2" :category :resource)
    (create-risk *current-project* 'risk3 "Risk 3" :category :technical)
    (let ((tech-risks (filter-risks-by-category *current-project* :technical)))
      (is (= 2 (length tech-risks))))))

(test filter-risks-by-severity
  "Test filtering risks by severity level"
  (with-test-project
    (create-risk *current-project* 'risk1 "High" :probability 0.8 :impact 0.9)
    (create-risk *current-project* 'risk2 "Low" :probability 0.1 :impact 0.1)
    (create-risk *current-project* 'risk3 "Critical" :probability 0.9 :impact 0.95)
    (let ((high-or-critical (filter-risks-by-min-severity *current-project* :high)))
      (is (= 2 (length high-or-critical))))))

(test sort-risks-by-score
  "Test sorting risks by score (descending)"
  (with-test-project
    (create-risk *current-project* 'risk1 "Low" :probability 0.1 :impact 0.2)
    (create-risk *current-project* 'risk2 "High" :probability 0.8 :impact 0.9)
    (create-risk *current-project* 'risk3 "Medium" :probability 0.5 :impact 0.5)
    (let ((sorted (sort-risks-by-score *current-project*)))
      (is (eq 'risk2 (risk-id (first sorted))))  ; Highest score first
      (is (eq 'risk3 (risk-id (second sorted))))
      (is (eq 'risk1 (risk-id (third sorted)))))))

;;; ============================================================================
;;; Risk Impact on Schedule Tests
;;; ============================================================================

(test risk-schedule-impact
  "Test calculating potential schedule impact from a risk"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1" :duration (duration 10 :days) :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    ;; If risk occurs, task takes 50% longer
    (let ((risk (create-risk *current-project* 'risk1 "Delay risk"
                             :tasks '(task1)
                             :schedule-impact 0.5)))
      ;; Impact days = 10 * 0.5 = 5 additional days
      (is (= 5 (risk-schedule-impact-days risk *current-project*))))))

(test risk-cost-impact
  "Test calculating potential cost impact from a risk"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1" :effort (duration 10 :days) :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    ;; Risk adds fixed cost of $5000
    (let ((risk (create-risk *current-project* 'risk1 "Cost risk"
                             :tasks '(task1)
                             :cost-impact 5000.0)))
      (is (= 5000.0 (risk-cost-impact-amount risk))))))

;;; ============================================================================
;;; Risk Register Summary Tests
;;; ============================================================================

(test risk-register-summary
  "Test risk register summary statistics"
  (with-test-project
    (let ((r1 (create-risk *current-project* 'risk1 "R1" :probability 0.8 :impact 0.9))
          (r2 (create-risk *current-project* 'risk2 "R2" :probability 0.1 :impact 0.1))
          (r3 (create-risk *current-project* 'risk3 "R3" :probability 0.5 :impact 0.5)))
      (setf (risk-status r3) :closed)
      (let ((summary (risk-register-summary *current-project*)))
        (is (= 3 (getf summary :total-risks)))
        (is (= 2 (getf summary :open-risks)))
        (is (numberp (getf summary :average-score)))))))

(test expected-monetary-value
  "Test EMV (Expected Monetary Value) calculation"
  (with-test-project
    ;; Risk with 30% probability of $10000 impact
    (create-risk *current-project* 'risk1 "Risk 1"
                 :probability 0.3
                 :impact 0.8
                 :cost-impact 10000.0)
    ;; Risk with 50% probability of $5000 impact
    (create-risk *current-project* 'risk2 "Risk 2"
                 :probability 0.5
                 :impact 0.5
                 :cost-impact 5000.0)
    ;; Total EMV = 0.3*10000 + 0.5*5000 = 3000 + 2500 = 5500
    (let ((emv (project-expected-monetary-value *current-project*)))
      (is (= 5500.0 emv)))))

;;; ============================================================================
;;; Risk Response Tests
;;; ============================================================================

(test risk-response-history
  "Test recording risk response actions"
  (with-test-project
    (let ((risk (create-risk *current-project* 'risk1 "Risk")))
      (add-risk-response risk "Identified potential issue" :identified)
      (add-risk-response risk "Implemented mitigation strategy" :action)
      (let ((history (risk-response-history risk)))
        (is (= 2 (length history)))))))

