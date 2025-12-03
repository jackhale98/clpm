;;;; src/risk/risk.lisp
;;;; Risk register and risk management

(in-package #:project-juggler)

;;; ============================================================================
;;; Risk Class
;;; ============================================================================

(defclass risk ()
  ((id :initarg :id :reader risk-id)
   (name :initarg :name :accessor risk-name)
   (description :initarg :description :initform nil :accessor risk-description)
   (probability :initarg :probability :initform 0.5 :accessor risk-probability
                :documentation "Probability of occurrence (0.0 - 1.0)")
   (impact :initarg :impact :initform 0.5 :accessor risk-impact
           :documentation "Impact severity if occurred (0.0 - 1.0)")
   (category :initarg :category :initform :general :accessor risk-category
             :documentation "Risk category: :technical, :resource, :schedule, :cost, :scope, :external, :general")
   (status :initarg :status :initform :open :accessor risk-status
           :documentation "Risk status: :open, :mitigated, :occurred, :closed")
   (owner :initarg :owner :initform nil :accessor risk-owner
          :documentation "Person responsible for managing this risk")
   (mitigation :initarg :mitigation :initform nil :accessor risk-mitigation
               :documentation "Mitigation strategy")
   (contingency :initarg :contingency :initform nil :accessor risk-contingency
                :documentation "Contingency plan if risk occurs")
   (tasks :initarg :tasks :initform nil :accessor risk-tasks
          :documentation "List of task IDs affected by this risk")
   (schedule-impact :initarg :schedule-impact :initform 0.0 :accessor risk-schedule-impact
                    :documentation "Fractional increase in task duration if risk occurs")
   (cost-impact :initarg :cost-impact :initform 0.0 :accessor risk-cost-impact-amount
                :documentation "Additional cost if risk occurs")
   (created-at :initarg :created-at :initform (get-universal-time) :reader risk-created-at)
   (updated-at :initarg :updated-at :initform (get-universal-time) :accessor risk-updated-at)
   (response-history :initform nil :accessor risk-response-history
                     :documentation "List of (timestamp action-text type) entries"))
  (:documentation "A project risk with probability, impact, and response plans"))

(defun risk-p (obj)
  "Check if object is a risk."
  (typep obj 'risk))

;;; ============================================================================
;;; Risk Calculations
;;; ============================================================================

(defun risk-score (risk)
  "Calculate risk score (probability * impact)."
  (* (risk-probability risk) (risk-impact risk)))

(defun risk-severity (risk)
  "Classify risk severity based on score.
   Returns :low, :medium, :high, or :critical."
  (let ((score (risk-score risk)))
    (cond
      ((< score 0.1) :low)
      ((< score 0.3) :medium)
      ((< score 0.6) :high)
      (t :critical))))

;;; Severity level ordering for comparisons
(defparameter *severity-order*
  '(:low 0 :medium 1 :high 2 :critical 3))

(defun severity>= (sev1 sev2)
  "Check if severity1 is >= severity2."
  (>= (getf *severity-order* sev1)
      (getf *severity-order* sev2)))

;;; ============================================================================
;;; Risk Creation and Management
;;; ============================================================================

(defun create-risk (project id name &key
                                       (probability 0.5)
                                       (impact 0.5)
                                       (category :general)
                                       description
                                       owner
                                       mitigation
                                       contingency
                                       tasks
                                       (schedule-impact 0.0)
                                       (cost-impact 0.0))
  "Create a new risk and register it with the project."
  (let ((risk (make-instance 'risk
                            :id id
                            :name name
                            :probability probability
                            :impact impact
                            :category category
                            :description description
                            :owner owner
                            :mitigation mitigation
                            :contingency contingency
                            :tasks tasks
                            :schedule-impact schedule-impact
                            :cost-impact cost-impact)))
    ;; Add to project's risk register
    (push risk (project-risk-register project))
    risk))

(defun get-risk (project id)
  "Get a risk by ID from the project."
  (find id (project-risk-register project) :key #'risk-id))

(defun delete-risk (project id)
  "Delete a risk from the project."
  (setf (project-risk-register project)
        (remove id (project-risk-register project) :key #'risk-id)))

(defun update-risk (project id &key probability impact category status
                                 description owner mitigation contingency
                                 tasks schedule-impact cost-impact)
  "Update risk properties."
  (let ((risk (get-risk project id)))
    (when risk
      (when probability (setf (risk-probability risk) probability))
      (when impact (setf (risk-impact risk) impact))
      (when category (setf (risk-category risk) category))
      (when status (setf (risk-status risk) status))
      (when description (setf (risk-description risk) description))
      (when owner (setf (risk-owner risk) owner))
      (when mitigation (setf (risk-mitigation risk) mitigation))
      (when contingency (setf (risk-contingency risk) contingency))
      (when tasks (setf (risk-tasks risk) tasks))
      (when schedule-impact (setf (risk-schedule-impact risk) schedule-impact))
      (when cost-impact (setf (risk-cost-impact-amount risk) cost-impact))
      (setf (risk-updated-at risk) (get-universal-time))
      risk)))

;;; ============================================================================
;;; Risk-Task Associations
;;; ============================================================================

(defun get-task-risks (project task-id)
  "Get all risks associated with a task."
  (remove-if-not (lambda (risk)
                   (member task-id (risk-tasks risk)))
                 (project-risk-register project)))

;;; ============================================================================
;;; Risk Filtering and Sorting
;;; ============================================================================

(defun project-risks (project)
  "Get all risks for a project (alias for project-risk-register)."
  (project-risk-register project))

(defun filter-risks-by-status (project status)
  "Filter risks by status."
  (remove-if-not (lambda (risk) (eq status (risk-status risk)))
                 (project-risk-register project)))

(defun filter-risks-by-category (project category)
  "Filter risks by category."
  (remove-if-not (lambda (risk) (eq category (risk-category risk)))
                 (project-risk-register project)))

(defun filter-risks-by-min-severity (project min-severity)
  "Filter risks with severity >= min-severity."
  (remove-if-not (lambda (risk)
                   (severity>= (risk-severity risk) min-severity))
                 (project-risk-register project)))

(defun sort-risks-by-score (project)
  "Sort risks by score (highest first)."
  (sort (copy-list (project-risk-register project))
        #'> :key #'risk-score))

;;; ============================================================================
;;; Risk Impact Calculations
;;; ============================================================================

(defun risk-schedule-impact-days (risk project)
  "Calculate potential schedule impact in days for a risk."
  (let ((total-days 0))
    (dolist (task-id (risk-tasks risk))
      (let ((task (gethash task-id (project-tasks project))))
        (when task
          (let ((task-duration (if (task-duration task)
                                   (duration-in-days (task-duration task))
                                   (if (task-effort task)
                                       (duration-in-days (task-effort task))
                                       0))))
            (incf total-days (* task-duration (risk-schedule-impact risk)))))))
    total-days))

;;; ============================================================================
;;; Risk Register Summary
;;; ============================================================================

(defun risk-register-summary (project)
  "Get summary statistics for the risk register."
  (let* ((all-risks (project-risk-register project))
         (open-risks (filter-risks-by-status project :open))
         (scores (mapcar #'risk-score all-risks))
         (avg-score (if scores
                        (/ (reduce #'+ scores) (length scores))
                        0)))
    (list :total-risks (length all-risks)
          :open-risks (length open-risks)
          :average-score avg-score
          :high-severity-count (length (filter-risks-by-min-severity project :high))
          :critical-count (length (filter-risks-by-min-severity project :critical)))))

(defun project-expected-monetary-value (project)
  "Calculate total Expected Monetary Value (EMV) for all project risks.
   EMV = Sum of (probability * cost-impact) for all risks."
  (let ((total 0.0))
    (dolist (risk (project-risk-register project))
      (incf total (* (risk-probability risk)
                     (risk-cost-impact-amount risk))))
    total))

;;; ============================================================================
;;; Risk Response History
;;; ============================================================================

(defun add-risk-response (risk text &optional (type :note))
  "Add a response entry to the risk's history."
  (push (list (get-universal-time) text type)
        (risk-response-history risk)))

