;;;; tests/session/test-session.lisp
;;;; Tests for Session Management (Phase 7)

(in-package #:project-juggler-tests)

(def-suite session-suite
  :in project-juggler-suite
  :description "Session management tests")

(in-suite session-suite)

;;; ============================================================================
;;; Load Project Session Tests
;;; ============================================================================

(test load-project-session-basic
  "Can load project from file into session"
  (with-temp-project-file (file)
    (let ((session (load-project-session file)))
      (is (typep session 'session))
      (is (typep (session-project session) 'project))
      (is (eq 'test-project (project-id (session-project session)))))))

(test load-project-session-with-tasks
  "Loaded session includes all project tasks"
  (with-temp-project-file (file)
    (let* ((session (load-project-session file))
           (project (session-project session)))
      (is (not (null (gethash 't1 (project-tasks project)))))
      (is (not (null (gethash 't2 (project-tasks project))))))))

(test load-project-session-finalized
  "Loaded project is finalized and scheduled"
  (with-temp-project-file (file)
    (let* ((session (load-project-session file))
           (project (session-project session))
           (t1 (gethash 't1 (project-tasks project))))
      ;; Project should be finalized and scheduled
      (is (not (null (task-start t1))))
      (is (not (null (task-end t1)))))))

;;; ============================================================================
;;; Change Tracking Tests
;;; ============================================================================

(test session-change-tracking-task-addition
  "Track task addition in session"
  (with-test-session
    (let ((initial-changes (length (session-changes *current-session*))))
      (add-task-to-session 'new-task "New Task"
                          :duration (duration 3 :days))
      (is (= (1+ initial-changes)
             (length (session-changes *current-session*)))))))

(test session-change-tracking-task-modification
  "Track task modification in session"
  (with-test-session
    (let* ((task (gethash 't1 (project-tasks (session-project *current-session*))))
           (initial-changes (length (session-changes *current-session*))))
      (modify-task-in-session task :priority 800)
      (is (= (1+ initial-changes)
             (length (session-changes *current-session*)))))))

(test session-change-tracking-task-deletion
  "Track task deletion in session"
  (with-test-session
    (let* ((task (gethash 't1 (project-tasks (session-project *current-session*))))
           (initial-changes (length (session-changes *current-session*))))
      (delete-task-from-session task)
      (is (= (1+ initial-changes)
             (length (session-changes *current-session*)))))))

;;; ============================================================================
;;; Undo/Redo Tests
;;; ============================================================================

(test undo-task-addition
  "Can undo task addition"
  (with-test-session
    (add-task-to-session 'new-task "New Task" :duration (duration 3 :days))
    (let ((project (session-project *current-session*)))
      (is (not (null (gethash 'new-task (project-tasks project)))))
      (undo)
      (is (null (gethash 'new-task (project-tasks project)))))))

(test undo-task-modification
  "Can undo task modification"
  (with-test-session
    (let* ((project (session-project *current-session*))
           (task (gethash 't1 (project-tasks project)))
           (original-priority (task-priority task)))
      (modify-task-in-session task :priority 800)
      (is (= 800 (task-priority task)))
      (undo)
      (is (= original-priority (task-priority task))))))

(test undo-task-deletion
  "Can undo task deletion"
  (with-test-session
    (let* ((project (session-project *current-session*))
           (task (gethash 't1 (project-tasks project))))
      (delete-task-from-session task)
      (is (null (gethash 't1 (project-tasks project))))
      (undo)
      (is (not (null (gethash 't1 (project-tasks project))))))))

(test redo-task-addition
  "Can redo task addition after undo"
  (with-test-session
    (add-task-to-session 'new-task "New Task" :duration (duration 3 :days))
    (let ((project (session-project *current-session*)))
      (is (not (null (gethash 'new-task (project-tasks project)))))
      (undo)
      (is (null (gethash 'new-task (project-tasks project))))
      (redo)
      (is (not (null (gethash 'new-task (project-tasks project))))))))

(test redo-task-modification
  "Can redo task modification after undo"
  (with-test-session
    (let* ((project (session-project *current-session*))
           (task (gethash 't1 (project-tasks project)))
           (original-priority (task-priority task)))
      (modify-task-in-session task :priority 800)
      (is (= 800 (task-priority task)))
      (undo)
      (is (= original-priority (task-priority task)))
      (redo)
      (is (= 800 (task-priority task))))))

(test multiple-undo-redo
  "Can undo/redo multiple changes"
  (with-test-session
    (add-task-to-session 'task1 "Task 1" :duration (duration 1 :day))
    (add-task-to-session 'task2 "Task 2" :duration (duration 2 :days))
    (add-task-to-session 'task3 "Task 3" :duration (duration 3 :days))
    (let ((project (session-project *current-session*)))
      (is (not (null (gethash 'task1 (project-tasks project)))))
      (is (not (null (gethash 'task2 (project-tasks project)))))
      (is (not (null (gethash 'task3 (project-tasks project)))))

      ;; Undo all three
      (undo)
      (is (null (gethash 'task3 (project-tasks project))))
      (undo)
      (is (null (gethash 'task2 (project-tasks project))))
      (undo)
      (is (null (gethash 'task1 (project-tasks project))))

      ;; Redo all three
      (redo)
      (is (not (null (gethash 'task1 (project-tasks project)))))
      (redo)
      (is (not (null (gethash 'task2 (project-tasks project)))))
      (redo)
      (is (not (null (gethash 'task3 (project-tasks project))))))))

(test undo-without-changes
  "Undo without changes does nothing"
  (with-test-session
    (let ((result (undo)))
      (is (null result)))))

(test redo-without-undo
  "Redo without undo does nothing"
  (with-test-session
    (add-task-to-session 'new-task "New Task" :duration (duration 3 :days))
    (let ((result (redo)))
      (is (null result)))))

;;; ============================================================================
;;; Save Session Tests
;;; ============================================================================

(test save-session-basic
  "Can save session back to file"
  (with-temp-project-file (file)
    (let ((*current-session* (load-project-session file)))
      (add-task-to-session 'new-task "New Task" :duration (duration 3 :days))
      (save-session *current-session* file)

      ;; Reload and verify
      (let* ((session2 (load-project-session file))
             (project2 (session-project session2)))
        (is (not (null (gethash 'new-task (project-tasks project2)))))))))

(test save-session-preserves-changes
  "Saved session preserves modifications"
  (with-temp-project-file (file)
    (let* ((*current-session* (load-project-session file))
           (project (session-project *current-session*))
           (task (gethash 't1 (project-tasks project))))
      (modify-task-in-session task :priority 900)
      (save-session *current-session* file)

      ;; Reload and verify
      (let* ((session2 (load-project-session file))
             (project2 (session-project session2))
             (task2 (gethash 't1 (project-tasks project2))))
        (is (= 900 (task-priority task2)))))))

(test save-session-with-deleted-tasks
  "Saved session excludes deleted tasks"
  (with-temp-project-file (file)
    (let* ((*current-session* (load-project-session file))
           (project (session-project *current-session*))
           (task (gethash 't2 (project-tasks project))))
      (delete-task-from-session task)
      (save-session *current-session* file)

      ;; Reload and verify
      (let* ((session2 (load-project-session file))
             (project2 (session-project session2)))
        (is (null (gethash 't2 (project-tasks project2))))))))

;;; ============================================================================
;;; Session State Tests
;;; ============================================================================

(test session-current-task
  "Session tracks current task"
  (with-test-session
    (let* ((project (session-project *current-session*))
           (task (gethash 't1 (project-tasks project))))
      (setf (session-current-task *current-session*) task)
      (is (eq task (session-current-task *current-session*))))))

(test session-reset-changes
  "Can reset session changes"
  (with-test-session
    (add-task-to-session 'task1 "Task 1" :duration (duration 1 :day))
    (add-task-to-session 'task2 "Task 2" :duration (duration 2 :days))
    (is (> (length (session-changes *current-session*)) 0))
    (reset-session-changes *current-session*)
    (is (= 0 (length (session-changes *current-session*))))))
