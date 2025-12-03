;;;; src/reporting/gantt.lisp
;;;; Gantt chart data generation and export

(in-package #:project-juggler)

;;; ============================================================================
;;; Gantt Chart Data Generation
;;; ============================================================================

(defun generate-gantt-data (project)
  "Generate Gantt chart data structure for project.

   Returns a list of plists, each representing a task:
   (:id :name :start :end :dependencies)"

  (let ((tasks (loop for task being the hash-values of (project-tasks project)
                    collect task)))
    ;; Sort tasks by start date
    (setf tasks (sort tasks #'date< :key #'task-start))

    ;; Build data structure
    (mapcar #'task-to-gantt-entry tasks)))

(defun task-to-gantt-entry (task)
  "Convert a task to a Gantt chart entry (plist)"
  (list :id (task-id task)
        :name (task-name task)
        :start (task-start task)
        :end (task-end task)
        :dependencies (get-dependency-ids task)))

(defun get-dependency-ids (task)
  "Get list of task IDs that this task depends on"
  (mapcar (lambda (dep)
           (task-id (dependency-target dep)))
         (task-dependencies task)))

;;; ============================================================================
;;; Extended Gantt Entry with Full Details
;;; ============================================================================

(defun task-to-extended-gantt-entry (task project)
  "Convert a task to an extended Gantt chart entry with all details."
  (let* ((is-critical (and (task-slack task)
                           (zerop (task-slack task))))
         (resources (get-task-allocated-resources task project)))
    (list :id (task-id task)
          :name (task-name task)
          :start (format-date-iso (task-start task))
          :end (format-date-iso (task-end task))
          :duration (if (task-duration task)
                       (duration-in-days (task-duration task))
                       0)
          :progress (or (task-complete task) 0)
          :milestone (if (task-milestone-p task) t nil)
          :critical is-critical
          :dependencies (get-dependency-ids task)
          :resources resources)))

(defun get-task-allocated-resources (task project)
  "Get list of resource names allocated to a task."
  (let ((allocations (task-allocations task)))
    (when allocations
      (loop for alloc in allocations
            append (loop for ref in (allocation-resource-refs alloc)
                        for resource = (gethash ref (project-resources project))
                        when resource collect (resource-name resource))))))

(defun format-date-iso (date)
  "Format a date as ISO 8601 string (YYYY-MM-DD)."
  (if date
      (format nil "~4,'0D-~2,'0D-~2,'0D"
              (date-year date)
              (date-month date)
              (date-day date))
      nil))

;;; ============================================================================
;;; JSON Export
;;; ============================================================================

(defun generate-gantt-json (project)
  "Generate Gantt chart data as JSON string.

   Returns a JSON array of task objects with:
   - id, name, start, end, duration
   - progress (0-100)
   - milestone (true/false)
   - critical (true/false)
   - dependencies (array of task IDs)
   - resources (array of resource names)"
  (let ((tasks (loop for task being the hash-values of (project-tasks project)
                    collect task)))
    ;; Sort by start date
    (setf tasks (sort tasks #'date< :key #'task-start))

    ;; Generate JSON
    (with-output-to-string (s)
      (format s "[~%")
      (loop for task in tasks
            for i from 0
            for entry = (task-to-extended-gantt-entry task project)
            do (when (> i 0) (format s ",~%"))
               (format s "  ~A" (gantt-entry-to-json entry)))
      (format s "~%]"))))

(defun gantt-entry-to-json (entry)
  "Convert a Gantt entry plist to JSON object string."
  (with-output-to-string (s)
    (format s "{")
    (format s "\"id\": \"~A\"" (getf entry :id))
    (format s ", \"name\": ~A" (json-escape-string (getf entry :name)))
    (format s ", \"start\": ~A" (json-value (getf entry :start)))
    (format s ", \"end\": ~A" (json-value (getf entry :end)))
    (format s ", \"duration\": ~A" (or (getf entry :duration) 0))
    (format s ", \"progress\": ~A" (or (getf entry :progress) 0))
    (format s ", \"milestone\": ~A" (if (getf entry :milestone) "true" "false"))
    (format s ", \"critical\": ~A" (if (getf entry :critical) "true" "false"))
    (format s ", \"dependencies\": [~{\"~A\"~^, ~}]" (getf entry :dependencies))
    (format s ", \"resources\": [~{~A~^, ~}]"
            (mapcar #'json-escape-string (or (getf entry :resources) nil)))
    (format s "}")))

(defun json-escape-string (str)
  "Escape a string for JSON."
  (if str
      (format nil "\"~A\""
              (cl-ppcre:regex-replace-all "\\\\" (format nil "~A" str) "\\\\\\\\")
              ;; Note: simplified escaping - proper impl would escape more chars
              )
      "null"))

(defun json-value (val)
  "Convert a value to JSON representation."
  (cond
    ((null val) "null")
    ((stringp val) (json-escape-string val))
    ((numberp val) (format nil "~A" val))
    ((eq val t) "true")
    (t (json-escape-string (format nil "~A" val)))))

;;; ============================================================================
;;; SVG Export
;;; ============================================================================

(defparameter *gantt-bar-height* 25)
(defparameter *gantt-bar-spacing* 35)
(defparameter *gantt-label-width* 200)
(defparameter *gantt-day-width* 20)
(defparameter *gantt-header-height* 40)

(defun generate-gantt-svg (project &key (width 1000) (height nil))
  "Generate Gantt chart as SVG.

   Options:
   - width: SVG width in pixels (default 1000)
   - height: SVG height in pixels (auto-calculated if nil)"
  (let* ((tasks (loop for task being the hash-values of (project-tasks project)
                     collect task))
         (sorted-tasks (sort tasks #'date< :key #'task-start))
         (num-tasks (length sorted-tasks))
         (calculated-height (or height
                                (+ *gantt-header-height*
                                   (* num-tasks *gantt-bar-spacing*)
                                   20)))
         (project-start (project-start project))
         (project-end (project-end project))
         (total-days (max 1 (days-between project-start project-end)))
         (chart-width (- width *gantt-label-width* 20))
         (pixels-per-day (/ chart-width total-days)))

    (with-output-to-string (s)
      ;; SVG header
      (format s "<svg xmlns=\"http://www.w3.org/2000/svg\" ")
      (format s "width=\"~A\" height=\"~A\" " width calculated-height)
      (format s "viewBox=\"0 0 ~A ~A\">~%" width calculated-height)

      ;; Styles
      (format s "<style>~%")
      (format s "  .task-bar { fill: #4CAF50; }~%")
      (format s "  .critical-bar { fill: #f44336; }~%")
      (format s "  .milestone { fill: #9C27B0; }~%")
      (format s "  .task-label { font-family: Arial, sans-serif; font-size: 12px; }~%")
      (format s "  .header { font-family: Arial, sans-serif; font-size: 14px; font-weight: bold; }~%")
      (format s "  .dependency-line { stroke: #666; stroke-width: 1; fill: none; }~%")
      (format s "  .grid-line { stroke: #eee; stroke-width: 1; }~%")
      (format s "</style>~%")

      ;; Background and grid
      (format s "<rect width=\"~A\" height=\"~A\" fill=\"white\"/>~%" width calculated-height)

      ;; Header
      (format s "<text x=\"10\" y=\"25\" class=\"header\">Task</text>~%")
      (format s "<text x=\"~A\" y=\"25\" class=\"header\">Timeline</text>~%" (+ *gantt-label-width* 10))
      (format s "<line x1=\"0\" y1=\"~A\" x2=\"~A\" y2=\"~A\" stroke=\"#ccc\"/>~%"
              *gantt-header-height* width *gantt-header-height*)

      ;; Task bars
      (loop for task in sorted-tasks
            for i from 0
            for y = (+ *gantt-header-height* (* i *gantt-bar-spacing*) 10)
            for task-start = (task-start task)
            for task-end = (task-end task)
            for start-offset = (if task-start
                                  (days-between project-start task-start)
                                  0)
            for task-days = (if (and task-start task-end)
                               (max 1 (days-between task-start task-end))
                               1)
            for x = (+ *gantt-label-width* (* start-offset pixels-per-day))
            for bar-width = (max 5 (* task-days pixels-per-day))
            for is-critical = (and (task-slack task) (zerop (task-slack task)))
            for is-milestone = (task-milestone-p task)
            for bar-class = (cond (is-milestone "milestone")
                                  (is-critical "critical-bar")
                                  (t "task-bar"))
            do
            ;; Task label
            (format s "<text x=\"10\" y=\"~A\" class=\"task-label\">~A</text>~%"
                    (+ y 17) (truncate-string (task-name task) 25))

            ;; Task bar or milestone diamond
            (if is-milestone
                ;; Draw diamond for milestone
                (let ((cx (+ x 5))
                      (cy (+ y 12)))
                  (format s "<polygon points=\"~A,~A ~A,~A ~A,~A ~A,~A\" class=\"~A\"/>~%"
                          cx (- cy 8)
                          (+ cx 8) cy
                          cx (+ cy 8)
                          (- cx 8) cy
                          bar-class))
                ;; Draw bar for regular task
                (format s "<rect x=\"~A\" y=\"~A\" width=\"~A\" height=\"~A\" class=\"~A\" rx=\"3\"/>~%"
                        x y bar-width *gantt-bar-height* bar-class))

            ;; Progress bar overlay
            (let ((progress (or (task-complete task) 0)))
              (when (and (> progress 0) (not is-milestone))
                (format s "<rect x=\"~A\" y=\"~A\" width=\"~A\" height=\"~A\" fill=\"rgba(0,0,0,0.2)\" rx=\"3\"/>~%"
                        x y (* bar-width (/ progress 100.0)) *gantt-bar-height*))))

      ;; Dependency arrows
      (let ((task-positions (make-hash-table :test 'eq)))
        ;; Build position map
        (loop for task in sorted-tasks
              for i from 0
              do (setf (gethash (task-id task) task-positions) i))

        ;; Draw arrows
        (loop for task in sorted-tasks
              for target-idx = (gethash (task-id task) task-positions)
              do (dolist (dep (task-dependencies task))
                   (let* ((source-id (dependency-target-ref dep))
                          (source-idx (gethash source-id task-positions)))
                     (when source-idx
                       (let* ((source-task (find source-id sorted-tasks :key #'task-id))
                              (source-end (task-end source-task))
                              (target-start (task-start task))
                              (source-x (+ *gantt-label-width*
                                          (* (days-between project-start source-end) pixels-per-day)))
                              (source-y (+ *gantt-header-height*
                                          (* source-idx *gantt-bar-spacing*)
                                          10
                                          (/ *gantt-bar-height* 2)))
                              (target-x (+ *gantt-label-width*
                                          (* (days-between project-start target-start) pixels-per-day)))
                              (target-y (+ *gantt-header-height*
                                          (* target-idx *gantt-bar-spacing*)
                                          10
                                          (/ *gantt-bar-height* 2))))
                         (format s "<path d=\"M~A,~A L~A,~A L~A,~A\" class=\"dependency-line\" marker-end=\"url(#arrow)\"/>~%"
                                 source-x source-y
                                 (+ source-x 10) source-y
                                 target-x target-y)))))))

      ;; Arrow marker definition
      (format s "<defs>~%")
      (format s "  <marker id=\"arrow\" markerWidth=\"10\" markerHeight=\"10\" refX=\"9\" refY=\"3\" orient=\"auto\">~%")
      (format s "    <path d=\"M0,0 L0,6 L9,3 z\" fill=\"#666\"/>~%")
      (format s "  </marker>~%")
      (format s "</defs>~%")

      (format s "</svg>"))))

(defun days-between (start-date end-date)
  "Calculate days between two dates."
  (if (and start-date end-date)
      (let ((start-ts (local-time:timestamp-to-unix (date-timestamp start-date)))
            (end-ts (local-time:timestamp-to-unix (date-timestamp end-date))))
        (max 0 (round (/ (- end-ts start-ts) 86400))))
      0))

(defun truncate-string (str max-len)
  "Truncate a string to max-len characters, adding ellipsis if needed."
  (if (> (length str) max-len)
      (concatenate 'string (subseq str 0 (- max-len 3)) "...")
      str))

;;; ============================================================================
;;; HTML with Embedded SVG
;;; ============================================================================

(defun generate-gantt-html (project &key (width 1000) (height nil) (title "Gantt Chart"))
  "Generate HTML page with embedded Gantt SVG."
  (let ((svg (generate-gantt-svg project :width width :height height)))
    (with-output-to-string (s)
      (format s "<!DOCTYPE html>~%")
      (format s "<html>~%")
      (format s "<head>~%")
      (format s "  <title>~A</title>~%" title)
      (format s "  <style>~%")
      (format s "    body { font-family: Arial, sans-serif; margin: 20px; }~%")
      (format s "    h1 { color: #333; }~%")
      (format s "    .gantt-container { overflow-x: auto; }~%")
      (format s "  </style>~%")
      (format s "</head>~%")
      (format s "<body>~%")
      (format s "<h1>~A</h1>~%" title)
      (format s "<div class=\"gantt-container\">~%")
      (format s "~A~%" svg)
      (format s "</div>~%")
      (format s "</body>~%")
      (format s "</html>"))))
