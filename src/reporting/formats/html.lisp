;;;; src/reporting/formats/html.lisp
;;;; HTML report formatting

(in-package #:project-juggler)

;;; ============================================================================
;;; HTML Task Reports
;;; ============================================================================

(defun generate-html-task-report (report tasks)
  "Generate HTML output for task report"
  (with-output-to-string (s)
    (format s "<!DOCTYPE html>~%")
    (format s "<html>~%")
    (format s "<head>~%")
    (format s "  <title>~A</title>~%" (report-title report))
    (format s "  <style>~%")
    (format s "    table { border-collapse: collapse; width: 100%%; }~%")
    (format s "    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }~%")
    (format s "    th { background-color: #4CAF50; color: white; }~%")
    (format s "    tr:nth-child(even) { background-color: #f2f2f2; }~%")
    (format s "  </style>~%")
    (format s "</head>~%")
    (format s "<body>~%")
    (format s "<h1>~A</h1>~%" (report-title report))
    (format s "<table>~%")

    ;; Header row
    (format s "  <tr>~%")
    (dolist (col (report-columns report))
      (format s "    <th>~A</th>~%" (string-capitalize (symbol-name col))))
    (format s "  </tr>~%")

    ;; Data rows
    (dolist (task tasks)
      (format s "  <tr>~%")
      (dolist (col (report-columns report))
        (let ((value (get-task-column-value task col)))
          (format s "    <td>~A</td>~%" (html-escape (format-cell col value)))))
      (format s "  </tr>~%"))

    (format s "</table>~%")
    (format s "</body>~%")
    (format s "</html>~%")))

(defun get-task-column-value (task column)
  "Get column value from task"
  (case column
    (:id (task-id task))
    (:name (task-name task))
    (:start (task-start task))
    (:end (task-end task))
    (:duration (task-duration task))
    (:effort (task-effort task))
    (:priority (task-priority task))
    (:milestone (task-milestone-p task))
    (:complete (task-complete task))
    (:scheduled (task-scheduled-p task))
    (:criticalness (task-criticalness task))
    (:path-criticalness (task-path-criticalness task))
    (:early-start (task-early-start task))
    (:early-finish (task-early-finish task))
    (:late-start (task-late-start task))
    (:late-finish (task-late-finish task))
    (:slack (task-slack task))
    (t nil)))

;;; ============================================================================
;;; HTML Resource Reports
;;; ============================================================================

(defun generate-html-resource-report (report resources)
  "Generate HTML output for resource report"
  (with-output-to-string (s)
    (format s "<!DOCTYPE html>~%")
    (format s "<html>~%")
    (format s "<head>~%")
    (format s "  <title>~A</title>~%" (report-title report))
    (format s "  <style>~%")
    (format s "    table { border-collapse: collapse; width: 100%%; }~%")
    (format s "    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }~%")
    (format s "    th { background-color: #4CAF50; color: white; }~%")
    (format s "    tr:nth-child(even) { background-color: #f2f2f2; }~%")
    (format s "  </style>~%")
    (format s "</head>~%")
    (format s "<body>~%")
    (format s "<h1>~A</h1>~%" (report-title report))
    (format s "<table>~%")

    ;; Header row
    (format s "  <tr>~%")
    (dolist (col (report-columns report))
      (format s "    <th>~A</th>~%" (string-capitalize (symbol-name col))))
    (format s "  </tr>~%")

    ;; Data rows
    (dolist (resource resources)
      (format s "  <tr>~%")
      (dolist (col (report-columns report))
        (let ((value (get-resource-column-value resource col)))
          (format s "    <td>~A</td>~%" (html-escape (format-cell col value)))))
      (format s "  </tr>~%"))

    (format s "</table>~%")
    (format s "</body>~%")
    (format s "</html>~%")))

(defun get-resource-column-value (resource column)
  "Get column value from resource"
  (case column
    (:id (resource-id resource))
    (:name (resource-name resource))
    (:efficiency (resource-efficiency resource))
    (:rate (resource-rate resource))
    (:criticalness (resource-criticalness resource))
    (:allocated-effort (resource-allocated-effort resource))
    (:available-effort (resource-available-effort resource))
    (t nil)))

;;; ============================================================================
;;; HTML Utilities
;;; ============================================================================

(defun html-escape (string)
  "Escape HTML special characters"
  (let ((str (if (stringp string) string (format nil "~A" string))))
    (setf str (cl-ppcre:regex-replace-all "&" str "&amp;"))
    (setf str (cl-ppcre:regex-replace-all "<" str "&lt;"))
    (setf str (cl-ppcre:regex-replace-all ">" str "&gt;"))
    (setf str (cl-ppcre:regex-replace-all "\"" str "&quot;"))
    str))
