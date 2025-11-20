;;;; src/reporting/formats/csv.lisp
;;;; CSV report formatting

(in-package #:project-juggler)

;;; ============================================================================
;;; CSV Task Reports
;;; ============================================================================

(defun generate-csv-task-report (report tasks)
  "Generate CSV output for task report"
  (with-output-to-string (s)
    ;; Header row
    (format s "~{~A~^,~}~%"
            (mapcar (lambda (col) (string-capitalize (symbol-name col)))
                   (report-columns report)))

    ;; Data rows
    (dolist (task tasks)
      (format s "~{~A~^,~}~%"
              (mapcar (lambda (col)
                       (csv-escape (format-cell col (get-task-column-value task col))))
                     (report-columns report))))))

;;; ============================================================================
;;; CSV Resource Reports
;;; ============================================================================

(defun generate-csv-resource-report (report resources)
  "Generate CSV output for resource report"
  (with-output-to-string (s)
    ;; Header row
    (format s "~{~A~^,~}~%"
            (mapcar (lambda (col) (string-capitalize (symbol-name col)))
                   (report-columns report)))

    ;; Data rows
    (dolist (resource resources)
      (format s "~{~A~^,~}~%"
              (mapcar (lambda (col)
                       (csv-escape (format-cell col (get-resource-column-value resource col))))
                     (report-columns report))))))

;;; ============================================================================
;;; CSV Utilities
;;; ============================================================================

(defun csv-escape (value)
  "Escape CSV special characters"
  (let ((str (if (stringp value) value (format nil "~A" value))))
    (if (or (find #\, str)
            (find #\" str)
            (find #\Newline str))
        ;; Needs quoting
        (format nil "\"~A\"" (cl-ppcre:regex-replace-all "\"" str "\"\""))
        str)))
