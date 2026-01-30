;;;; tests/cli/test-output.lisp
;;;; Tests for CLI output formatting

(in-package #:claps-tests)

(in-suite output-suite)

;;; ============================================================================
;;; JSON String Escaping Tests
;;; ============================================================================

(test to-json-string-null
  "null converts to JSON null"
  (is (equal "null" (claps/cli:to-json-string nil))))

(test to-json-string-true
  "t converts to JSON true"
  (is (equal "true" (claps/cli:to-json-string t))))

(test to-json-string-integer
  "Integer converts properly"
  (is (equal "42" (claps/cli:to-json-string 42)))
  (is (equal "-5" (claps/cli:to-json-string -5))))

(test to-json-string-float
  "Float converts properly"
  (let ((result (claps/cli:to-json-string 3.14)))
    (is (search "3.14" result))))

(test to-json-string-simple-string
  "Simple string converts properly"
  (is (equal "\"hello\"" (claps/cli:to-json-string "hello"))))

(test to-json-string-escapes-quotes
  "Quotes in strings are escaped"
  (is (equal "\"hello \\\"world\\\"\"" (claps/cli:to-json-string "hello \"world\""))))

(test to-json-string-escapes-backslash
  "Backslashes are escaped"
  (is (equal "\"path\\\\to\\\\file\"" (claps/cli:to-json-string "path\\to\\file"))))

(test to-json-string-escapes-newlines
  "Newlines are escaped"
  (is (equal "\"line1\\nline2\"" (claps/cli:to-json-string "line1
line2"))))

(test to-json-string-symbol
  "Symbol converts to string"
  (is (equal "\"TASK-NAME\"" (claps/cli:to-json-string 'task-name))))

;;; ============================================================================
;;; JSON Array Tests
;;; ============================================================================

(test to-json-null
  "nil converts to null"
  (is (equal "null" (claps/cli:to-json-string nil))))

(test to-json-array-integers
  "List of integers converts to array"
  (is (equal "[1,2,3]" (claps/cli:to-json-string '(1 2 3)))))

(test to-json-array-strings
  "List of strings converts to array"
  (is (equal "[\"a\",\"b\",\"c\"]" (claps/cli:to-json-string '("a" "b" "c")))))

;;; ============================================================================
;;; JSON Object Tests
;;; ============================================================================

(test to-json-plist-to-object
  "Plist converts to object"
  (is (equal "{\"a\":1,\"b\":2}" (claps/cli:to-json-string '(:a 1 :b 2)))))

(test to-json-nested-plist
  "Nested plist converts to nested object"
  (let ((result (claps/cli:to-json-string '(:outer (:inner 42)))))
    (is (search "\"outer\"" result))
    (is (search "\"inner\"" result))
    (is (search "42" result))))

(test to-json-plist-with-list-value
  "Plist with list value"
  (let ((result (claps/cli:to-json-string '(:items (1 2 3)))))
    (is (search "\"items\"" result))
    (is (search "[1,2,3]" result))))

;;; ============================================================================
;;; Error Formatting Tests
;;; ============================================================================

(test format-reference-error
  "Reference error formats properly"
  (let ((msg (claps/cli:format-lisp-error
              (make-condition 'claps:reference-error
                              :reference 'unknown-task
                              :message "Task not found"))))
    (is (search "unknown-task" (string-downcase msg)))))

(test format-circular-dependency-error
  "Circular dependency error formats properly"
  (let ((msg (claps/cli:format-lisp-error
              (make-condition 'claps:circular-dependency-error
                              :cycles '("A -> B -> A")
                              :message "Cycle detected"))))
    (is (search "Circular" msg))))

(test format-validation-error
  "Validation error formats properly"
  (let ((msg (claps/cli:format-lisp-error
              (make-condition 'claps:validation-error
                              :message "Invalid task configuration"))))
    (is (search "Validation" msg))))

(test format-file-error
  "File error formats properly"
  (let ((msg (claps/cli:format-lisp-error
              (make-condition 'file-error
                              :pathname #P"/nonexistent/file.lisp"))))
    (is (search "File not found" msg))))

;;; ============================================================================
;;; Date Formatting in JSON Tests
;;; ============================================================================

(test to-json-date
  "Date converts to ISO string"
  (let ((d (claps:date 2024 3 15)))
    (let ((result (claps/cli:to-json-string d)))
      (is (search "2024" result))
      (is (search "03" result))
      (is (search "15" result)))))

(test to-json-duration
  "Duration converts to days number"
  (let ((d (claps:duration 5 :days)))
    (is (equal "5" (claps/cli:to-json-string d)))))
