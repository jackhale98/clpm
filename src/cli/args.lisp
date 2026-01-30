;;;; src/cli/args.lisp
;;;; Command-line argument parsing for CLAPS CLI

(in-package #:claps/cli)

(defvar *claps-version* "1.0.0"
  "CLAPS version string")

(define-condition cli-argument-error (error)
  ((message :initarg :message :reader cli-argument-error-message))
  (:documentation "Error during argument parsing")
  (:report (lambda (condition stream)
             (format stream "~A" (cli-argument-error-message condition)))))

(defclass cli-options ()
  ((file :initarg :file :initform nil :accessor option-file
         :documentation "Project file path")
   (output-dir :initarg :output-dir :initform nil :accessor option-output-dir
               :documentation "Output directory for reports")
   (report :initarg :report :initform nil :accessor option-report
           :documentation "Specific report ID to generate")
   (validate :initarg :validate :initform nil :accessor option-validate
             :documentation "Validate without scheduling")
   (critical-path :initarg :critical-path :initform nil :accessor option-critical-path
                  :documentation "Show critical path tasks")
   (scenarios :initarg :scenarios :initform nil :accessor option-scenarios
              :documentation "List available scenarios")
   (compare :initarg :compare :initform nil :accessor option-compare
            :documentation "Pair of scenarios to compare")
   (summary :initarg :summary :initform nil :accessor option-summary
            :documentation "Show project summary")
   (resources :initarg :resources :initform nil :accessor option-resources
              :documentation "Show resource utilization")
   (overallocations :initarg :overallocations :initform nil :accessor option-overallocations
                    :documentation "Show resource overallocations")
   (evm :initarg :evm :initform nil :accessor option-evm
        :documentation "Show earned value metrics")
   (status-date :initarg :status-date :initform nil :accessor option-status-date
                :documentation "Status date for EVM calculations")
   (simulate :initarg :simulate :initform nil :accessor option-simulate
             :documentation "Run Monte Carlo simulation")
   (trials :initarg :trials :initform 1000 :accessor option-trials
           :documentation "Number of simulation trials")
   (milestones :initarg :milestones :initform nil :accessor option-milestones
               :documentation "Show milestone timeline")
   (quiet :initarg :quiet :initform nil :accessor option-quiet
          :documentation "Suppress informational output")
   (json :initarg :json :initform nil :accessor option-json
         :documentation "Output in JSON format")
   (help :initarg :help :initform nil :accessor option-help
         :documentation "Show help message")
   (version :initarg :version :initform nil :accessor option-version
            :documentation "Show version")
   (repl :initarg :repl :initform nil :accessor option-repl
         :documentation "Start interactive REPL after loading"))
  (:documentation "Parsed command-line options"))

(defun parse-date-string (date-str)
  "Parse a date string in YYYY-MM-DD format"
  (let ((parts (split-sequence:split-sequence #\- date-str)))
    (when (= (length parts) 3)
      (let ((year (parse-integer (first parts) :junk-allowed t))
            (month (parse-integer (second parts) :junk-allowed t))
            (day (parse-integer (third parts) :junk-allowed t)))
        (when (and year month day)
          (claps:date year month day))))))

(defun parse-arguments (args)
  "Parse command-line arguments into a cli-options object.
   ARGS should be a list of strings."
  (let ((opts (make-instance 'cli-options))
        (positional nil)
        (i 0))
    (loop while (< i (length args))
          for arg = (nth i args)
          do (cond
               ;; Help and version
               ((or (string= arg "--help") (string= arg "-h"))
                (setf (option-help opts) t))
               ((or (string= arg "--version") (string= arg "-v"))
                (setf (option-version opts) t))

               ;; Analysis commands
               ((string= arg "--validate")
                (setf (option-validate opts) t))
               ((string= arg "--critical-path")
                (setf (option-critical-path opts) t))
               ((string= arg "--scenarios")
                (setf (option-scenarios opts) t))
               ((string= arg "--summary")
                (setf (option-summary opts) t))
               ((string= arg "--resources")
                (setf (option-resources opts) t))
               ((string= arg "--overallocations")
                (setf (option-overallocations opts) t))
               ((string= arg "--evm")
                (setf (option-evm opts) t))
               ((string= arg "--simulate")
                (setf (option-simulate opts) t))
               ((string= arg "--milestones")
                (setf (option-milestones opts) t))
               ((string= arg "--repl")
                (setf (option-repl opts) t))

               ;; Output options
               ((string= arg "--quiet")
                (setf (option-quiet opts) t))
               ((string= arg "--json")
                (setf (option-json opts) t))

               ;; Options with values
               ((string= arg "--output-dir")
                (incf i)
                (if (< i (length args))
                    (setf (option-output-dir opts) (nth i args))
                    (error 'cli-argument-error
                           :message "--output-dir requires a directory path")))

               ((string= arg "--report")
                (incf i)
                (if (< i (length args))
                    (setf (option-report opts) (intern (string-upcase (nth i args))))
                    (error 'cli-argument-error
                           :message "--report requires a report ID")))

               ((string= arg "--status-date")
                (incf i)
                (if (< i (length args))
                    (let ((date (parse-date-string (nth i args))))
                      (if date
                          (setf (option-status-date opts) date)
                          (error 'cli-argument-error
                                 :message "Invalid date format. Use YYYY-MM-DD")))
                    (error 'cli-argument-error
                           :message "--status-date requires a date (YYYY-MM-DD)")))

               ((string= arg "--trials")
                (incf i)
                (if (< i (length args))
                    (let ((n (parse-integer (nth i args) :junk-allowed t)))
                      (if (and n (plusp n))
                          (setf (option-trials opts) n)
                          (error 'cli-argument-error
                                 :message "--trials requires a positive integer")))
                    (error 'cli-argument-error
                           :message "--trials requires a number")))

               ((string= arg "--compare")
                (incf i)
                (if (< (+ i 1) (length args))
                    (let ((s1 (intern (string-upcase (nth i args))))
                          (s2 (intern (string-upcase (nth (1+ i) args)))))
                      (setf (option-compare opts) (list s1 s2))
                      (incf i))
                    (error 'cli-argument-error
                           :message "--compare requires two scenario names")))

               ;; Unknown option
               ((and (> (length arg) 0) (char= (char arg 0) #\-))
                (error 'cli-argument-error
                       :message (format nil "Unknown option: ~A" arg)))

               ;; Positional argument (file)
               (t
                (push arg positional)))
          (incf i))

    ;; Set file from positional args
    (when positional
      (setf (option-file opts) (first (reverse positional))))

    opts))

(defun print-usage ()
  "Print usage information"
  (format t "CLAPS - Common Lisp Automated Project Scheduling~%")
  (format t "Version ~A~%~%" *claps-version*)
  (format t "Usage: claps [OPTIONS] <project.lisp>~%~%")
  (format t "Options:~%")
  (format t "  <file.lisp>             Project file to process~%")
  (format t "  --output-dir DIR        Output directory for reports~%")
  (format t "  --report ID             Generate specific report only~%")
  (format t "  --validate              Validate without scheduling~%")
  (format t "  --critical-path         Print critical path tasks~%")
  (format t "  --scenarios             List available scenarios~%")
  (format t "  --compare S1 S2         Compare two scenarios~%")
  (format t "  --summary               Quick project overview~%")
  (format t "  --resources             Show resource utilization~%")
  (format t "  --overallocations       Show resource conflicts~%")
  (format t "  --evm                   Show earned value metrics~%")
  (format t "  --status-date DATE      Status date for EVM (YYYY-MM-DD)~%")
  (format t "  --simulate              Run Monte Carlo simulation~%")
  (format t "  --trials N              Number of simulation trials (default: 1000)~%")
  (format t "  --milestones            Show milestone timeline~%")
  (format t "  --repl                  Start interactive REPL after loading~%")
  (format t "  --quiet                 Suppress informational output~%")
  (format t "  --json                  Output in JSON format~%")
  (format t "  -h, --help              Show this help message~%")
  (format t "  -v, --version           Show version~%~%")
  (format t "Examples:~%")
  (format t "  claps project.lisp                     # Process and generate reports~%")
  (format t "  claps project.lisp --summary           # Quick project overview~%")
  (format t "  claps project.lisp --critical-path     # Show critical path~%")
  (format t "  claps project.lisp --resources         # Resource utilization~%")
  (format t "  claps project.lisp --evm               # Earned value metrics~%")
  (format t "  claps project.lisp --simulate          # Monte Carlo simulation~%"))

(defun print-version ()
  "Print version information"
  (format t "CLAPS ~A~%" *claps-version*))
